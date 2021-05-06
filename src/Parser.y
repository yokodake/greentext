{
module Parser ( parseProgram
              , parseReplLine
              , parseTokens
              , Expr (..)
              , PError (..)
              ) where

import Prelude hiding (Ordering(..))

import Lexer
import Ast
import Control.Monad.Except

}

-- Entry
%name program program
%name repl    Repl

-- lexer structure
%tokentype { Token }

-- parser monad
%monad { Except PError } { (>>=) } { return }
%error { parseError }

-- token names
%token
  NUM  { INT $$ }
  VAR  { SYM $$ }
  STR  { STR $$ }
  '('  { LPAR }
  ')'  { RPAR }
  ','  { COM }
  '+'  { PLUS }
  '-'  { MINUS }
  '*'  { MULT }
  '/'  { DIV }
  '%'  { MOD }
  '<'  { LT }
  '>'  { GT }
  '<=' { LTE }
  '>=' { GTE }
  '==' { EQ }
  '!=' { NEQ }
  ':=' { ASS }
  'and'  { AND }
  'or'  { OR }
  'true' { TRUE }
  'false' { FALSE }
  'if'     { IF }
  'else'   { ELSE }
  'endif'  { ENDIF }
  'for'    { FOR }
  'from'   { FROM }
  'to'     { TO }
  'by'     { BY }
  'endfor' { ENDFOR }
  'def'   { DECL }
  'fn'    { FN }
  'return'{ RET }
  'rval'  { RVAL }
  'call'  { CALL }
  'main'  { MAIN }
  'exit'  { EXIT }
  'print' { PRINT }
  '\n'    { EOL }

%nonassoc '==' '!='
%left 'and' 'or'
%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'

%%

program : '\n' Decls { $2 }
        | Decls { $1 } 


-- TODO: when we'll includes/modules, empty modules should be allowed
Decls : Decl            { [$1] }
      -- | '\n' Decl Decls { $2:$3 }

{-
Decls : Decl            { [$1] }
      | Decl '\n' Decls { $1:$3 }
-}

Decl : VarAss           { Var (vname $1) (vexp $1) }
     | FunDec           { $1 }
     | MainDec          { $1 }

VarAss : 'def' Var ':=' Expr           { VAss $2 (Just $4) }
VarAss : 'def' Var                     { VAss $2 Nothing }
FunDec : 'fn' Var Formals '\n' Stmts   { Fun $2 $3 $5 }
MainDec : 'main' '\n' Stmts            { Fun (sym "main") [] $3 }

Var : VAR { $1 }
{-- current sr-conflict: 
        Stmts -> Stmt . '\n'    shift, and enter state 55
                                 (reduce using rule 14)
        Stmts -> Stmt . %eof    reduce using rule 14

     This is okay because we'll prioritize shifting, and that's what we want.
Stmts : Stmt '\n' Stmts { $1:$3 }
      | Stmt            { [$1] }
-}

Stmts : Stmt '\n' Stmts { $1:$3 }
      |                 { [] }

-- TODO: allow empty Stmts. which will create more conflicts though,
--       mainly due to the ambiguity between assignment in a Stmt and a Decl in the top-level
--       either we should consider indentation (at least for function bodies) different keywords for 
--       toplevel declaration and assignment/declaration inside a stmt.

Stmt : 'exit'           { Exit }
     | VarAss           { Ass (vname $1) (vexp $1) }
     | Cond             { $1 }
     | For              { $1 }
     | While            { $1 }
     | 'print'          { Print [] }
     | 'print' Args     { Print $2 }
     | Call             { $1 }
     | Return           { $1 }

Return : 'return'       { Ret Nothing }
       | 'return' Expr  { Ret (Just $2) }

Literal : NUM       { LInt $1 }
        | STR       { LStr $1 }
        | 'true'    { LBoo True }
        | 'false'   { LBoo False }

Expr : Literal      { Lit $1 }
     | Infix        { $1 }
     | '(' Expr ')' { $2 }
     | Var          { Ref $1 }
     | 'rval'       { RVal }

Infix : Expr 'and' Expr { Infix (sym "and") $1 $3 }
      | Expr 'or' Expr  { Infix (sym "or") $1 $3 }
      | Expr '<' Expr   { Infix (sym "<") $1 $3 }
      | Expr '>' Expr   { Infix (sym ">") $1 $3 }
      | Expr '<=' Expr  { Infix (sym "<=") $1 $3 }
      | Expr '>=' Expr  { Infix (sym ">=") $1 $3 }
      | Expr '==' Expr  { Infix (sym "==") $1 $3 }
      | Expr '!=' Expr  { Infix (sym "!=") $1 $3 }
      | Expr '+' Expr   { Infix (sym "+") $1 $3 }
      | Expr '-' Expr   { Infix (sym "-") $1 $3 }
      | Expr '*' Expr   { Infix (sym "*") $1 $3 }
      | Expr '/' Expr   { Infix (sym "/") $1 $3 }
      | Expr '%' Expr   { Infix (sym "%") $1 $3 }

While : 'for' Expr '\n' LBdy  { While $2 $4 }
For   : 'for' Var 'from' Expr 'to' Expr '\n' LBdy            { For $2 $4 $6 Nothing $8 }
      | 'for' Var 'from' Expr 'to' Expr 'by' Expr '\n' LBdy  { For $2 $4 $6 (Just $8) $10 }

LBdy : Stmts 'endfor'     { $1 }

Cond : 'if' Expr '\n' Stmts 'endif' { Cond $2 $4 Nothing }
     | 'if' Expr '\n' Stmts 'else' Stmts 'endif' { Cond $2 $4 (Just $6)}

Call : 'call' Var '(' Args ')' { Call $2 $4 }
     | 'call' Var              { Call $2 [] }

Args : Expr          { [$1] }
     | Expr ',' Args { $1:$3 }

Formals :              { [] }
        | '(' Pars ')' { $2 }
Pars : Var             { [$1] }
     | Var ',' Pars    { $1:$3}

-- TODO multiple Repl lines (?)
Repl : Expr   { RExp $1 }
     | FunDec { RFun $1 }
     | Stmt   { RStm $1 }
     | Stmt '\n' { RStm $1 }

{
data PError = PError String
            | UnexpectedEOI
            deriving Show

parseError :: [Token] -> Except PError a
parseError (l:ls) = (throwError . PError) ("Parser error: " <> show (l:ls))
parseError [] = throwError UnexpectedEOI

parseProgram :: String -> Either PError [Decl]
parseProgram input = runExcept $ do
  tokenStream <- withExcept PError (scanTokens input)
  program tokenStream

parseReplLine :: String -> Either PError ReplAst
parseReplLine input = runExcept $ do 
  tokenStream <- withExcept PError (scanTokens input) 
  repl tokenStream

parseTokens :: String -> Either PError [Token]
parseTokens = runExcept . withExcept PError . scanTokens
}
