{
module Parser ( parseProgram
              , parseTokens
              , Expr (..)
              ) where

import Prelude hiding (Ordering(..))

import Lexer
import Ast
import Control.Monad.Except

}

-- Entry
%name program

-- lexer structure
%tokentype { Token }

-- parser monad
%monad { Except String } { (>>=) } { return }
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
  'main'  { main }
  'exit'  { EXIT }
  'print' { PRINT }
  '\n'    { EOL }

%nonassoc '==' '!='
%left 'and' 'or'
%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'

%%

Program : Decls          { $1 }

Decls : Decl            { [$1] }
      | Decl '\n' Decls { $1:$3 }

Decl : VarAss           { Var (vname $1) (vexp $1) }
     | FunDec           { $1 }
     | MainDec          { $1 }

VarAss : 'def' Var ':=' Expr           { VAss $2 (Just $4) }
VarAss : 'def' Var                     { VAss $2 Nothing }
FunDec : 'fn' Var Formals '\n' Stmts   { Fun $2 $3 $5 }
MainDec : 'main' '\n' Stmts            { Fun (sym "main") [] $3 }

Var : VAR { $1 }

Stmts : Stmt             { [$1] }
      | Stmt '\n' Stmts  { $1:$3 }

Stmt : 'exit'         { Exit }
     | VarAss         { Ass (vname $1) (vexp $1) }
     | Cond           { $1 }
     | For            { $1 }
     | While          { $1 }
     | 'print'        { Print [] }
     | 'print' Args   { Print $2 }
     | Call           { $1 }
     | 'return' Expr  { Ret $2 }

Literal : NUM     { LInt $1 }
        | STR     { LStr $1 }
        | 'true'  { LBoo True }
        | 'false' { LBoo False }

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

LBdy : Stmts '\n' 'endfor'     { $1 }

Cond : 'if' Expr '\n' Stmts '\n' 'endif' { Cond $2 $4 Nothing }
     | 'if' Expr '\n' Stmts '\n' 'else' Stmts '\n' 'endif' { Cond $2 $4 (Just $7)}

Call : 'call' Var '(' Args ')' { Call $2 $4 }

Args : Expr          { [$1] }
     | Expr ',' Args { $1:$3 }

Formals : '(' ')'      { [] }
        | '(' Pars ')' { $2 }
Pars : Var             { [$1] }
     | Var ',' Pars    { $1:$3}


{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError $ "Parser error: " <> show ls
parseError [] = throwError "Unexpected end of Input"

parseProgram :: String -> Either String [Decl]
parseProgram input = runExcept $ do
  tokenStream <- scanTokens input
  program tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
}
