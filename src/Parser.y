{
module Parser ( parseExpr
              , parseTokens
              , Expr (..)
              ) where

import Lexer
import Control.Monad.Except

}

-- Entry
%name expr

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
  OP   { OP $$ }
  '('  { LPAR }
  ')'  { RPAR }
  ','  { COM }
  '<'  { LT }
  '<'  { GT }
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
  'def'   { DEF }
  'fn'    { FN }
  'return'{ RET }
  'call'  { CALL }
  'main'  { main }
  'exit'  { EXIT }
  '\n'    { EOL }

%%

Program : Decls          { App $1 $2 }

Decls : VarAss           { $1 }
      | FunDec           { $1 }
      | MainDec          { $1 }

FunDec : 'fn' Var Formals '\n' Stmts   { Fun $2 $3 $5 }
VarAss : 'def' Var ':=' Expr           { Ass $2 $4 }
MainDec : 'main' '\n' Stmts            { Fun (sym "main") [] $3 }

Stmts : Stmt             { }
      | Stmt '\n' Stmts  { }

Stmt : 'exit'         { Exit }
     | VarDec         { $1 }
     | Cond           { $1 }
     | For            { $1 }
     | While          { $1 }
     | 'return' Expr  { Ret $2 }

Expr : Literal
     | Infix        { $2 }
     | '(' Expr ')' { $2 }

Infix : Expr OP Expr { Infix $2 $1 $3 }

While : 'for' BExpr '\n' LBdy  { }
For   : 'for' Var 'from' Expr 'to' Expr '\n' LBdy            {}
      | 'for' Var 'from' Expr 'to' Expr 'by' Expr '\n' LBdy  {}

LBdy : Stmts '\n' 'endfor'     { $1 }

Cond : 'if' BExpr '\n' Stmts '\n' 'endif' { }
     | 'if' BExpr '\n' Stmts '\n' 'else' Stmts '\n' 'endif' { }

Args : Expr          { [$1] }
     | Expr ',' Args { $1:$3 }

Formals : '(' ')'      { [] }
        | '(' Pars ')' { $2 }
Pars : Var             { [$1] }
     | Var ',' Pars    { $1:$3}

Call : 'call' Var '(' Args ')' { }

{
data Expr = App Expr Expr
        | Lam String Expr
        | Var String
        | Lit Int
        deriving (Eq, Show)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError $ "Parser error: " <> show ls
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Expr
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
}
