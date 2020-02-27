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
  NUM  { TokInt $$ }
  VAR  { TokSym $$ }
  '\\' { TokLam }
  '.'  { TokDot }
  '('  { TokLParen }
  ')'  { TokRParen }

%%

Expr : Atom Expr          { App $1 $2 }
     | '\\' VAR '.' Expr  { Lam $2 $4 }
     | Atom               { $1 }

Atom : '(' Expr ')' { $2 }
     | NUM          { Lit $1 }
     | VAR          { Var $1 }

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
