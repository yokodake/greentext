{
module Lexer
  ( Token(..)
  , scanTokens
  ) where

-- import AST
import Control.Monad.Except

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
  -- Whitespace insenstive
  $eol ;
  $white+ ;

  -- Comments
  "#".* ;

  -- Syntax
  $digit+ { \s -> TokInt (read s) }
  "."     { \_ -> TokDot }
  \\      { \_ -> TokLam }
  \(      { \_ -> TokLParen }
  \)      { \_ -> TokRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokSym s }



{

data Token = TokInt Int
           | TokSym String
           | TokDot
           | TokLam
           | TokLParen
           | TokRParen
           | TokEOF
           deriving (Eq, Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n', [], str)
  where
    go :: AlexInput -> Except String [Token]
    go inp@(_,bs,str) =
      case alexScan inp 0 of
        AlexEOF -> return []
        AlexError (_, _, str) -> throwError $ "Invalid lexeme: " <> str
        AlexSkip inp' len -> go inp'
        AlexToken inp' len act -> do
          res <- go inp'
          let rest = act (take len str)
          return (rest:res)
}
