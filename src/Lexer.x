{
module Lexer
  ( Token(..)
  , scanTokens
  ) where

import Prelude hiding (Ordering(..))
-- import AST
import Control.Monad.Except
import Debug.Trace

}

%wrapper "basic"

$space = [\ \t\f\v]
$digit = 0-9
$alpha = [a-zA-Z]
$eol   = \n

tokens :-
  -- Whitespace insenstive
  $space+ ;

  -- Comments
  "#".* ;

  -- eol sensitive (?)
  $eol+ { \_ -> EOL }
  -- Syntax
  \(      { \_ -> LPAR }
  \)      { \_ -> RPAR }
  \,      { \_ -> COM }
  \<      { \_ -> LT }
  \>      { \_ -> GT }
  \+      { \_ -> PLUS }
  \-      { \_ -> MINUS }
  \*      { \_ -> MULT }
  \/      { \_ -> DIV }
  \%      { \_ -> MOD }
  "<="    { \_ -> LTE }
  ">="    { \_ -> GTE }
  "=="    { \_ -> EQ }
  "!="    { \_ -> NEQ }
  ":^)"   { \_ -> TRUE }
  ":^("   { \_ -> FALSE }
  "is"    { \_ -> EQ }
  "isn't" { \_ -> NEQ }
  "and"   { \_ -> AND }
  "or"    { \_ -> OR }
  "like"  { \_ -> ASS }
  "from"  { \_ -> FROM }
  "to"    { \_ -> TO }
  "by"    { \_ -> BY }
  "wew"   { \_ -> RVAL}
  ">inb4"     { \_ -> FOR }
  ">implying" {\_ -> IF }
  ">or"       {\_ -> ELSE }
  ">mfw"      { \_ -> PRINT }
  ">be"       { \_ -> DECL }
  ">tfw"      { \_ -> RET }
  ">wewlad"   { \_ -> FN }
  ">wew"      { \_ -> CALL }
  ">done"     { \_ -> ENDFOR }
  ">be me"    { \_ -> MAIN }
  ">done implying"    {\_ -> ENDIF }
  ">thank mr skeltal" {\_ -> EXIT}
  $digit+                       { \s -> INT (read s) }
  -- @TODO floats
  \" .* \"                      { \s -> STR (strip s) }
  $alpha [$alpha $digit \_ \']* { \s -> SYM s }

{
data Token = INT Int
           | SYM String
           | STR String
           | LPAR | RPAR | COM
           | IF | ELSE | ENDIF
           | FOR | FROM | TO | BY | ENDFOR
           | TRUE | FALSE
           | LT | GT | LTE | GTE
           | AND | OR
           | EQ | NEQ
           | PLUS | MINUS | MULT | DIV | MOD
           | DECL | ASS
           | FN | RET | CALL | PRINT | RVAL
           | MAIN | EXIT
           | EOL | EOF
           deriving (Eq, Show)

strip :: String -> String
strip ('"':xs) = go xs
  where go ['"'] = []
        go (x:xs)  = go xs

alexEOF = EOF

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\0', [], str)
  where
    go :: AlexInput -> Except String [Token]
    go inp@(_,bs,str) =
      case alexScan inp 0 of
        AlexEOF -> return []
        AlexError (_, _, str) -> throwError $ "Invalid lexeme: " <> str
        AlexSkip inp' len -> go inp'
        AlexToken inp' len act -> do
          let res = act (take len str)
          rest <- go inp'
          return (res:rest)
}
