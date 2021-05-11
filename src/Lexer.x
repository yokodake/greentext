{
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lexer
  ( Token(..)
  , scanTokens
  ) where

import Prelude hiding (Ordering(..))
-- import AST
import Control.Monad.Except
import Data.Int (Int32)

}

%wrapper "basic"

$space = [\ \t\f\v]
$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n\r]

tokens :-
  -- Whitespace insenstive
  $space+ ;

  -- Comments
  "#".* ;

  -- eol sensitive (?)
  $eol+   { \_ -> EOL }
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
  "baka"  { \_ -> NOT } -- FIXME
  ">inb4"      { \_ -> FOR }
  ">implying"  {\_ -> IF }
  ">or"        {\_ -> ELSE }
  ">mfw"       { \_ -> PRINT }
  ">be"        { \_ -> DECL }
  ">tfw"       { \_ -> RET }
  ">wewlad"    { \_ -> FN }
  ">wew"       { \_ -> CALL }
  ">be me"     { \_ -> MAIN }
  ">done inb4" { \_ -> ENDFOR }
  ">done implying"    {\_ -> ENDIF }
  ">thank mr skeltal" {\_ -> EXIT}
  $digit+                       { \s -> INT (read s) }
  -- @TODO floats
  \" .* \"                      { \s -> STR (strip s) }
  $alpha [$alpha $digit \_ \']* { \s -> SYM s }

{
data Token 
    -- literals
    = INT Int32
    | STR String
    | TRUE | FALSE
    -- variables
    | SYM String
    -- unary operators
    | NOT
    -- binary operators
    | LT | GT | LTE | GTE
    | AND | OR
    | EQ | NEQ
    | PLUS | MINUS | MULT | DIV | MOD
    -- conditional statements
    | IF | ELSE | ENDIF
    -- loops 
    | FOR | FROM | TO | BY | ENDFOR
    -- other keywords
    | CALL | PRINT | RVAL | EXIT
    | DECL | ASS
    | FN | RET 
    | MAIN 
    -- syntactic constructs 
    | LPAR | RPAR | COM
    | EOL 
    | EOF
    deriving (Eq, Show)

{-- ideas:
  1. wordfilters
    * baka (~smh) for unary negation
    * desu 
    * senpai
    * kek
    * keikaku
    * cringe/based
    * onions
  2. memearrows
    * >imagine
  3. other
-}

strip :: String -> String
strip ('"':xs) = go xs
  where go ['"'] = []
        go (_:xs)  = go xs

alexEOF = EOF

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\0', [], str)
  where
    go :: AlexInput -> Except String [Token]
    go inp@(_, _bs, str) =
      case alexScan inp 0 of
        AlexEOF -> return []
        AlexError (_, _, str) -> throwError $ "Invalid lexeme: " <> str
        AlexSkip inp' _len -> go inp'
        AlexToken inp' len act -> do
          let res = act (take len str)
          
          rest <- go inp'
          return (res:rest)
}
