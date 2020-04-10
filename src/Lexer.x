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
  $white+ ;

  -- Comments
  "#".* ;

  -- eol sensitive (?)
  $eol { \_ -> EOL }
  -- Syntax
  \(      { \_ -> LPAR }
  \)      { \_ -> RPAR }
  \,      { \_ -> COM }
  \<      { \_ -> LT }
  \>      { \_ -> GT }
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
  \" .* \"                      { \s -> STR (id s) } -- @TODO strip string
  $alpha [$alpha $digit \_ \']* { \s -> SYM s }
  [\+ \- \* \/ \%]              { \s -> OP s }



{

data Token = INT Int
           | SYM String
           | STR String
           | OP String
           | LPAR | RPAR | COM
           | IF | ELSE | ENDIF
           | FOR | FROM | TO | BY | ENDFOR
           | TRUE | FALSE
           | LT | GT | EQ | NEQ | LT | GT | LTE | GTE
           | AND | OR
           | DECL | ASS
           | FN | RET | CALL | RVAL
           | MAIN | EXIT
           | EOL | EOF
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
