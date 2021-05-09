module Ast where

import Data.Int (Int32)

type Program = [Decl]

data Decl = Var Sym (Maybe Expr)
          | Fun Sym [String] [Stmt]
          deriving (Eq, Show)

data Stmt = Exit
          | Print [Expr]
          | Ass Sym (Maybe Expr)
          | While Expr [Stmt]
          | For Sym Expr Expr (Maybe Expr) [Stmt]
          | Cond Expr [Stmt] (Maybe [Stmt])
          | Call Sym [Expr]
          | Ret (Maybe Expr)
          deriving (Eq, Show)

data Expr = Lit LitV
          | Ref Sym
          | RVal -- previously returned value
          | Infix String Expr Expr
          deriving (Eq, Show)

-- | @NOTE Can we use GADTs here?
data LitV = LStr String
          | LBoo Bool
          | LInt Int32
          | LDou Double
          deriving (Eq, Show)

-- | helper in the parser so we can parse @Var@ and @Ass@ with same rule.
--   this means that this node never occurs in the AST!
data VAss = VAss { vname :: Sym, vexp :: Maybe Expr }
          deriving (Eq, Show)

-- * REPL
data ReplAst = RExp Expr
             | RStm Stmt
             | RFun Decl
             deriving (Eq, Show)

-- * Helpers
-- @TODO Symbols
type Sym = String

sym :: String -> Sym
sym = id
