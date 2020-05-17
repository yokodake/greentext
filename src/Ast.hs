module Ast where

type Program = [Decl]

data VAss = VAss { vname :: Sym, vexp :: (Maybe Expr) }
          deriving (Eq, Show)

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
          | Ret Expr
          deriving (Eq, Show)

data Expr = Lit LitV
          | Ref Sym
          | RVal -- previously returned value
          | Infix String Expr Expr
          deriving (Eq, Show)

data LitV = LStr String
          | LBoo Bool
          | LInt Int
          deriving (Eq, Show)

-- @TODO Symbols
type Sym = String

sym :: String -> Sym
sym = id
