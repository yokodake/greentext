module Emit where

import Prelude hiding (init)

import Ast
import Code

emitVarDec :: Decl -> GtcM ()
emitVarDec (Var n e) = do newGlobal n
                          case e of
                            Nothing -> pure ()
                            Just e -> atStart (emitExpr e >> store n)

emitFunDec :: Decl -> GtcM ()
emitFunDec (Fun n ps b) = body >>= mkFunBinding n (length ps)
  where
    body = with init $
             do mapM_ newVar ps
                emitStmts body
                -- return ??

emitStmts :: [Stmt] -> GtcM ()
emitStmts = mapM_ emitStmt

emitStmt :: Stmt -> GtcM ()
emitStmt node = case node of
  Exit -> exit
  Print args -> mapM_ emitExpr args >> print
  Ass var (Just e) -> newVar var >> emitExpr e >> store var >> pop
  Ass var Nothing  -> newVar var
  While _ _ -> undefined
  For _ _ _ _ _ -> undefined
  Cond _ _ _ -> undefined
  Call sym args -> mapM_ emitExpr args >> call sym
  Ret Nothing -> return
  Ret (Just e) -> emitExpr e >> returnTop

emitExpr :: Expr -> GtcM ()
emitExpr e = case e of
  Lit lit -> pushLit lit
  Ref var -> pushVar var
  RVal    -> pushRet
  Infix op l r -> do emitExpr l
                     emitExpr r
                     emitOp op

emitOp :: String -> Instr
emitOp op = write $ case op of
  "+"   -> Iadd
  "*"   -> Imul
  "-"   -> Imin
  "/"   -> Idiv
  "%"   -> Imod
  "and" -> Iand
  "or"  -> Ior
  "=="  -> Ieq
  "!="  ->  Ineq
  ">"   -> Igt
  "<"   -> Ilt
  ">="  -> Ige
  "<="  -> Ile

pushLit :: LitV -> GtcM ()
pushLit lit = case lit of
  LStr _  -> undefined
  LBoo b -> do addr <- newConst b
               write Ilit1
               write addr
  LInt i -> do addr <- newConst i
               write Ilit4
               write addr


-- | adds a function to the env with name, param number and body
mkFun :: Sym -> Int -> Chunk -> GtcM ()
mkFun = undefined

-- | executes the action with the passed writer instead and returns that one
with :: Chunk -> GtcM () -> GtcM Chunk
with = undefined
