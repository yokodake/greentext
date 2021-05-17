{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Emit where

import           Control.Monad.State.Strict (gets)
import           Data.Map                   (member)
import           Data.Word                  (Word8)
import           Prelude                    hiding (init, return)
import           Text.Printf                (printf)

import           Ast
import           Code                       hiding (LInt)
import           Gtc hiding (local)

type EmitError = String
type EmitState = Module
type EmitM = GtcM EmitState String
cModule :: EmitState -> Module
cModule = id

err :: String -> EmitM a
err e = throwError e

emitVarDec :: Decl -> EmitM ()
emitVarDec (Var n e) = global n e
emitVarDec _         = error "emitVarDec: not a Var"

emitFunDec :: Decl -> EmitM ()
emitFunDec (Fun n ps b) = error "emitFunDec: @TODO"
emitFunDec _            = error "emitFunDec: not a Fun"

mkFunBinding :: Sym -> Int -> Chunk -> EmitM ()
mkFunBinding = error "mkFunBinding: @TODO"

emitStmts :: [Stmt] -> EmitM ()
emitStmts = mapM_ emitStmt

emitStmt :: Stmt -> EmitM ()
emitStmt node = case node of
  Exit             -> exit
  Print args       -> emitPrint args
  Ass var (Just e) -> local var >> emitExpr e >> store var >> pop
  Ass var Nothing  -> local var
  While p ss       -> emitWhile p ss
  For {}           -> undefined
  Cond p c a       -> emitCond p c a
  Call sym args    -> mapM_ emitExpr args >> call sym
  Ret Nothing      -> ret
  Ret (Just e)     -> emitExpr e >> retTop

emitWhile :: Expr -> [Stmt] -> EmitM ()
emitWhile e ss = mdo start <-label
                     emitExpr e   -- while e
                     brf end      -- {
                     emitStmts ss
                     jmp start    -- }
                     end <- label
                     pure ()

emitCond :: Expr -> [Stmt] -> Maybe [Stmt] -> EmitM ()
emitCond p c a = mdo emitExpr p
                     brf alt
                     emitStmts c
                     alt <- label
                     maybe (pure ()) emitStmts a

emitExpr :: Expr -> EmitM ()
emitExpr e = case e of
  Lit l -> lit l
  Ref var -> pushVar var
  RVal    -> pushRet
  Infix op l r -> do emitExpr l
                     emitExpr r
                     emitOp op

emitOp :: Sym -> EmitM ()
emitOp op = do
  op <- case op of
          "+"   -> pure Iadd
          "*"   -> pure Imul
          "-"   -> pure Imin
          "/"   -> pure Idiv
          "%"   -> pure Imod
          "and" -> pure Iand
          "or"  -> pure Ior
          "=="  -> pure Ieq
          "!="  -> pure Ineq
          ">"   -> pure Igt
          "<"   -> pure Ilt
          ">="  -> pure Ige
          "<="  -> pure Ile
          x     -> err (printf "unrecognized operator `%s`" x)
  write op

emitPrint :: [Expr] -> EmitM ()
emitPrint args = do mapM_ emitExpr args
                    write Iprint
                    let len = Prelude.length args
                    if len < fromIntegral (maxBound @Word8) then
                      write (toWord8 len)
                    else
                      err ("too many args for Print")

  where toWord8 x = fromIntegral x :: Word8

lit :: LitV -> EmitM ()
lit l = case l of
  LStr _  -> undefined
  LDou _  -> undefined
  LBoo b -> do addr <- litAddr b
               write Ilit1
               write addr
  LInt i -> do addr <- litAddr i
               write Ilit4
               write addr


-- | adds a function to the env with name, param number and body
mkFun :: Sym -> Int -> Chunk -> EmitM ()
mkFun = undefined

-- | executes the action with the passed writer instead and returns that one
with :: Chunk -> EmitM () -> EmitM Chunk
with = undefined

-- | generate a new constant
--
-- @IMPROVEMENT eliminate duplicate literals
litAddr :: Marshal a => a -> EmitM SAddr
litAddr lit = do mod <- gets cModule
                 let (mod', addr) = insertStatic lit mod
                 put mod'
                 pure addr

global :: Sym -> Maybe Expr -> EmitM ()
global n m_e = do mod <- gets cModule
                  case n `member` globals mod of
                    True -> fail
                    False  -> do let code = maybe initNil assign m_e
                                 put . fst $ insertGlobal n code mod
  where
    fail = err (printf "global variable `%s` already declared" n)

local :: Sym -> EmitM ()
local = error "local: @TODO"

initNil :: SAddr -> Code
initNil = error "assign: @TODO"

assign :: Expr -> SAddr -> Code
assign _ _ = error "assign: @TODO"

pushVar = error "pushVar: @TODO"

-- * Instructions
exit :: EmitM ()
exit = write Iexit
{-# INLINE exit #-}

pushRet :: EmitM ()
pushRet = write IloadRet
{-# INLINE pushRet #-}

retTop :: EmitM ()
retTop = write Irettop
{-# INLINE retTop #-}

ret :: EmitM ()
ret = write Iret
{-# INLINE ret #-}

pop :: EmitM ()
pop = write Ipop
{-# INLINE pop #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

store :: Sym -> EmitM ()
store _ = do _ <- error "store: @TODO"
             write Istore

print :: EmitM ()
print = error "print: @TODO"

call :: Sym -> EmitM ()
call = error "call: @TODO"

brf :: IAddr -> EmitM ()
brf a = do write Ibrf
           write a

jmp :: IAddr -> EmitM ()
jmp a = do write Ijmp
           write a
