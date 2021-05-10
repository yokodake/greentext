{-# LANGUAGE RecursiveDo #-}
module Emit where

import           Data.ByteString         as BS (length)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Coerce
import           Prelude                 hiding (init, return)

import           Ast
import           Code                    hiding (LInt, write)
import           Gtc

emitVarDec :: Decl -> GtcM s e ()
emitVarDec (Var n e) = error "global var: @TODO"
emitVarDec _         = error "emitVarDec: not a Var"

emitFunDec :: Decl -> GtcM Module e ()
emitFunDec (Fun n ps b) = error "emitFunDec: @TODO"
emitFunDec _            = error "emitFunDec: not a Fun"

mkFunBinding :: Sym -> Int -> Chunk -> GtcM s e ()
mkFunBinding = error "mkFunBinding: @TODO"

genFun :: Sym -> GtcM s e ()
genFun name = undefined
  where
    chunk name start bs = MkChunk {name, start, code = toStrict $ toLazyByteString bs}


emitStmts :: [Stmt] -> GtcM Module e ()
emitStmts = mapM_ emitStmt

emitStmt :: Stmt -> GtcM Module e ()
emitStmt node = case node of
  Exit             -> exit
  Print args       -> emitPrint args
  Ass var (Just e) -> newVar var >> emitExpr e >> store var >> pop
  Ass var Nothing  -> newVar var
  While p ss       -> emitWhile p ss
  For {}           -> undefined
  Cond p c a       -> emitCond p c a
  Call sym args    -> mapM_ emitExpr args >> call sym
  Ret Nothing      -> ret
  Ret (Just e)     -> emitExpr e >> retTop

emitWhile :: Expr -> [Stmt] -> GtcM Module e ()
emitWhile e ss = mdo start <-label
                     emitExpr e   -- while e
                     brf end      -- {
                     emitStmts ss
                     jmp start    -- }
                     end <- label
                     pure ()

emitCond :: Expr -> [Stmt] -> Maybe [Stmt] -> GtcM Module e ()
emitCond p c a = mdo emitExpr p
                     brf alt
                     emitStmts c
                     alt <- label
                     maybe (pure ()) emitStmts a

emitExpr :: Expr -> GtcM Module e ()
emitExpr e = case e of
  Lit lit -> pushLit lit
  Ref var -> pushVar var
  RVal    -> pushRet
  Infix op l r -> do emitExpr l
                     emitExpr r
                     emitOp op

emitOp :: String -> GtcM s e ()
emitOp op = write $ case op of
  "+"   -> Iadd
  "*"   -> Imul
  "-"   -> Imin
  "/"   -> Idiv
  "%"   -> Imod
  "and" -> Iand
  "or"  -> Ior
  "=="  -> Ieq
  "!="  -> Ineq
  ">"   -> Igt
  "<"   -> Ilt
  ">="  -> Ige
  "<="  -> Ile
  _     -> error "emitOp: not an operator"

emitPrint :: [Expr] -> GtcM s e ()
emitPrint = undefined

pushLit :: LitV -> GtcM Module e ()
pushLit lit = case lit of
  LStr _  -> undefined
  LDou _  -> undefined
  LBoo b -> do addr <- newConst b
               write Ilit1
               write addr
  LInt i -> do addr <- newConst i
               write Ilit4
               write addr


-- | adds a function to the env with name, param number and body
mkFun :: Sym -> Int -> Chunk -> GtcM s e ()
mkFun = undefined

-- | executes the action with the passed writer instead and returns that one
with :: Chunk -> GtcM s e () -> GtcM s e Chunk
with = undefined

-- | generate a new constant
newConst :: Marshal a => a -> GtcM Module e LAddr
newConst lit = do let bs = encode lit
                  sz <- BS.length . coerce . constants <$> get
                  modify' (\m@MkModule{constants} -> m{constants=constants <> coerce (encode lit)}) -- FIXME use lenses?
                  pure (fromIntegral sz)

newVar = error "newVar: @TODO"
pushVar = error "pushVar: @TODO"

-- * Instructions
exit :: GtcM s e ()
exit = write Iexit
{-# INLINE exit #-}

pushRet :: GtcM s e ()
pushRet = write IloadRet
{-# INLINE pushRet #-}

retTop :: GtcM s e ()
retTop = write Irettop
{-# INLINE retTop #-}

ret :: GtcM s e ()
ret = write Iret
{-# INLINE ret #-}

pop :: GtcM s e ()
pop = write Ipop
{-# INLINE pop #-}

store :: Sym -> GtcM s e ()
store _ = do error "store: @TODO"
             write Istore

print :: GtcM s e ()
print = error "print: @TODO"

call :: Sym -> GtcM s e ()
call = error "call: @TODO"

brf :: IAddr -> GtcM s e ()
brf a = do write Ibrf
           write a

jmp :: IAddr -> GtcM s e ()
jmp a = do write Ijmp
           write a
