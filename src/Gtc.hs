{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- GreenText Compiler: environment and code gen monad.
module Gtc
    ( -- Greentext compiler
      -- * Environment
      GtcEnv(..)
    , defaultGtcEnv
      -- * Monad
    , GtcM
    , runGtcM
    , label
    , Write(..)
      -- ** re-exports
    , MonadError(..)
    , MonadFix(..)
    , MonadReader(..)
    , asks
    , MonadState(..)
    , modify'
    , modify
      -- ** inner types
    , ByteBuilder
    , IAddr
    ) where

-- import Control.Monad.Writer.Class
import           Control.Applicative        (liftA2)
import           Control.Monad.Except       (MonadError (..))
import           Control.Monad.Fix          (MonadFix (..))
import           Control.Monad.Reader.Class (MonadReader (..), asks)
import           Control.Monad.State.Class  (MonadState (..), modify, modify')
import qualified Data.ByteString            as S
import           Data.ByteString.Builder    (Builder, byteString,
                                             lazyByteString, word8)
import qualified Data.ByteString.Lazy       as L
import           Data.Coerce                (coerce)

import           Code
import           Config                     (DFlags, Target, defaultDFlags,
                                             mkTarget)

-- * Environment
-- | Greentext compiler envrionment, context and other stuff
data GtcEnv = GtcEnv { flags  :: DFlags -- ^ settings & cmdline passed flags
                     , target :: Target -- ^ the file to be interpreted
                     } deriving (Show)
defaultGtcEnv :: GtcEnv
defaultGtcEnv = GtcEnv { flags = defaultDFlags , target = mkTarget "<repl>"}

-- | bytecode builder
type ByteBuilder = Builder

-- | Code generation monad ~ ExceptT RWS
newtype GtcM s e a =
  GtcM { runGtcM :: GtcEnv -> s -> IAddr -> Either e (ByteBuilder, s, IAddr, a)
       }

instance Functor (GtcM s e) where
  fmap f (GtcM m) = GtcM
    $ \r s i -> case m r s i of
      Left e             -> Left e
      Right (b, s, i, a) -> Right (b, s, i, f a)
  {-# INLINE fmap #-}

instance Applicative (GtcM s e) where
  pure a = GtcM $ \_ s i -> Right (mempty, s, i, a)
  {-# INLINE pure #-}

  (GtcM mf) <*> (GtcM ma) = GtcM
    $ \r s0 i0 -> do
      (w1, s1, i1, f) <- mf r s0 i0
      (w2, s2, i2, a) <- ma r s1 i1
      return (w1 <> w2, s2, i2, f a)
  {-# INLINE (<*>) #-}

instance Monad (GtcM s e) where
  return = pure
  {-# INLINE return #-}

  m >>= k = GtcM
    $ \r s0 i0 -> do
      (w1, s1, i1, a) <- runGtcM m r s0 i0
      (w2, s2, i2, b) <- runGtcM (k a) r s1 i1
      return (w1 <> w2, s2, i2, b)
  {-# INLINE (>>=) #-}

instance MonadError e (GtcM s e) where
  throwError e = GtcM $ \_ _ _ -> Left e
  {-# INLINE throwError #-}

  catchError (GtcM m) f = GtcM
    $ \r s i -> case m r s i of
      Left e  -> runGtcM (f e) r s i
      Right a -> Right a
  {-# INLINE catchError #-}

instance MonadReader GtcEnv (GtcM s e) where
  reader f = GtcM $ \r s i -> pure (mempty, s, i, f r)
  {-# INLINE reader #-}

  local f (GtcM m) = GtcM $ \r s i -> m (f r) s i
  {-# INLINE local #-}

instance MonadState s (GtcM s e) where
  get = GtcM $ \_ s i -> Right (mempty, s, i, s)
  {-# INLINE get #-}

  put s = GtcM $ \_ _ i -> Right (mempty, s, i, ())
  {-# INLINE put #-}

  state f = GtcM
    $ \_ s0 i -> let (a, s1) = f s0
                 in Right (mempty, s1, i, a)
  {-# INLINE state #-}

instance MonadFix (GtcM s e) where
  mfix f = GtcM
    $ \r s i ->
      let x = runGtcM (f a) r s i
          (_, _, _, a) = unEither x
      in x
    where
      unEither (Right a) = a
      unEither (Left _)  = error "mfix: Left"

{-- might not want that, since we'll have to update the IAddr, and Builders don't track sizes
 -- so, either we wrap around Builders and add size info, or we don't use Writer
 -- another alternative is to use `State`, since that's essentially what it is.

instance MonadWriter ByteBuilder (GtcM s e) where
  writer (a, w) = GtcM $ \r s i -> Right (w, s, i, a)
  {-# INLINE writer #-}
  listen m = GtcM $ \r s i -> (\(w, s, i, a) -> (w, s, (a, w))) <$> runGtcM m r s
  {-# INLINE listen #-}
  pass m = GtcM $ \r s i -> (\(w, s, (a, f)) -> (f w, s, a)) <$> runGtcM m r s
  {-# INLINE pass #-}
-}

-- | gets the current location in the bytebuilder
label :: GtcM s e IAddr
label = GtcM $ \_ s i -> pure (mempty, s, i, i)

class Write a where
  write :: a -> GtcM s e ()
  size :: a -> Int

instance Write Instr where
  size _ = 1
  write x = write_ (word8 x) 1
instance Write OpCode where
  size _ = 1
  write = write . ctob
instance Write [Instr] where
  size = length
  write xs = let bs = S.pack xs
             in bs `seq` write_ (byteString bs) (S.length bs)
instance Write [OpCode] where
  size = length
  write xs = let bs = (S.pack . fmap ctob) xs
             in bs `seq` write_ (byteString bs) (S.length bs)

instance Write S.ByteString where
  size = S.length
  write bs = write_ (byteString bs) (S.length bs)
instance Write L.ByteString where
  size = fromIntegral . L.length
  write bs = write_ (lazyByteString bs) (size bs)
instance Write IAddr where
  size _ = 2
  write = liftA2 write_ (byteString . coerce encode_w16) size
instance Write SAddr where
  size _ = 2
  write = liftA2 write_ (byteString . coerce encode_w16) size

write_ :: ByteBuilder -> Int -> GtcM s e ()
write_ bb len = GtcM
  $ \_ s i -> Right (bb, s, i + MkIAddr (fromIntegral len), ())
