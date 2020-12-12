{-# Language KindSignatures
           , MultiParamTypeClasses
           , FlexibleInstances #-}
module Gtc where

import System.FilePath
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Except

-- * Environment
-- | Greentext compiler envrionment, context and other stuff
data GtcEnv =
  GtcEnv { flags :: Flags
         -- ^ settings & cmdline passed flags
         , target :: Target
         -- ^ the file to be interpreted
         } deriving Show


-- | represents what has to be interpreted
data Target = Target { name :: String
                     -- ^ filename
                     , path :: FilePath
                     -- ^ path to the file
                     } deriving (Eq, Show)

-- | build the target from an absolute path
mkTarget :: FilePath -> Target
mkTarget p = Target{name=takeFileName p, path=takeDirectory p}

-- | temporary
type Flags = [Flag]

-- | temporary
type Flag = String

parseFlag :: String -> Flag
parseFlag ('-':s) = s
parseFlag s       = s

type ByteBuilder = ()

-- * Monad
-- | Code generation monad ~ ExceptT RWS
newtype GtcM s e a =
  GtcM { runGtcM :: GtcEnv -> s -> Either e (ByteBuilder, s, a) }

instance Functor (GtcM s e) where
  fmap f (GtcM m) = GtcM $ \r s -> case m r s of
                                     Left e -> Left e
                                     Right (b, s, a) -> Right (b, s, f a)
  {-# INLINE fmap #-}

instance Applicative (GtcM s e) where
  pure a = GtcM $ \r s -> Right (mempty, s, a)
  {-# INLINE pure #-}
  (GtcM mf) <*> (GtcM ma) = GtcM $ \r s0 ->
    do (w0, s1, f) <- mf r s0
       (w1, s2, a) <- ma r s1
       return (w0 <> w1, s2, f a)
  {-# INLINE (<*>) #-}

instance Monad (GtcM s e) where
  return = pure
  {-# INLINE return #-}
  m >>= k = GtcM $ \r s0 ->
    do (w0, s1, a) <- runGtcM m r s0
       (w1, s2, b) <- runGtcM (k a) r s1
       return (w0 <> w1, s2, b)
  {-# INLINE (>>=) #-}

instance MonadError e (GtcM s e) where
  throwError e = GtcM $ \_ _ -> Left e
  {-# INLINE throwError #-}
  catchError (GtcM m) f = GtcM $ \r s -> case m r s of
                                           Left e -> runGtcM (f e) r s
                                           Right a -> Right a
  {-# INLINE catchError #-}

instance MonadReader GtcEnv (GtcM s e) where
  reader f = GtcM $ \r s -> pure (mempty, s, f r)
  {-# INLINE reader #-}
  local f (GtcM m) = GtcM $ \r s -> m (f r) s
  {-# INLINE local #-}

instance MonadWriter ByteBuilder (GtcM s e) where
  writer (a, w) = GtcM $ \r s -> Right (w, s, a)
  {-# INLINE writer #-}
  listen m = GtcM $ \r s -> (\(w, s, a) -> (w, s, (a, w))) <$> runGtcM m r s
  {-# INLINE listen #-}
  pass m = GtcM $ \r s -> (\(w, s, (a, f)) -> (f w, s, a)) <$> runGtcM m r s
  {-# INLINE pass #-}

instance MonadState s (GtcM s e) where
  get = GtcM $ \r s -> Right (mempty, s, s)
  {-# INLINE get #-}
  put s = GtcM $ \r s -> Right (mempty, s, ())
  {-# INLINE put #-}
  state f = GtcM $ \r s0 -> let (a, s1) = f s0 in
                              Right (mempty, s1, a)
  {-# INLINE state #-}
