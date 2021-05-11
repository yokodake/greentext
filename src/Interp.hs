{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples   #-}
module Interp where

import           Data.ByteString (ByteString)

import           Config          (DFlags, Target (..))
import           Stack           (Stack)
import           Utils           (makeLenses_)

-- | should be like @GtcEnv@ except the target is optional.
--   also contains the current environment (bindings from previous commands)
data GtiEnv = GtiEnv { flags  :: DFlags
                     , target :: Maybe Target
                     } deriving (Show)

makeLenses_ ''GtiEnv

-- | state of the VM
data GtiState = GtiState { code  :: {-# UNPACK #-} !ByteString
                         , ip    :: {-# UNPACK #-} !Int
                         , stack :: {-# UNPACK #-} !Stack
                         , ret   :: {-# UNPACK #-} !ByteString
                         } deriving Show

data RVal = VBoo !Bool
          | VInt !Int
          | VDou !Double
          | VStr !String -- QUESTION should this be ByteString instead to avoid unpacking?

-- exec :: GtiState -> IO GtiState
