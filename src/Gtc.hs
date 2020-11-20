module Gtc where

import System.FilePath

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
