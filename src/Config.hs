{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Data.ByteString.Char8 as BS (ByteString, drop, isPrefixOf,
                                              pack, unpack, length)
import           Data.Map              as Map (Map, fromList, (!?))
import           Data.String           (IsString (..))
import           System.FilePath       (takeDirectory, takeFileName)
import           Text.Printf           (printf)

import Debug.Trace
import Data.Maybe (isJust)

-- | represents what has to be compiled
data Target = Target { fname :: String -- ^ filename
                     , fpath :: FilePath -- ^ path to the file
                     } deriving (Eq, Show)

-- | build the target from an absolute path
mkTarget :: FilePath -> Target
mkTarget p = Target { fname = takeFileName p, fpath = takeDirectory p }

-- | Dynamic Flags. Similar to haskell, they can be set by invocation (command-line),
-- in the repl (or maybe per file with pragmas?)
data DFlags = DFlags
  { _rprint_static :: Bool
  , _ddump_code    :: Bool
  } deriving (Eq, Show)

all_flags :: [(FlagName, FlagType)]
all_flags =
  [ ("rprint-static", Switch (\s d@DFlags{_rprint_static} -> d{_rprint_static= parseSwitch s}))
  , ("ddump-code"   , Switch (\s d@DFlags{_ddump_code}    -> d{_ddump_code= parseSwitch s}))
  ]
  where
    parseSwitch str | "--no-" `isPrefixOf` str = False
                    | "--" `isPrefixOf` str = True
                    | otherwise = error ("Config::flags::parseSwitch: flag not sanitized.")

-- | construct the @DFlags@ from a list of commandline args
toDFlags :: [ByteString] -> Either String DFlags
toDFlags ss = go defaultDFlags ss where
  go acc [] = Right acc
  go acc (f:fs) = maybeToEither (printf "invalid flag `%s`" (unpack f)) (flag_map !? getFlagName f)>>=
                  \case Switch update -> go (update f acc) fs
                        Option update -> case fs of
                          [] -> Left (printf "invalid usage of flag `%s`: expected an option" (unpack f))
                          x:xs -> go (update (f, x) acc) xs

-- TODO: refactor by adding the default value in @all_flags@
defaultDFlags :: DFlags
defaultDFlags = DFlags { _rprint_static = True -- FIXME only for development
                       , _ddump_code = False
                       }

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right


-- TODO handle invalid flag better
getFlagName str | not ("--" `isPrefixOf` str) = error (printf "Config::getFlagName: invalid FLAG `%s`" (unpack str)) 
                | "--no-" `isPrefixOf` str = BS.drop (BS.length "--no-") str
                | otherwise                = BS.drop (BS.length "--") str

data FlagType = Switch (ByteString -> DFlags -> DFlags)            -- ^ bool switch
              | Option  ((ByteString, ByteString) -> DFlags -> DFlags) -- ^ expects an option after the `--flag`

-- !TODO lenses would be ideal here
-- ("name", FlagType)
type FlagName = ByteString
flag_map :: Map FlagName FlagType
flag_map = fromList all_flags
