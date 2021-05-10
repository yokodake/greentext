{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Config where

import           Data.ByteString.Char8 as BS (ByteString, drop, isPrefixOf,
                                              length, pack, unpack)
import           Data.Map              as Map (Map, fromList, (!?))
import           Data.Maybe            (isJust)
import           Data.String           (IsString (..))
import           Lens.Micro            (ASetter', set)
import           System.FilePath       (takeDirectory, takeFileName, (</>))
import           Text.Printf           (printf)
import           Utils                 (makeLenses')

import           Debug.Trace

-- | represents what has to be compiled
data Target = Target { fname :: String -- ^ filename
                     , fpath :: FilePath -- ^ path to the file
                     } deriving (Eq, Show)

-- | build the target from an absolute path
mkTarget :: FilePath -> Target
mkTarget p = Target { fname = takeFileName p, fpath = takeDirectory p }

-- | retrieve the fullpath form Target
fromTarget :: Target -> FilePath
fromTarget Target{fpath, fname} = fpath </> fname

-- | Dynamic Flags. Similar to haskell, they can be set by invocation (command-line),
-- in the repl (or maybe per file with pragmas?)
data DFlags = DFlags
  { rprint_static :: Bool
  , ddump_code    :: Bool
  , ddump_flags   :: Bool
  } deriving (Eq, Show)
makeLenses' ''DFlags

all_flags :: [(FlagName, FlagType)]
all_flags =
  [ ("rprint-static", Switch (setSwitch _rprint_static))
  , ("ddump-code"   , Switch (setSwitch _ddump_code))
  , ("ddump-flags"  , Switch (setSwitch _ddump_flags))
  ]
  where
    setSwitch :: ASetter' DFlags Bool -> FlagName -> DFlags -> DFlags
    setSwitch l fl = set l (parseSwitch fl)
    setOption :: ASetter' DFlags a -> (FlagName, ByteString) -> DFlags -> DFlags
    setOption l (fl,arg) = set l (parseOption fl arg)

    parseOption = undefined
    parseSwitch str | "--no-" `isPrefixOf` str = False
                    | "--" `isPrefixOf` str = True
                    | otherwise = error ("Config::flags::parseSwitch: flag not sanitized.")

-- | construct the @DFlags@ from a list of commandline args
toDFlags :: [ByteString] -> Either String DFlags
toDFlags ss = go defaultDFlags ss where
  traceFlag :: ByteString -> Maybe a -> Maybe a
  traceFlag name = flip trace <*> (printf "fl:%s isJust:%s\n" (show name) . show . isJust)
  go acc [] = Right acc
  go acc (f:fs) = maybeToEither (printf "invalid flag `%s`" (unpack f)) (flag_map !? getFlagName f)>>=
                  \case Switch update -> go (update f acc) fs
                        Option update -> case fs of
                          [] -> Left (printf "invalid usage of flag `%s`: expected an option" (unpack f))
                          x:xs -> go (update (f, x) acc) xs

-- TODO: refactor by adding the default value in @all_flags@
defaultDFlags :: DFlags
defaultDFlags = DFlags { rprint_static = True -- FIXME only for development
                       , ddump_code = False
                       , ddump_flags = False
                       }

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right


-- TODO handle invalid flag better
getFlagName str | not ("--" `isPrefixOf` str) = error (printf "Config::getFlagName: invalid FLAG `%s`" (unpack str))
                | "--no-" `isPrefixOf` str = BS.drop (BS.length "--no-") str
                | otherwise                = BS.drop (BS.length "--") str

data FlagType = Switch (ByteString -> DFlags -> DFlags)            -- ^ bool switch
              | Option  ((ByteString, ByteString) -> DFlags -> DFlags) -- ^ expects an option after the `--flag`

-- ("name", FlagType)
type FlagName = ByteString

flag_map :: Map FlagName FlagType
flag_map = fromList all_flags
