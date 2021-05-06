module Main where

import           Control.Monad.Trans      (MonadIO (liftIO))
import           Data.ByteString.Char8    (ByteString, pack)
import           Data.Maybe               (fromMaybe)
import           System.Console.Haskeline
import           System.Environment
import           System.Exit              (die)
import           Text.Printf              (printf)

import           Config (Target (Target), mkTarget, toDFlags)
import           Gtc
import           Interp (GtiEnv)
import qualified Interp
import           Parser (PError (..), parseProgram, parseTokens)

-- @Options@ is for later,
-- including bringing all definitions into scope of the repl
repl :: GtiEnv -> IO ()
repl _ = runInputT defaultSettings loop
  where
    prompt = " > "
    loop = do
      minput <- getInputLine prompt
      case minput of
        Nothing -> loop
        Just ":q"-> outputStrLn "Leaving."
        Just input -> (process input) >> loop

eval :: GtcEnv -> IO ()
eval = print

main :: IO ()
main = do args <- getArgs
          case args of
            []        -> error "greentxt: no input file"
            "repl":xs -> mkIEnv (parseArgs xs) >>= exitLeft repl
            _         -> mkEnv (parseArgs args) >>= exitLeft eval
  where
    exitLeft _ (Left e) = die (printf "error: %s\n" e)
    exitLeft f (Right x) = f x

-- | repl input process
process :: [Char] -> InputT IO ()
process input = multiline (input)
  where
    multiline input = 
      do if null input then liftIO $ print "unexpected end of input."
         else case parseProgram input of
           Left (PError s) -> liftIO $ print s
           Left UnexpectedEOI -> getInputLine ".| " >>= multiline . ((input <> "\n") <>) . fromMaybe ""
           Right ast -> liftIO $ print ast

-- | build the GtcEnv
mkEnv :: Opts -> IO (Either String GtcEnv)
mkEnv MkOpts{ filepath_opts, flags_opts}  = 
  do let flags = toDFlags flags_opts
     let target = maybe (Left "no file specified (TODO: print Usage)") (Right . mkTarget) filepath_opts
     return $ GtcEnv <$> flags <*> target

-- | builds the GtiEnv, for repl
mkIEnv :: Opts -> IO (Either String GtiEnv)
mkIEnv MkOpts{ filepath_opts, flags_opts } =
  do let flags = toDFlags flags_opts
     let target = case filepath_opts of
                    -- no file specified, so this is fine
                    Nothing -> Right Nothing 
                    -- if file specified but we couldn't make a target, we should error
                    Just f  -> maybe (Left "no file specified (TODO: print Usage)") (Right . Just . mkTarget) filepath_opts 
     return $ Interp.GtiEnv <$> flags <*> target

-- TODO handle case where multiple files are given
parseArgs :: [String] -> Opts
parseArgs = foldl f MkOpts{ filepath_opts = Nothing, flags_opts = [] }
  where
    -- IMPROVEMENT: getArgs converts the CStrings to linked lists, which is probably slower
    --              than making a bytestirng right away.
    f o@MkOpts{flags_opts} ('-':s) = o{flags_opts= pack s : flags_opts}
    f o s  = o{filepath_opts=Just s}

data Opts = MkOpts { filepath_opts :: Maybe FilePath
                   , flags_opts    :: [ByteString]
                   } deriving (Show, Eq)