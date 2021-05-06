module Main where

import System.Environment

import Control.Monad.Trans
import System.Console.Haskeline

import Parser (parseProgram, parseTokens, PError (..)) 
import Gtc
import Interp (GtiEnv)
import qualified Interp
import Data.Maybe (fromMaybe)

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
            "repl":xs -> mkIEnv (parseArgs xs) >>= repl
            _         -> mkEnv (parseArgs args) >>= eval

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
mkEnv :: Opts -> IO GtcEnv
mkEnv MkOpts{ filepath_opts, flags_opts=flags }  = return GtcEnv{ target, flags }
  where target = maybe 
                   Target{ fname="", fpath="" }
                   mkTarget
                   filepath_opts

-- | builds the GtiEnv, for repl
mkIEnv :: Opts -> IO GtiEnv 
mkIEnv MkOpts{ filepath_opts=target, flags_opts=flags }  = 
  return $ Interp.GtiEnv flags (mkTarget <$> target)

-- | !TODO handle case where multiple files are given
parseArgs :: [String] -> Opts
parseArgs = foldl f MkOpts{ filepath_opts = Nothing, flags_opts = [] }
  where
    f o@MkOpts{flags_opts} ('-':s) = o{flags_opts= parseFlag s : flags_opts}
    f o s  = o{filepath_opts=Just s}

data Opts = MkOpts { filepath_opts :: Maybe FilePath
                   , flags_opts    :: Flags
                   } deriving (Show, Eq)