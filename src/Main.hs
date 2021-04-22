module Main where

import System.Environment

import Control.Monad.Trans
import System.Console.Haskeline

import Parser (parseProgram, parseTokens)
import Gtc

-- @Options@ is for later,
-- including bringing all definitions into scope of the repl
repl :: GtcEnv -> IO ()
repl _ = runInputT defaultSettings loop
  where
    prompt = " > "
    loop = do
      minput <- getInputLine prompt
      case minput of
        Nothing -> outputStrLn "Leaving."
        Just input -> liftIO (process input) >> loop

eval :: GtcEnv -> IO ()
eval = print

main :: IO ()
main = do args <- getArgs
          case args of
            []        -> error "greentxt: no input file"
            "repl":xs -> mkEnv (parseArgs xs) >>= repl
            _         -> mkEnv (parseArgs args) >>= eval

-- | repl input process
process :: String -> IO ()
process input = do
  let tokens = parseTokens input
  putStrLn ("Tokens: " ++ show tokens)
  let ast = parseProgram input
  putStrLn ("Syntax: " ++ show ast)

-- | build the GtcEnv
mkEnv :: Opts -> IO GtcEnv
mkEnv MkOpts{ filepath_opts, flags_opts=flags }  = return GtcEnv{ target, flags }
  where target = maybe 
                   Target{ fname="", fpath="" }
                   mkTarget
                   filepath_opts

-- | !TODO handle case where multiple files are given
parseArgs :: [String] -> Opts
parseArgs = foldl f MkOpts{ filepath_opts = Nothing, flags_opts = [] }
  where
    f o@MkOpts{flags_opts} ('-':s) = o{flags_opts= parseFlag s : flags_opts}
    f o s  = o{filepath_opts=Just s}

data Opts = MkOpts { filepath_opts :: Maybe FilePath
                   , flags_opts    :: Flags
                   } deriving (Show, Eq)