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
eval env = print env

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
mkEnv :: (Maybe FilePath, Flags) -> IO GtcEnv
mkEnv (Nothing, fs) =
  return GtcEnv{flags=fs, target=Target{fname="",fpath=""}}
mkEnv (Just fn, fs) =
  do -- pwd <- getEnv "PWD"
     return GtcEnv{flags=fs, target=mkTarget fn}

-- | !TODO handle case where multiple files are given
parseArgs :: [String] -> (Maybe String, Flags)
parseArgs = foldl f (Nothing, [])
  where
    f (fn, fs) ('-':s) = (fn ,parseFlag s : fs)
    f (_,  fs) s  = (Just s, fs)
