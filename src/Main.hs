module Main where

import           Control.Monad.Trans      (MonadIO (liftIO))
import           Data.ByteString.Char8    (ByteString, pack)
import           Data.Maybe               (fromMaybe)
import           System.Console.Haskeline
import           System.Environment
import           System.Exit              (die)
import           Text.Printf              (printf)

import           Config (Target (Target), mkTarget, toDFlags, DFlags(..), fromTarget)
import           Gtc
import           Interp (GtiEnv)
import qualified Interp
import           Parser (PError (..), parseProgram, parseTokens, parseReplLine)
import           Ast (ReplAst)
import Debug.Trace
import Control.Monad (when)

main :: IO ()
main = do args <- getArgs
          case args of
            []        -> error "greentxt: no input file"
            "repl":xs -> mkIEnv (parseArgs xs) >>= exitLeft repl
            _         -> mkEnv (parseArgs args) >>= exitLeft eval
  where
    exitLeft _ (Left e) = die (printf "error: %s\n" e)
    exitLeft f (Right x) = f x

-- @Options@ is for later,
-- including bringing all definitions into scope of the repl
repl :: GtiEnv -> IO ()
repl _ = runInputT defaultSettings go 
  where
    isCommand (':':_) = True
    isCommand _       = False
    prompt = " # "

    go = do continue <- loop
            if continue then
              go
            else 
              pure ()

    loop = do
      minput <- getInputLine prompt
      case minput of
        Nothing -> quitRepl 
        Just input | isCommand input -> replCommand input
                   | isBlank input -> pure True
                   | otherwise -> getLines print input >> pure True

isBlank = all (== ' ')

quitRepl = liftIO (putStrLn "Leaving.") >> return False

replCommand :: String -> InputT IO Bool
replCommand ":q" = quitRepl
replCommand _    = liftIO (putStrLn "Uknown command") >> return True 

-- TODO refactor this crap
-- | get (maybe) multiple lines of input 
getLines :: (ReplAst -> IO ()) -> String -> InputT IO ()
getLines f firstLine = go firstLine
  where
    go input = 
      do case parseReplLine input of
           Left (PError s) -> liftIO $ print s 
           Left UnexpectedEOI -> multiline input >>= liftIO . final f 
           Right ast -> liftIO (f ast)

    final f input = 
      case parseReplLine input of 
        Left UnexpectedEOI -> print "parse error: unexpected end of input"
        Left s             -> print s
        Right ast          -> f ast

    -- TODO strip spaces
    multiline :: String -> InputT IO String
    multiline prev = 
      do new <- getInputLine " | "
         case new of
           Nothing -> return prev 
           Just s | isBlank s -> return (prev <> "\n")
                  | otherwise -> multiline (prev <> "\n" <> s)

{-
      do if null input then liftIO $ print "unexpected end of input."
         else case parseReplLine input of
           Left (PError s) -> liftIO $ print s
           Left UnexpectedEOI -> getInputLine " | " >>= multiline . ((input <> "\n") <>) . fromMaybe ""
           Right ast -> liftIO $ print ast
-}

eval :: GtcEnv -> IO ()
eval env@GtcEnv{flags, target} = 
  do when (_ddump_flags flags) (print env) 
     putStrLn ""
     src <- (readFile . fromTarget) target
     (print . parseProgram) src


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
    f o@MkOpts{flags_opts} s@('-':_) = o{flags_opts= pack s : flags_opts}
    f o s  = o{filepath_opts=Just s}

data Opts = MkOpts { filepath_opts :: Maybe FilePath
                   , flags_opts    :: [ByteString]
                   } deriving (Show, Eq)