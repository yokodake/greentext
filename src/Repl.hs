{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Repl where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (MonadState(..), StateT (StateT), execStateT, evalStateT, modify)
import Control.Monad.Trans (MonadTrans(lift))
import Data.ByteString.Char8 (ByteString, pack)
import Data.Char (isSpace)
import Data.Map ((!?))
import System.Console.Haskeline (InputT, getInputLine, runInputT, defaultSettings)
import Text.Printf (printf)

import Ast (ReplAst)
import Config
import Interp
import Parser (PError(..), parseReplLine)

class Monad m => MonadInput m where
  liftInput :: InputT IO a -> m a

instance MonadInput (InputT IO) where
  liftInput = id
  {-# INLINE liftInput #-}

instance MonadInput m => MonadInput (StateT s m) where
  liftInput = lift . liftInput
  {-# INLINE liftInput #-}

instance MonadState s m => MonadState s (InputT m) where
  get = lift get
  put = lift . put
  state = lift . state

type ReplM = StateT GtiEnv (InputT IO)

-- @Options@ is for later,
-- including bringing all definitions into scope of the repl
repl :: GtiEnv -> IO ()
repl env = (runInputT defaultSettings . flip evalStateT env) go
  where
    isCommand (':':_) = True
    isCommand _       = False
    prompt = " # "

    go :: ReplM ()
    go = do continue <- loop
            if continue then
              go
            else
              pure ()
    loop = do
      minput <- liftInput (getInputLine prompt)
      case minput of
        Nothing -> liftIO $ quitRepl
        Just input | isCommand input -> replCommand input
                   | isBlank input -> pure True
                   | otherwise -> liftInput $ getLines print input >> pure True

isBlank = all (== ' ')

quitRepl :: IO Bool
quitRepl = (putStrLn "Leaving.") >> return False

replCommand :: (MonadIO m, MonadState GtiEnv m) => String -> m Bool
replCommand ":q" = liftIO $ quitRepl
replCommand _    = liftIO (putStrLn "Uknown command") >> return True

-- TODO refactor this crap
-- | get (maybe) multiple lines of input
getLines :: (MonadIO m, MonadInput m) => (ReplAst -> IO ()) -> String -> m ()
getLines f firstLine = go firstLine
  where
    go input =
      do case parseReplLine input of
           Left (PError s)    -> liftIO $ print s
           Left UnexpectedEOI -> multiline input >>= liftIO . final f
           Right ast          -> liftIO (f ast)

    final f input =
      case parseReplLine input of
        Left UnexpectedEOI -> print "parse error: unexpected end of input"
        Left s             -> print s
        Right ast          -> f ast

    -- TODO strip spaces
    multiline :: MonadInput m => String -> m String
    multiline prev =
      do new <- liftInput (getInputLine " | ")
         case new of
           Nothing -> return prev
           Just s | isBlank s -> return (prev <> "\n")
                  | otherwise -> multiline (prev <> "\n" <> s)

extractCommand :: String -> Maybe (String, [String])
extractCommand (':':rest) = let ws = splitSpace [] rest in
                            (,) <$> hd ws <*> pure (tail ws)
  where
    hd [] = Nothing
    hd (x:xs) = Just x
    splitSpace acc []       = acc `seq` acc
    splitSpace acc (' ':xs) = acc `seq` splitSpace acc xs
    splitSpace acc (x:xs) = let (y, ys) = untilSpace (x:xs)
                            in splitSpace (acc <> [y]) ys
    untilSpace [] = ([], [])
    untilSpace (x:xs) | isSpace x = ([], xs)
                      | otherwise = case untilSpace xs of
                                      (y,ys) -> (x:y, ys)

commands :: [(String, [String] -> ReplM (), Bool)]
commands =
  [ ("q"   , const quitRepl                , False)
  , ("load", const (notImplemented ":load"), True)
  , ("set" , mapM_ setFlag         , True)
  ]
  where
    quitRepl = liftIO (putStrLn "Leaving.")
    notImplemented x = liftIO $ putStrLn ("NotImplemented yet: `" <> x <> "`")

    setFlag :: String -> ReplM ()
    setFlag fl@(pack -> flb) = case flag_map !? (getFlagName flb) of
      Nothing -> liftIO $ putStrLn (printf "unrecognised flag `%s`" fl)
      Just (Option _) -> liftIO $ putStrLn "commands.setFlag.Option: Not Implemented Yet"
      Just (Switch update) -> error "@TODO"
