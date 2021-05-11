{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Repl where

import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.State.Strict (MonadState (..), StateT (..),
                                             evalStateT, modify)
import           Control.Monad.Trans        (MonadTrans (lift))
import qualified Data.ByteString            as BW
import qualified Data.ByteString.Builder    as Bld
import           Data.ByteString.Char8      (empty, pack)
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (isSpace)
import           Data.Functor               (($>))
import           Data.Map.Strict            ((!?))
import qualified Data.Map.Strict            as M
import           Lens.Micro                 ((%~))
import           System.Console.Haskeline   (InputT, defaultSettings,
                                             getInputLine, runInputT)
import           Text.Printf                (printf)

import           Ast                        (ReplAst (..))
import           Code
import           Config
import           Data.Coerce                (coerce)
import           Debug                      (disassembleWithoutHeader)
import           Emit                       (emitExpr)
import           Gtc                        (GtcM (..), defaultGtcEnv)
import           Interp
import           Parser                     (PError (..), parseReplLine)

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
    stripSpace [] = []
    stripSpace (x:xs) | isSpace x = stripSpace xs
                      | otherwise = x:xs
    isCommand (stripSpace -> (':':_)) = True
    isCommand _                       = False
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
                   | otherwise -> liftInput $ getLines process input >> pure True

process :: ReplAst -> IO ()
process (RExp exp) =
  let env = defaultGtcEnv
      mod = MkModule (coerce empty) [] M.empty
      toChunk addr builder = MkChunk "it" addr (BW.toStrict $ Bld.toLazyByteString $ builder)
  in case runGtcM (emitExpr exp) env mod 0 of
  Left s -> putStrLn s
  Right (builder, mod, _, _) -> BL.putStr $ disassembleWithoutHeader (static mod) (toChunk 0 builder)
process _ = putStrLn "Not supported."

isBlank :: String -> Bool
isBlank = all isSpace

quitRepl :: IO Bool
quitRepl = (putStrLn "Leaving.") >> return False

replCommand :: String -> ReplM Bool
replCommand line =
  case extractCommand line of
    Nothing -> msg line
    Just (cmd, args) -> case commands_map !? cmd of
      Nothing -> msg cmd
      Just f  -> f args
  where msg s = liftIO (putStrLn (printf "unrecognized command: `%s`" s)) $> True


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
extractCommand (':':rest) =
  let hd []     = Nothing
      hd (x:_) = Just x
      splitSpace acc []       = acc `seq` acc
      splitSpace acc (' ':xs) = acc `seq` splitSpace acc xs
      splitSpace acc (x:xs) = let (y, ys) = untilSpace (x:xs)
                              in splitSpace (acc <> [y]) ys
      untilSpace [] = ([], [])
      untilSpace (x:xs) | isSpace x = ([], xs)
                        | otherwise = case untilSpace xs of
                                        (y,ys) -> (x:y, ys)
      ws = splitSpace [] rest
  in (,) <$> hd ws <*> pure (tail ws)
extractCommand _ = error "no command"

commands :: [(String, [String] -> ReplM (), Bool)]
commands =
  [ ("q"   , const quitRepl                , False)
  , ("load", const (notImplemented ":load"), True)
  , ("set" , mapM_ setFlag                 , True)
  ]
  where
    quitRepl = liftIO (putStrLn "Leaving.")
    notImplemented x = liftIO $ putStrLn ("NotImplemented yet: `" <> x <> "`")

    setFlag :: String -> ReplM ()
    setFlag fl@(pack -> flb) = case flag_map !? (getFlagName flb) of
      Nothing -> liftIO $ putStrLn (printf "unrecognised flag `%s`" fl)
      Just (Option _) -> liftIO $ putStrLn "commands.setFlag.Option: Not Implemented Yet"
      Just (Switch update) -> modify (_flags %~ update flb)

commands_map :: M.Map String ([String] -> ReplM Bool)
commands_map = M.fromList . map mkcmd $ commands
  where mkcmd (name, m, continue) = (name, \args -> m args >> pure continue)
