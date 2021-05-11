{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Driver where

import           Data.ByteString.Char8    as B (ByteString, pack, packCString, head, unpack)
import           System.Exit              (die)
import           Text.Printf              (printf)

import           Config                   (DFlags (..), Target (Target),
                                           fromTarget, mkTarget, toDFlags)
import           Control.Monad            (when)
import           Debug.Trace
import           Foreign                  (Ptr, Storable (peek), advancePtr,
                                           alloca, peekArray)
import           Foreign.C                (CInt, CString)
import           Gtc
import           Interp                   (GtiEnv)
import qualified Interp
import           Parser                   (parseProgram, parseTokens)
import Repl (repl)


main :: IO ()
main = do args <- getArgs_
          case args of
            x:xs | x == pack "repl" -> mkIEnv (parseArgs xs) >>= exitLeft repl
            []   -> error "greentxt: no input file"
            _    -> mkEnv (parseArgs args) >>= exitLeft eval
  where
    exitLeft _ (Left e)  = die (printf "error: %s\n" e)
    exitLeft f (Right x) = f x

eval :: GtcEnv -> IO ()
eval env@GtcEnv{flags, target} =
  do when (ddump_flags flags) (print env)
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
                    Just _  -> maybe (Left "no file specified (TODO: print Usage)")
                                     (Right . Just . mkTarget) filepath_opts
     return $ Interp.GtiEnv <$> flags <*> target

-- TODO handle case where multiple files are given
parseArgs :: [ByteString] -> Opts
parseArgs = foldl f MkOpts{ filepath_opts = Nothing, flags_opts = [] }
  where
    -- use 'unsafeHead'? aren't all args guaranteed to be non empty strings?
    f o@MkOpts{flags_opts} s@(B.head -> '-') = o{flags_opts= s : flags_opts}
    f o s                                    = o{filepath_opts=Just (unpack s)}

data Opts = MkOpts { filepath_opts :: Maybe FilePath
                   , flags_opts    :: [ByteString]
                   } deriving (Show, Eq)

getArgs_ :: IO [ByteString]
getArgs_ =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
    getProgArgv p_argc p_argv
    argc <- fromIntegral <$> peek p_argc
    argv <- peek p_argv
    peekArray (argc - 1) (advancePtr argv 1) >>= mapM packCString

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
