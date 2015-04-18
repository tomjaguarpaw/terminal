{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}

module Terminal where

import           Prelude hiding (read)
import qualified System.Posix.Pty as Pty
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Char8    as Char8
import qualified Data.String      as String
import qualified Control.Concurrent       as Concurrent
import qualified Control.Monad.Free as F
import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Monad.Trans.Class as Trans

data Raw a = Read  (BS.ByteString -> a)
           | Write BS.ByteString a
             deriving Functor

data Bytes a = ReadByte   (Maybe Char -> a)
             | WriteBytes BS.ByteString a
               deriving Functor

read :: F.Free Raw BS.ByteString
read = F.liftF (Read id)

write :: BS.ByteString -> F.Free Raw ()
write bs = F.liftF (Write bs ())

bytes :: Bytes a -> State.StateT BS.ByteString (F.Free Raw) a
bytes r@(ReadByte continue) = do
  buffer <- State.get
  case Char8.uncons buffer of
    Nothing       -> do
      bs <- Trans.lift read
      State.put bs
      bytes r
    Just (hd, tl) -> do
      State.put tl
      return (continue (Just hd))
bytes (WriteBytes bs continue) = do
  Trans.lift (write bs)
  return continue





main :: IO ()
main = do
  (pty, _) <- Pty.spawnWithPty Nothing False "/bin/zsh" [] (20, 10)
  foo      <- Pty.readPty pty
  BS.putStr foo
  foo      <- Pty.readPty pty
  BS.putStr foo
  Pty.writePty pty (String.fromString "echo hi\n")
  Concurrent.threadDelay (1000 * 1000)
  foo      <- Pty.readPty pty
  BS.putStr foo
