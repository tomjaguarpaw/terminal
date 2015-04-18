{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Terminal where

import           Prelude hiding (read)
import qualified System.Posix.Pty as Pty
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Char8    as Char8
import qualified Data.String      as String
import qualified Control.Concurrent       as Concurrent
import qualified Control.Monad      as Monad
import qualified Control.Monad.Free as F
import qualified Control.Monad.Trans.Free as TF
import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Morph as Morph
import qualified System.IO as IO

data Raw a = Read  (Maybe BS.ByteString -> a)
           | Write BS.ByteString a
             deriving Functor

data Bytes a = ReadByte (Maybe Char -> a)
             | WriteBytes BS.ByteString a
               deriving Functor

read :: F.Free Raw (Maybe BS.ByteString)
read = F.liftF (Read id)

write :: BS.ByteString -> F.Free Raw ()
write bs = F.liftF (Write bs ())

readByte :: F.Free Bytes (Maybe Char)
readByte = F.liftF (ReadByte id)

writeBytes :: BS.ByteString -> F.Free Bytes ()
writeBytes bs = F.liftF (WriteBytes bs ())

bytes :: Bytes a -> State.StateT BS.ByteString (F.Free Raw) a
bytes r@(ReadByte continue) = do
  buffer <- State.get
  case Char8.uncons buffer of
    Nothing       -> do
      bs <- Trans.lift read
      case bs of Nothing  -> return (continue Nothing)
                 Just  bs -> do
                   State.put bs
                   bytes r
    Just (hd, tl) -> do
      State.put tl
      return (continue (Just hd))
bytes (WriteBytes bs continue) = do
  Trans.lift (write bs)
  return continue

ptyRaw :: Pty.Pty -> Raw a -> IO a
ptyRaw pty (Read f) = do
  bs <- Pty.tryReadPty pty
  (return . f) $ case bs of Left _   -> Nothing
                            Right bs -> Just bs
ptyRaw pty (Write bs a) = do
  Pty.writePty pty bs
  return a

iter' :: (Monad m, Functor f) => (f (m a) -> m (m a)) -> F.Free f a -> m a
iter' f = F.iterM (Monad.join . f)


iterT' :: (Functor f, Monad m) => (f (m a) -> m (m a)) -> TF.FreeT f m a -> m a
iterT' f = TF.iterT (Monad.join . f)

bytesIO :: Bytes a -> State.StateT BS.ByteString (TF.FreeT Raw IO) a
bytesIO =  Morph.hoist F.toFreeT . bytes

ptyRawIO :: Pty.Pty -> TF.FreeT Bytes IO a -> IO a
ptyRawIO pty = iterT' (ptyRaw pty)
               . flip State.evalStateT BS.empty
               . iterT' bytesIO
               . TF.hoistFreeT (Trans.lift . Trans.lift)

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering

  (pty, _) <- Pty.spawnWithPty Nothing False "/bin/zsh" [] (20, 10)

  ptyRawIO pty $ flip mapM_ [1..100] $ \_ -> do
    b <- F.toFreeT readByte
    case b of Just c  -> Trans.lift (putStr (show c))
              Nothing -> return ()


  print "Here"

  (ptyRawIO pty . F.toFreeT . writeBytes . String.fromString) "echo hi\n"

  ptyRawIO pty $ flip mapM_ [1..100] $ \_ -> do
    b <- F.toFreeT readByte
    case b of Just c  -> Trans.lift (putStr (show c))
              Nothing -> return ()

character :: Char -> Character
character = \case '\t' -> Control TAB
                  '\b' -> Control BS
		  '\r' -> Control CR
                  '\f' -> Control LF
                  '\v' -> Control VT
                  '\n' -> Control LF
                  '\a' -> Control BEL
                  '\o33' -> Control ESC
                  '\o16' -> Control SO
                  '\o17' -> Control SI
                  '\o32' -> Control SUB
                  '\o30' -> Control CAN
                  '\o05' -> Control ENQ
                  '\o00' -> Control NUL
                  '\o21' -> Control XON
                  '\o23' -> Control XOFF
                  '\o177' -> Control DEL
                  c       -> Printing c

data Character = Control  Control
               | Printing Char

data Control = TAB
             | BS
             | CR
             | LF
             | VT
             | BEL
             | ESC
             | SO
             | SI
             | SUB
             | CAN
             | ENQ
             | NUL
             | XON
             | XOFF
             | DEL
