{-# LANGUAGE ViewPatterns #-}

module OverlayMsg (
  OverlayMsg(..), hGetOverlayMsg, hPutOverlayMsg
) where

import Control.Monad
import Control.Applicative
import Control.Exception
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO

data OverlayMsg =
      OverlayMsgInit {
        overlayMsgInitWidth  :: Int,
        overlayMsgInitHeight :: Int
  } | OverlayMsgShmem {
        overlayMsgShmemName :: String
  } | OverlayMsgBlit {
        overlayMsgBlitX      :: Int,
        overlayMsgBlitY      :: Int,
        overlayMsgBlitWidth  :: Int,
        overlayMsgBlitHeight :: Int
  } | OverlayMsgActive {
        overlayMsgActiveX      :: Int,
        overlayMsgActiveY      :: Int,
        overlayMsgActiveWidth  :: Int,
        overlayMsgActiveHeight :: Int
  } | OverlayMsgPid {
        overlayMsgPidPid :: Int
  } | OverlayMsgFps {
        overlayMsgFpsFps :: Float
  } | OverlayMsgInteractive {
        overlayMsgInteractiveInteractive :: Bool
  } deriving (Show)

overlayMagicNumber :: Num a => a
overlayMagicNumber = 5

getNum32host :: (Num a) => Get a
getNum32host = fromIntegral `fmap` getWord32host

hGetOverlayMsg :: Handle -> IO OverlayMsg
hGetOverlayMsg h = do
  str <- L.hGet h 8
  when (L.length str < 8) $
    throw $ userError "hGetOverlayMsg: couldn't read message header"
  let l = runGet getNum32host (L.drop 4 str)
  (L.splitAt 4 -> (tstr, bstr)) <- L.hGet h (4 + fromIntegral l)
  when (L.length bstr < l) $
    throw $ userError "hGetOverlayMsg: couldn't read message body"
  let n = getNum32host
      s = (L8.unpack . fst . L.break (==0)) `fmap` getRemainingLazyByteString
      b = toEnum `fmap` getNum32host
  return . flip runGet bstr $ case runGet getWord32host tstr of
    0 -> OverlayMsgInit <$> n <*> n
    1 -> OverlayMsgShmem <$> s
    2 -> OverlayMsgBlit <$> n <*> n <*> n <*> n
    3 -> OverlayMsgActive <$> n <*> n <*> n <*> n
    4 -> OverlayMsgPid <$> n
    5 -> OverlayMsgFps <$> pure 0 -- (wordToFloat `fmap` getWord32host)
    6 -> OverlayMsgInteractive <$> b
    _ -> throw $ userError "hGetOverlayMsg: unknown message type"

hPutOverlayMsg :: Handle -> OverlayMsg -> IO ()
hPutOverlayMsg pipeHandle msg = do
  let n = putWord32host . fromIntegral
      s l str =    putLazyByteString (L8.pack str)
                *> replicateM_ (l - length str) (putWord8 0)
  let str = runPut $ case msg of
                       OverlayMsgInit w h       -> n 0 *> n w *> n h
                       OverlayMsgShmem p        -> n 1 *> s 2048 p
                       OverlayMsgBlit x y w h   -> n 2 *> n x *> n y *> n w *> n h
                       OverlayMsgActive x y w h -> n 3 *> n x *> n y *> n w *> n h
                       OverlayMsgPid p          -> n 4 *> n p
                       OverlayMsgFps f          -> n 4 *> n 0 -- putWord32host (floatToWord f)
                       OverlayMsgInteractive i  -> n 4 *> n (fromEnum i)
  let buf = runPut (   putWord32host overlayMagicNumber
                    *> putWord32host (fromIntegral $ L.length str - 4)
                    *> putLazyByteString str)
  L.hPut pipeHandle buf
  hFlush pipeHandle
