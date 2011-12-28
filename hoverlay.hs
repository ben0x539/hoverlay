{-# LANGUAGE ViewPatterns #-}

import Control.Concurrent
import Control.Monad
import Control.Applicative
import System.IO
import System.Environment
import Data.Array.MArray
import Data.Maybe
import Foreign.Ptr
import Foreign.Storable

import Prelude hiding (catch)
import Control.Exception

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import System.Posix.IO
import System.Posix.SharedMem
import System.Posix.Files

import Network
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Bindings.MMap

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
      s = ((L8.unpack . fst . L.break (==0)) `fmap` getRemainingLazyByteString)
      b = (toEnum `fmap` getNum32host)
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

drawDefaultImage :: IO Surface
drawDefaultImage = do
  let r = 16
      s = 2
      w = 400
      h = 48
      x = 0
      y = 0
  surf <- createImageSurface FormatARGB32 (round w) (round h)
  renderWith surf $ do
    setOperator OperatorClear
    paint
    setOperator OperatorOver
    translate x y
    arc (r+s) (r+s) r pi (pi*1.5)
    lineTo (w-r-s) s
    arc (w-r-s) (r+s) r (pi*1.5) (pi*2)
    lineTo (w-s) (h-r-s)
    arc (w-r-s) (h-r-s) r 0 (pi*0.5)
    lineTo (r+s) (h-s)
    arc (r+s) (h-r-s) r (pi*0.5) pi
    lineTo s (r+s)
    setSourceRGBA 0.5 0.5 0.5 0.5
    fillPreserve
    setSourceRGBA 0 0 0 1
    setLineWidth (s*2)
    stroke
    moveTo r 36
    setSourceRGBA 1 1 1 1
    setFontSize 28
    showText "No connection to Mumble"
  return surf

data LoopState = LoopState {
    loopWindow       :: Window,
    loopPipeHandle   :: Handle,
    loopShmHandle    :: Maybe (Ptr ()),
    loopMVar         :: MVar (Surface, Rectangle),
    loopTexture      :: Surface,
    loopDefaultImage :: Surface
  }

setupPipe :: IO Handle
setupPipe = do
  home <- getEnv "HOME"
  h <- connectTo "" (UnixSocket $ home ++ "/.MumbleOverlayPipe")
  hPutOverlayMsg h $ OverlayMsgInit 1920 1080
  return h

initShm :: FilePath -> IO (Ptr ())
initShm path = bracket
    (shmOpen path (ShmOpenFlags False False False False)
                  (unionFileModes ownerReadMode ownerWriteMode))
    closeFd
    (\fd -> c'mmap nullPtr size c'PROT_READ c'MAP_SHARED (fromIntegral fd) 0)
  where
    size = 1920*1080*4

deinitShm :: Ptr () -> IO ()
deinitShm ptr = c'munmap ptr size >> return ()
  where
    size = 1920*1080*4

fixWindowSize :: Window -> Int -> Int -> IO ()
fixWindowSize window w h =
    windowSetGeometryHints window (Nothing :: Maybe Widget)
           size size Nothing Nothing Nothing
  where
    size = Just (w, h)

getImageSurfaceSize :: Surface -> IO (Int, Int)
getImageSurfaceSize surf =
  liftM2 (,) (imageSurfaceGetWidth surf)
             (imageSurfaceGetHeight surf)

pipeLoop :: LoopState -> IO ()
pipeLoop state@LoopState { loopWindow = wnd,
                           loopPipeHandle = pipeHandle,
                           loopShmHandle = ptr,
                           loopMVar = ref,
                           loopTexture = tex,
                           loopDefaultImage = def } = do
    newState <- go `onException` maybe (return ())
                                       deinitShm
                                       (loopShmHandle state)
    pipeLoop newState
  where
    go = do
      emsg <- try $ hGetOverlayMsg pipeHandle
      case emsg of
        Right msg -> handleMsg msg
        Left e -> do
          hPutStrLn stderr (show (e :: IOException))
          hClose pipeHandle
          swapMVar ref (def, Rectangle 0 0 0 0)
          (w, h) <- getImageSurfaceSize def
          postGUIAsync $ do
            windowMove wnd (1920-w) (1080-h)
            fixWindowSize wnd w h
            widgetQueueDraw wnd
          reinit 0
    handleMsg msg = do
      print msg
      case msg of
        OverlayMsgShmem path -> do
          hPutOverlayMsg pipeHandle msg
          swapMVar ref (tex, Rectangle 0 0 0 0)
          postGUIAsync $ fixWindowSize wnd 0 0
          maybe (return ()) deinitShm $ loopShmHandle state
          newPtr <- initShm path
          return $ state { loopShmHandle = Just newPtr }
        OverlayMsgBlit x y w h -> do
          when (isJust ptr) $ do
            blit x y w h
            postGUIAsync $ widgetQueueDraw wnd
          return state
        OverlayMsgActive x y w h -> do
          swapMVar ref (tex, Rectangle x y w h)
          postGUIAsync $ do
            windowMove wnd x y
            fixWindowSize wnd w h
            widgetQueueDraw wnd
          return state
        _ -> return state
    blit x y w h = do
      stride <- imageSurfaceGetStride tex
      px <- imageSurfaceGetPixels tex
      let ptr32 = castPtr (fromJust ptr) :: Ptr Word32
      surfaceFlush tex
      (mw, mh) <- getImageSurfaceSize tex
      forM_ [y .. y+h-1] $ \y' ->
        forM_ [x .. x+w-1] $ \x' -> do
          pixel <- peekElemOff ptr32 (y' * mw + x')
          let offset = y' * (stride `div` 4) + x'
          writeArray px offset (pixel :: Word32)
      surfaceMarkDirty tex
    reinit n = do
      eh <- try setupPipe
      case eh of
        Left e -> do
          hPutStrLn stderr (show (e :: IOException))
          threadDelay (n * 1000000)
          reinit $ min 20 (succ n)
        Right newHandle -> return $ state { loopPipeHandle = newHandle }

main = do
    initGUI
    w <- windowNew
    windowSetTitle w "Overlay Test"
    onDestroy w mainQuit
    widgetSetAppPaintable w True
    windowSetDecorated w False
    widgetSetEvents w []
    widgetGetScreen w >>= setRGBAColorMap w
    on w screenChanged $ setRGBAColorMap w

    defaultImage <- drawDefaultImage
    (defaultWidth, defaultHeight) <- getImageSurfaceSize defaultImage
    fixWindowSize w defaultWidth defaultHeight
    windowMove w (1920 - defaultWidth) (1080 - defaultHeight)

    texture <- createImageSurface FormatARGB32 1920 1080
    ref <- newMVar (defaultImage, Rectangle 0 0 defaultWidth defaultHeight)
    h <- setupPipe
    forkIO $ pipeLoop LoopState {
      loopWindow = w,
      loopPipeHandle = h,
      loopShmHandle = Nothing,
      loopMVar = ref,
      loopTexture = texture,
      loopDefaultImage = defaultImage
    } `catch` \e -> postGUIAsync $ throwIO (e :: SomeException)

    onExposeRect w $ \_ -> do
      dw <- widgetGetDrawWindow w
      withMVar ref $ \(surf, Rectangle offX offY _ _) ->
        renderWithDrawable dw $ do
          translate (fromIntegral $ -offX) (fromIntegral $ -offY)
          setOperator OperatorSource
          setSourceSurface surf 0 0
          paint

    widgetShowAll w
    mainGUI
  where
    setRGBAColorMap window screen =
      screenGetRGBAColormap screen >>= \mc -> case mc of
        Just c  -> widgetSetColormap window c >> putStrLn "okay"
        Nothing -> hPutStrLn stderr "no rgba colormap"
