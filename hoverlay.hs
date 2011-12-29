{-# LANGUAGE ViewPatterns, ForeignFunctionInterface, BangPatterns #-}

import Control.Concurrent
import Control.Monad
import Control.Applicative
import System.IO
import System.Environment
import Data.Array.Base
import Data.Maybe
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

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
import System.Posix.Signals

import Network
import System.Glib.GObject
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Bindings.MMap

import Unsafe.Coerce

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

foreign import ccall unsafe "memcpy"
  c_memcpy :: Ptr a -> Ptr b -> CSize -> IO (Ptr ())

data SurfaceDataLookalike i e = SurfaceDataLookalike !Surface
                                                     {-# UNPACK #-} !(Ptr e)
                                                                    !(i,i)
                                                     {-# UNPACK #-} !Int

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
    blit x y w h = {-# SCC "blit" #-} do
      surfaceFlush tex
      px <- imageSurfaceGetPixels tex :: IO (SurfaceData Int Word32)
      !stride <- imageSurfaceGetStride tex
      let !ptr32 = castPtr (fromJust ptr) :: Ptr Word32
      (!mw, !mh) <- getImageSurfaceSize tex
      (_, succ -> pxSize) <- getBounds px
      let maxOffset = stride * (y+h)
      when (pxSize < maxOffset) $
        throwIO (userError "Active overlay area outside of texture bounds")
      -- hacky:
      let (SurfaceDataLookalike _ texPtr _ _) = unsafeCoerce px
      {-# SCC "loop-y" #-} loop y (y+h) $ \ !y' -> {-# SCC "loop-y-body" #-} do
        let !sourcePtr = (ptr32  `plusPtr` (y' * mw * 4))
            !destPtr   = (texPtr `plusPtr` (y' * stride))
            !count     = fromIntegral $ w * 4
        {-# SCC "memcpy" #-} c_memcpy destPtr sourcePtr count >> return ()
      unsafeRead px 0 -- to call touchForeignPtr
      -- slightly less hacky:
      --{-# SCC "loop-y" #-} loop y (y+h) $ \ !y' -> {-# SCC "loop-y-body" #-} do
      --  let !rowPtr    = ptr32 `plusPtr` (y' * mw * 4)
      --      !rowOffset = y' * stride `div` 4
      --  {-# SCC "loop-x" #-} loop x (x+w) $ \ !x' -> {-# SCC "loop-x-body" #-}  do
      --    !pixel <- {-# SCC "peekElemOff" #-} peekElemOff rowPtr x'
      --    let !offset = rowOffset + x'
      --    {-# SCC "unsafeWrite" #-} unsafeWrite px (rowOffset + x') pixel
      surfaceMarkDirty tex
    loop :: Int -> Int -> (Int -> IO ()) -> IO ()
    loop i max body = go i
      where
        go !i
          | i < max   = body i >> go (succ i)
          | otherwise = return ()
    reinit n = do
      eh <- try setupPipe
      case eh of
        Left e -> do
          hPutStrLn stderr (show (e :: IOException))
          threadDelay (n * 1000000)
          reinit $ min 20 (succ n)
        Right newHandle -> return $ state { loopPipeHandle = newHandle }

data CairoRegion
foreign import ccall unsafe
  cairo_region_create :: IO (Ptr CairoRegion)
foreign import ccall unsafe
  cairo_region_destroy :: Ptr CairoRegion -> IO ()

foreign import ccall unsafe
  gdk_window_input_shape_combine_region :: Ptr GObject -> Ptr CairoRegion -> CInt -> CInt -> IO ()
foreign import ccall unsafe
  gdk_window_set_override_redirect :: Ptr GObject -> CInt -> IO ()

drawWindowInputShapeCombineRegion :: DrawWindowClass self => self -> Ptr CairoRegion -> Int -> Int -> IO ()
drawWindowInputShapeCombineRegion self region offX offY =
    withForeignPtr (unGObject (toGObject self)) $ \ptrSelf ->
      gdk_window_input_shape_combine_region ptrSelf region cOffX cOffY
  where
    cOffX = fromIntegral offX
    cOffY = fromIntegral offY

drawWindowSetOverrideRedirect :: DrawWindowClass self => self -> Bool -> IO ()
drawWindowSetOverrideRedirect self bool =
    withForeignPtr (unGObject (toGObject self)) $ \ptrSelf ->
      gdk_window_set_override_redirect ptrSelf (fromIntegral $ fromEnum bool)

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

    onRealize w $ do
      dw <- widgetGetDrawWindow w

      drawWindowSetOverrideRedirect dw True
      bracket cairo_region_create cairo_region_destroy $ \region ->
        drawWindowInputShapeCombineRegion dw region 0 0

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

    installHandler sigINT (CatchOnce mainQuit) Nothing
    mainGUI
  where
    setRGBAColorMap window screen =
      screenGetRGBAColormap screen >>= \mc -> case mc of
        Just c  -> widgetSetColormap window c >> putStrLn "okay"
        Nothing -> hPutStrLn stderr "no rgba colormap"
