{-# LANGUAGE ViewPatterns, ForeignFunctionInterface, BangPatterns #-}

module PipeListener (
  forkPipeListener
) where

import Control.Monad
import Control.Concurrent
import Control.Exception
import System.IO
import Data.Word
import Data.Maybe
import Data.Array.Base

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import System.Posix.IO
import System.Posix.SharedMem
import System.Posix.Files

import Network

import Bindings.MMap

import Foreign.Ptr
import Foreign.C.Types

import Unsafe.Coerce

import Prelude hiding (catch)

import OverlayMsg

forkPipeListener :: String -> Window -> IO ThreadId
forkPipeListener pipePath w = do
  defaultImage <- drawDefaultImage
  (defaultWidth, defaultHeight) <- getImageSurfaceSize defaultImage
  fixWindowSize w defaultWidth defaultHeight
  windowMove w (myWidth - defaultWidth) (myHeight - defaultHeight)

  texture <- createImageSurface FormatARGB32 myWidth myHeight
  ref <- newMVar (defaultImage, Rectangle 0 0 defaultWidth defaultHeight)

  onExposeRect w $ \_ -> do
    dw <- widgetGetDrawWindow w
    withMVar ref $ \(surf, Rectangle offX offY _ _) ->
      renderWithDrawable dw $ do
        translate (fromIntegral $ -offX) (fromIntegral $ -offY)
        setOperator OperatorSource
        setSourceSurface surf 0 0
        paint

  h <- setupPipe pipePath
  forkIO $ pipeLoop LoopState {
    loopWindow = w,
    loopPipePath = pipePath,
    loopPipeHandle = h,
    loopShmHandle = Nothing,
    loopMVar = ref,
    loopTexture = texture,
    loopDefaultImage = defaultImage
  } `catch` \e -> postGUIAsync $ throwIO (e :: SomeException)

myWidth, myHeight :: Num a => a
myWidth  = 1920
myHeight = 1080

drawDefaultImage :: IO Surface
drawDefaultImage = do
  let r = 6
      s = 0
      w = 288
      h = 32
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
    setSourceRGBA 0 0 0 0.7
    fillPreserve
    setSourceRGBA 0 0 0 1
    setLineWidth (s*2)
    stroke
    moveTo (r * 2 ) (h * 0.75)
    setSourceRGBA 1 1 1 1
    setFontSize 20
    showText "No connection to Mumble"
  return surf

data LoopState = LoopState {
    loopWindow       :: Window,
    loopPipePath     :: String,
    loopPipeHandle   :: Handle,
    loopShmHandle    :: Maybe (Ptr ()),
    loopMVar         :: MVar (Surface, Rectangle),
    loopTexture      :: Surface,
    loopDefaultImage :: Surface
  }

setupPipe :: String -> IO Handle
setupPipe path = do
  h <- connectTo "" (UnixSocket path)
  hPutOverlayMsg h $ OverlayMsgInit myWidth myHeight
  return h

initShm :: FilePath -> IO (Ptr ())
initShm path = bracket
    (shmOpen path (ShmOpenFlags False False False False)
                  (unionFileModes ownerReadMode ownerWriteMode))
    closeFd
    (\fd -> c'mmap nullPtr size c'PROT_READ c'MAP_SHARED (fromIntegral fd) 0)
  where
    size = myWidth*myHeight*4

deinitShm :: Ptr () -> IO ()
deinitShm ptr = c'munmap ptr size >> return ()
  where
    size = myWidth*myHeight*4

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

loop :: Int -> Int -> (Int -> IO ()) -> IO ()
loop first end body = go first
  where
    go !i
      | i < end   = body i >> go (succ i)
      | otherwise = return ()

pipeLoop :: LoopState -> IO ()
pipeLoop state@LoopState { loopWindow = wnd,
                           loopPipePath   = pipePath,
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
          hPrint stderr (e :: IOException)
          hClose pipeHandle
          swapMVar ref (def, Rectangle 0 0 0 0)
          (w, h) <- getImageSurfaceSize def
          postGUIAsync $ do
            windowMove wnd (myWidth-w) (myHeight-h)
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
      surfaceFlush tex
      px <- imageSurfaceGetPixels tex :: IO (SurfaceData Int Word32)
      !stride <- imageSurfaceGetStride tex
      let !ptr32 = castPtr (fromJust ptr) :: Ptr Word32
      !mw <- imageSurfaceGetWidth tex
      (_, succ -> pxSize) <- getBounds px
      let maxOffset = stride * (y+h)
      when (pxSize < maxOffset) $
        throwIO (userError "Active overlay area outside of texture bounds")
      -- hacky:
      let (SurfaceDataLookalike _ texPtr _ _) = unsafeCoerce px
      loop y (y+h) $ \ !y' -> do
        let !sourcePtr = ptr32  `plusPtr` ((y' * mw + x) * 4)
            !destPtr   = texPtr `plusPtr` (y' * stride)
            !count     = fromIntegral $ w * 4
        c_memcpy destPtr sourcePtr count >> return ()
      unsafeRead px 0 -- to call touchForeignPtr
      -- slightly less hacky:
      -- loop y (y+h) $ \ !y' -> do
      --  let !rowPtr    = ptr32 `plusPtr` (y' * mw * 4)
      --      !rowOffset = y' * stride `div` 4
      --  loop x (x+w) $ \ !x' -> do
      --    !pixel <- peekElemOff rowPtr x'
      --    let !offset = rowOffset + x'
      --    unsafeWrite px (rowOffset + x') pixel
      surfaceMarkDirty tex
    reinit n = do
      eh <- try $ setupPipe pipePath
      case eh of
        Left e -> do
          hPrint stderr (e :: IOException)
          threadDelay (n * 1000000)
          reinit $ min 20 (succ n)
        Right newHandle -> return $ state { loopPipeHandle = newHandle }
