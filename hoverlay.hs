{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import System.IO
import System.Environment

import Control.Exception

import System.Posix.Signals

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import PipeListener
import GtkHelpers

main = do
    args <- getArgs
    pipePath <- case args of
      [path] -> return path
      _      -> do
        home <- getEnv "HOME"
        return $ home ++ "/.MumbleOverlayPipe"
    initGUI
    w <- windowNew
    windowSetTitle w "Mumble Overlay"
    onDestroy w mainQuit
    widgetSetAppPaintable w True
    windowSetDecorated w False
    widgetSetEvents w []
    widgetGetScreen w >>= setRGBAColorMap w
    on w screenChanged $ setRGBAColorMap w

    onRealize w $ do
      dw <- widgetGetDrawWindow w

      drawWindowSetOverrideRedirect dw True
      withCairoRegion $ \region ->
        drawWindowInputShapeCombineRegion dw region 0 0

    forkPipeListener pipePath w

    widgetShowAll w

    installHandler sigINT (CatchOnce mainQuit) Nothing
    mainGUI
  where
    setRGBAColorMap window screen =
      screenGetRGBAColormap screen >>= \mc -> case mc of
        Just c  -> widgetSetColormap window c
        Nothing -> hPutStrLn stderr "no rgba colormap"
