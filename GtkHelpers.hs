{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module GtkHelpers (
  drawWindowInputShapeCombineRegion,
  drawWindowSetOverrideRedirect,
  withCairoRegion
) where

import Control.Exception

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

import Graphics.UI.Gtk
import System.Glib.GObject

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

withCairoRegion = bracket cairo_region_create cairo_region_destroy

drawWindowSetOverrideRedirect :: DrawWindowClass self => self -> Bool -> IO ()
drawWindowSetOverrideRedirect self bool =
    withForeignPtr (unGObject (toGObject self)) $ \ptrSelf ->
      gdk_window_set_override_redirect ptrSelf (fromIntegral $ fromEnum bool)
