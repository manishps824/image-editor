module HelperFunctions where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Filters.GD
import Graphics.GD
import Data.IORef
import System.FilePath.Posix
import System.Directory -- for doesFileExist

undoLast :: [Graphics.GD.Image -> IO()] -> IO Graphics.GD.Image -> IO Graphics.GD.Image
undoLast [] img = img
undoLast [a] img = img
undoLast (x:xs) img = do
  tmpimg <- img
  x tmpimg
  undoLast xs (return tmpimg)

loadImgFile fpath = do
  ext <- return (takeExtension fpath)
  case ext of
    ".jpeg" -> loadJpegFile fpath
    ".jpg" -> loadJpegFile fpath
    
saveImgFile quality fpath image = do
  ext <- return (takeExtension fpath)
  case ext of
    ".jpeg" -> saveJpegFile quality fpath image
    ".jpg" -> saveJpegFile quality fpath image
    
okAction tmpFileName tmpFileName1 bwindow = do
  tmpFile1 <- readIORef tmpFileName1
  tmpFile <- readIORef tmpFileName
  myimg <- loadImgFile tmpFile
  saveImgFile (-1) tmpFile myimg
  removeFile tmpFile1
  widgetDestroy bwindow

cancelAction tmpFileName tmpFileName1 bwindow canvas = do
  tmpFile1 <-readIORef tmpFileName1
  tmpFile <- readIORef tmpFileName
  removeFile tmpFile1
  imageSetFromFile canvas tmpFile
  widgetDestroy bwindow