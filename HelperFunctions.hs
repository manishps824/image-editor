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
------------------------------------------------------------------------

loadImgFile fpath = do
  ext <- return (takeExtension fpath)
  case ext of
    ".jpeg" -> loadJpegFile fpath
    ".jpg" -> loadJpegFile fpath
    ".png" -> loadPngFile fpath
    ".gif" -> loadGifFile fpath
------------------------------------------------------------------------
    
saveImgFile quality fpath image = do
  ext <- return (takeExtension fpath)
  case ext of
    ".jpeg" -> saveJpegFile quality fpath image
    ".jpg" -> saveJpegFile quality fpath image
    ".png" -> savePngFile fpath image
    ".gif" -> saveGifFile fpath image
------------------------------------------------------------------------
    
okAction tmpFileName tmpFileName1 bwindow = do
  tmpFile1 <- readIORef tmpFileName1
  tmpFile <- readIORef tmpFileName
  myimg <- loadImgFile tmpFile
  saveImgFile (-1) tmpFile myimg
  removeFile tmpFile1
  widgetDestroy bwindow
------------------------------------------------------------------------

cancelAction tmpFileName tmpFileName1 bwindow canvas = do
  tmpFile1 <-readIORef tmpFileName1
  tmpFile <- readIORef tmpFileName
  removeFile tmpFile1
  imageSetFromFile canvas tmpFile
  widgetDestroy bwindow
------------------------------------------------------------------------
  
prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)  
------------------------------------------------------------------------

openAction fileName tmpFileName tmpFileName1 canvas = 
  do
    fcd <- fileChooserDialogNew (Just "Open File") Nothing FileChooserActionSave [("Cancel", ResponseCancel),("Open", ResponseAccept)] -- create a file chooser dialog
    widgetShow fcd
    response <- dialogRun fcd
    case response of
      ResponseCancel -> putStrLn "Cancel" -- user pressed Cancel
      ResponseAccept -> do 
        file <- fileChooserGetFilename fcd -- get filename of the file that has been selected
        case file of 
          Just fpath -> do
            writeIORef fileName fpath -- save original file location
            myimg <- loadImgFile fpath  -- load image from this location 
            basename <- return (takeBaseName fpath)
            saveImgFile (-1) (basename++"temp"++".jpeg") myimg -- save temp file in code dir for future use
            writeIORef tmpFileName (basename++"temp"++".jpeg") -- remember temp file's name
            writeIORef tmpFileName1 (basename++"temp1"++".jpeg") -- remember temp file's name
            imageSetFromFile canvas (basename++"temp"++".jpeg") -- render the image from temp file on canvas
            putStrLn $ "Opening File: " ++ fpath
          Nothing -> putStrLn "error: No file was selected"
    widgetDestroy fcd
