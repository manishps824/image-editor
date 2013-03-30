module Effects where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Filters.GD
import Graphics.GD
import Data.IORef
import System.FilePath.Posix
import System.Directory -- for doesFileExist
import HelperFunctions
-- add function signature here 
zoomInOut zoomAmount tmpFileName canvas factor = 
  do         
    zAmnt <- readIORef zoomAmount -- read the zoom amount
    writeIORef zoomAmount (zAmnt+factor) -- increment by one and write it
    zAmnt <- readIORef zoomAmount
    putStrLn $ show zAmnt
    tmpFile <- readIORef tmpFileName -- read the temp file location
    pix <- pixbufNewFromFile tmpFile -- get pixbuf from the temp file
    w <- pixbufGetWidth pix
    h <- pixbufGetHeight pix
    putStrLn $ show $ truncate $ (1.1**(fromIntegral zAmnt))* fromIntegral w -- for debugging
    pix <- pixbufScaleSimple pix (truncate $ (1.1**(fromIntegral zAmnt))*(fromIntegral w))(truncate $ (1.1**(fromIntegral zAmnt))*(fromIntegral h)) InterpBilinear -- exponent takes care of zoom-in/out
    imageSetFromPixbuf canvas pix -- finally set the scaled pixbuf to canvas

-- add function signature here
rotateA tmpFileName canvas factor = 
  do   
    tmpFile <- readIORef tmpFileName
    image <- loadImgFile tmpFile
    newImage<-rotateImage (45*factor) image
    saveImgFile (-1) tmpFile newImage
    imageSetFromFile canvas tmpFile

noArgEffect f changeList tmpFileName canvas = 
  do
    opList <- readIORef changeList 
    writeIORef changeList (opList++[f]) -- add f to the list of effects applied so far
    do
      tmpFile <- readIORef tmpFileName
      myimg <- loadImgFile tmpFile -- load temp image 
      f myimg -- APPLY EFFECT
      saveImgFile (-1) tmpFile myimg -- save back to temp image
      imageSetFromFile canvas tmpFile -- set new temp image on canvas
