module HelperFunctions where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Filters.GD
import Graphics.GD
import Data.IORef
import System.FilePath.Posix
import System.Directory -- for doesFileExist

data FileList a = FileList [a] [a] deriving (Show)

-- goForward moves forward on a FileList 
goForward :: FileList String -> FileList String
goForward (FileList [] ys) = FileList [] ys
goForward (FileList [x] ys) = FileList [x] ys 
goForward (FileList (x:xs) ys) = FileList xs (x:ys)


--goBackwards moves backwards on a FileList
goBackward :: FileList String -> FileList String
goBackward (FileList xs []) = FileList xs []
--goBackward (FileList xs [y]) = FileList [] (reverse (y:xs))
goBackward (FileList xs (y:ys)) = FileList (y:xs) ys 

toFileList :: [String] -> FileList String --makes a FileList from a simple list of files
toFileList xs = FileList xs []  

createFileList :: FilePath -> IO (FileList String)
createFileList dir = do
                   fl <- getDirectoryContents dir
                   filelst <- return (map ((dir++"/")++) fl)
                   let imageList = filter isImage filelst in
                     return $ toFileList imageList
                     
getFrontFile :: FileList String -> String                     
getFrontFile (FileList [] ys) = "" 
getFrontFile (FileList (x:xs) _) = x
                   
--getFrontFileList2 :: FileList String -> String                                        
--getFrontFileList2 (FileList _ (y:ys)) = y
                                        
isImage :: FilePath -> Bool
isImage x = (takeExtension x) `elem` [".jpg",".png",".jpeg"]


adjustFileList :: (FileList String) -> FilePath -> (FileList String) 
adjustFileList (FileList (x:xs) b) currentFile = if x == currentFile then
                                                   (FileList (x:xs) b)
                                                 else (adjustFileList (FileList xs (x:b)) currentFile)

adjustFileList (FileList [] ys) currentFile = (FileList [] ys)

undoLast :: [Graphics.GD.Image -> IO()] -> IO Graphics.GD.Image -> IO Graphics.GD.Image
undoLast [] img = img
undoLast [a] img = img
undoLast (x:xs) img = do
  tmpimg <- img
  x tmpimg
  undoLast xs (return tmpimg)
------------------------------------------------------------------------

loadImgFile :: FilePath -> IO Graphics.GD.Image
loadImgFile fpath = do
  ext <- return (takeExtension fpath)
  case ext of
    ".jpeg" -> loadJpegFile fpath
    ".jpg" -> loadJpegFile fpath
    ".png" -> loadPngFile fpath
    ".gif" -> loadGifFile fpath
------------------------------------------------------------------------

cropRect :: Graphics.GD.Image -> (Int, Int) -> (Int, Int) -> IO ()
cropRect img (a,b) (c,d) = do
  Graphics.GD.drawLine (a,b) (c,b) (rgb 254 254 254) img
  Graphics.GD.drawLine (c,b) (c,d) (rgb 254 254 254) img
  Graphics.GD.drawLine (a,d) (c,d) (rgb 254 254 254) img
  Graphics.GD.drawLine (a,b) (a,d) (rgb 254 254 254) img
------------------------------------------------------------------------    

crop :: FilePath -> (Int, Int) -> (Int, Int) -> Graphics.GD.Image -> IO ()

crop tmpFile (a,b) (c,d) img = do
  newimg <- newImage ((c-a),(d-b))
  copyRegion (a,b)  ((c-a),(d-b)) img (0,0) newimg
  finalImg <- resizeImage (c-a) (d-b) newimg
  img <- return finalImg
  saveImgFile (-1) tmpFile finalImg
  putStrLn "hello"
------------------------------------------------------------------------

-- newCrop :: Graphics.GD.Image -> (FilePath, Graphics.GD.Point, Graphics.GD.Point) -> IO ()
-- newCrop (tmpFile, p1, p2) img = do
--   img1 <- crop img p1 p2
--   saveImgFile (-1) tmpFile img1
------------------------------------------------------------------------

saveImgFile :: Int -> FilePath -> Graphics.GD.Image -> IO ()    
saveImgFile quality fpath image = do
  ext <- return (takeExtension fpath)
  case ext of
    ".jpeg" -> saveJpegFile quality fpath image
    ".jpg" -> saveJpegFile quality fpath image
    ".png" -> savePngFile fpath image
    ".gif" -> saveGifFile fpath image
------------------------------------------------------------------------

okAction :: WidgetClass self => IORef FilePath -> IORef FilePath -> self -> IO ()    
okAction tmpFileName tmpFileName1 bwindow = do
  tmpFile1 <- readIORef tmpFileName1
  tmpFile <- readIORef tmpFileName
  myimg <- loadImgFile tmpFile1
  saveImgFile (-1) tmpFile myimg
  val <- doesFileExist tmpFile1
  if (val==True) then removeFile tmpFile1 else putStrLn "No file Present" 
  --if (val==True) then removeFile tmpFile else putStrLn "No file Present" 
  widgetDestroy bwindow
------------------------------------------------------------------------

cancelAction :: WidgetClass self => IORef FilePath -> IORef FilePath -> self -> Graphics.UI.Gtk.Image -> IO ()
cancelAction tmpFileName tmpFileName1 bwindow canvas = do
  tmpFile1 <-readIORef tmpFileName1
  tmpFile <- readIORef tmpFileName
  val <- doesFileExist tmpFile1
  if (val==True) then removeFile tmpFile1 else putStrLn "No file Present" 
  imageSetFromFile canvas tmpFile
  widgetDestroy bwindow
------------------------------------------------------------------------

prAct :: ActionClass self => self -> IO (ConnectId self)  
prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)  
------------------------------------------------------------------------

printFileList :: FileList String -> IO ()
printFileList (FileList a b) = do
                                  putStrLn "LIST 1"
                                  mapM putStrLn a
                                  putStrLn "LIST 2"
                                  mapM putStrLn b
                                  putStrLn "-----------------"


openAction :: IORef FilePath -> IORef [Char] -> IORef [Char] -> Graphics.UI.Gtk.Image -> IORef (FileList String) -> IO ()
openAction fileName tmpFileName tmpFileName1 canvas myFileList = 
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
            basename <- return (takeBaseName fpath)
            putStrLn ("BASENAME"++basename)
            -----------------------------------------------------
            directory <- return $ takeDirectory fpath
            tempfileList <- createFileList directory
            printFileList tempfileList
            fileList <- return $ adjustFileList tempfileList fpath
            writeIORef myFileList fileList
            printFileList fileList
            -----------------------------------------------------
            myimg <- loadImgFile fpath  -- load image from tI don't know the reason why it failed, but my guess is that the cohis location 
            ext <- return (takeExtension fpath)
            saveImgFile (-1) (basename++"temp"++ext) myimg -- save temp file in code dir for future use
            writeIORef tmpFileName (basename++"temp"++ext) -- remember temp file's name
            writeIORef tmpFileName1 (basename++"temp1"++ext) -- remember temp file's name
            imageSetFromFile canvas (basename++"temp"++ext) -- render the image from temp file on canvas
            putStrLn $ "Opening File: " ++ fpath
          Nothing -> putStrLn "error: No file was selected"
    widgetDestroy fcd
