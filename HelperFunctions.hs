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

printFileList (FileList a b) = do
                                  putStrLn "LIST 1"
                                  mapM putStrLn a
                                  putStrLn "LIST 2"
                                  mapM putStrLn b
                                  putStrLn "-----------------"


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
            saveImgFile (-1) (basename++"temp"++".jpeg") myimg -- save temp file in code dir for future use
            writeIORef tmpFileName (basename++"temp"++".jpeg") -- remember temp file's name
            writeIORef tmpFileName1 (basename++"temp1"++".jpeg") -- remember temp file's name
            imageSetFromFile canvas (basename++"temp"++".jpeg") -- render the image from temp file on canvas
            putStrLn $ "Opening File: " ++ fpath
          Nothing -> putStrLn "error: No file was selected"
    widgetDestroy fcd
