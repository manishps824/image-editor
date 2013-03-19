module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Filters.GD
import Graphics.GD
import System.FilePath.Posix
import Data.IORef
import System.Directory -- for doesFileExist
main = do
  initGUI
  Just xml <- xmlNew "editor.glade"
  window <- xmlGetWidget xml castToWindow "window1" -- this is the main window
  set window [windowTitle := "Image Editor",windowDefaultWidth := 1300,windowDefaultHeight := 600]
  button1 <- xmlGetWidget xml castToButton "button1"
  button2 <- xmlGetWidget xml castToButton "button2"
  button3 <- xmlGetWidget xml castToButton "button3"
  button4 <- xmlGetWidget xml castToButton "button4"
  canvas <- xmlGetWidget xml castToImage "image1"
  menubox <- xmlGetWidget xml castToVBox "vbox3"
  scrolledwindow1 <- xmlGetWidget xml castToScrolledWindow "scrolledwindow1"
  
  fma <- actionNew "FMA" "File" Nothing Nothing
  ema <- actionNew "EMA" "Edit" Nothing Nothing
  hma <- actionNew "HMA" "Help" Nothing Nothing
  newa <- actionNew "NEWA" "New"     (Just "New Image") (Just stockNew)
  opna <- actionNew "OPNA" "Open"    (Just "Open Image") (Just stockOpen)
  sava <- actionNew "SAVA" "Save"    (Just "Save") (Just stockSave)
  svaa <- actionNew "SVAA" "Save As" (Just "Save As") (Just stockSaveAs)
  exia <- actionNew "EXIA" "Exit"    (Just "Exit") (Just stockQuit)
  cuta <- actionNew "CUTA" "Cut"   (Just "Cut") (Just stockCut)    
  copa <- actionNew "COPA" "Copy"  (Just "Copy") (Just stockCopy)
  psta <- actionNew "PSTA" "Paste" (Just "Paste") (Just stockPaste)
  hlpa <- actionNew "HLPA" "Help"  (Just "help") (Just stockHelp)
  unda <- actionNew "UNDA" "Undo" (Just "Undo") (Just stockUndo)
  zina <- actionNew "ZINA" "Zoom In" (Just "Zoom In") (Just stockZoomIn)
  zoua <- actionNew "ZOUA" "Zoom Out" (Just "Zoom Out") (Just stockZoomOut)
  
  --create an action group with name AGR
  --actionGroupNew :: String -> IO ActionGroup
  agr <- actionGroupNew "AGR"
  -- add the actions to a group using actionGroupAddAction
  -- actionGroupAddAction :: ActionClass action => ActionGroup -> action -> IO ()
  -- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
  mapM_ (actionGroupAddAction agr) [fma, ema, hma]
  -- set no shortcut keys for all except exit
  mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing) [newa,opna,sava,svaa,cuta,copa,psta,hlpa,unda,zina,zoua]
  -- The shortcut keys do not work
  actionGroupAddActionWithAccel agr exia (Just "<Control>e")
  
  ui <- uiManagerNew
  uiManagerAddUiFromString ui uiDecl
  uiManagerInsertActionGroup ui agr 0
     --extract  the elements from the xml and play
  maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
  let menubar = case maybeMenubar of
        (Just x) -> x
        Nothing -> error "Cannot get menubar from string." 
  boxPackStart menubox menubar PackNatural 0
  maybeToolbar <- uiManagerGetWidget ui "/ui/toolbar"
  let toolbar = case maybeToolbar of
        (Just x) -> x
        Nothing -> error "Cannot get toolbar from string." 
  boxPackStart menubox toolbar PackNatural 0
  --actionSetSensitive cuta False
  onActionActivate exia (widgetDestroy window)
     --define the action handler for each action
     --right now it is same for each so using mapM_
  mapM_ prAct [fma,ema,hma,newa,sava,svaa,cuta,copa,psta,hlpa,unda,zina,zoua] -- add any new button for menubar here for rendering
  
  expand <- newIORef True
  changeList <- newIORef []
  fileName <- newIORef ""
  tmpFileName <- newIORef ""
------------------------------------------------------------------------------------------
  onActionActivate opna $ do
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
            myimg <- loadJpegFile fpath  -- load image from this location 
            basename <- return (takeBaseName fpath)
            saveJpegFile (-1) (basename++"temp"++".jpeg") myimg -- save temp file in code dir for future use
            writeIORef tmpFileName (basename++"temp"++".jpeg") -- remember temp file's name
            imageSetFromFile canvas (basename++"temp"++".jpeg") -- render the image from temp file on canvas
            putStrLn $ "Opening File: " ++ fpath
          Nothing -> putStrLn "error: No file was selected"
    widgetDestroy fcd
    
--------------------------------------------------------------------------------------------------
  
  onActionActivate unda $ do    -- user pressed undo button
    effectList <- readIORef changeList
    originalPath <- readIORef fileName
    tmpPath <- readIORef tmpFileName
    newImg <- undoLast effectList (loadJpegFile originalPath) -- function that will apply all the effects present in the list again
    saveJpegFile (-1) tmpPath newImg
    imageSetFromFile canvas tmpPath
  
  
-------------------------------------------------------------------------------------------  
  {--
  onClicked button2 $ do
    exp <- readIORef expand
    case exp of
         True -> do
           path <- get canvas imageFile  --find the filename
           basename <- return (takeBaseName path) --take the name only not extension
           writeIORef expand False -- toggle
           pix <- pixbufNewFromFileAtScale path 1000 1000 True 
           pixbufSave pix (basename++".jpeg") "jpeg" [] 
           --imageSetFromPixbuf canvas pix
           imageSetFromFile canvas (basename++".jpeg")
           putStrLn "DONE"
         False -> do
           --here you need to ensure that all effects applied till that point are reapplied
           originalFile <- readIORef fileName
           writeIORef expand True
           imageSetFromFile canvas originalFile
 --}
--------------------------------------------------------------------- 
  onActionActivate zina $ do         
    ---initpath <- get canvas imageFile
    pix <- imageGetPixbuf canvas
    w <- pixbufGetWidth pix
    h <- pixbufGetHeight pix
    putStrLn $ show $ truncate $ 1.1* fromIntegral w
    pix <- pixbufScaleSimple pix (truncate $ 1.1 * (fromIntegral w)) (truncate $ 1.1 * (fromIntegral h)) InterpBilinear
    imageSetFromPixbuf canvas pix
    
---------------------------------------------------------------------            
  onActionActivate zoua $ do         
    ---initpath <- get canvas imageFile
    pix <- imageGetPixbuf canvas
    w <- pixbufGetWidth pix
    h <- pixbufGetHeight pix
    putStrLn $ show $ truncate $ 0.9 * fromIntegral w
    pix <- pixbufScaleSimple pix (truncate $ 0.9 * (fromIntegral w)) (truncate $ 0.9 * (fromIntegral h)) InterpBilinear
    imageSetFromPixbuf canvas pix  

  onClicked button1 $ do
    opList <- readIORef changeList 
    writeIORef changeList (opList++[grayscale])
    do
      tmpFile <- readIORef tmpFileName
      --writeIORef tmpFileName tmpFile
      myimg <- loadJpegFile tmpFile -- load image from this location 
      grayscale myimg -- APPLY EFFECT
      saveJpegFile (-1) tmpFile myimg
      imageSetFromFile canvas tmpFile
   
    



{--
    fch <- fileChooserWidgetNew FileChooserActionOpen
    widgetShow fch
    img <- imageNew
    fileChooserSetPreviewWidget fch img
    
    onUpdatePreview fch $ do
      file <- fileChooserGetPreviewFilename fch
      case file of 
        Nothing -> putStrLn "No file Secleted"
        Just fpath -> imageSetFromFile img fpath
        
    onFileActivated fch $ do
      dir <- fileChooserGetCurrentFolder fch
      case dir of 
        Just dpath -> putStrLn ("curr dir: "++ dpath)
        Nothing -> putStrLn ("NOthing")
      file <- fileChooserGetFilename fch
      case file of
        Just fpath -> putStrLn ("you selected;" ++ fpath)
        Nothing -> putStrLn "Nothing"
    widgetDestroy fch    
    --}
    
  widgetShowAll window  
  onDestroy window mainQuit
  mainGUI
  
     
uiDecl=  "<ui>\
\           <menubar>\
\            <menu action=\"FMA\">\
\              <menuitem action=\"NEWA\" />\
\              <menuitem action=\"OPNA\" />\
\              <menuitem action=\"SAVA\" />\
\              <menuitem action=\"SVAA\" />\
\              <separator />\
\              <menuitem action=\"EXIA\" />\
\            </menu>\
\           <menu action=\"EMA\">\
\              <menuitem action=\"CUTA\" />\
\              <menuitem action=\"COPA\" />\
\              <menuitem action=\"PSTA\" />\
\              <menuitem action=\"UNDA\" />\
\              <menuitem action=\"ZINA\" />\
\              <menuitem action=\"ZOUA\" />\
\           </menu>\
\            <separator />\
\            <menu action=\"HMA\">\
\              <menuitem action=\"HLPA\" />\
\            </menu>\
\           </menubar>\
\           <toolbar>\
\            <toolitem action=\"NEWA\" />\
\            <toolitem action=\"OPNA\" />\
\            <toolitem action=\"SAVA\" />\
\            <toolitem action=\"EXIA\" />\
\            <separator />\
\            <toolitem action=\"CUTA\" />\
\            <toolitem action=\"COPA\" />\
\            <toolitem action=\"PSTA\" />\
\            <toolitem action=\"UNDA\" />\
\            <toolitem action=\"ZINA\" />\
\            <toolitem action=\"ZOUA\" />\
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\           </toolbar>\
\          </ui>"

prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)  
                                  
undoLast :: [Graphics.GD.Image -> IO()] -> IO Graphics.GD.Image -> IO Graphics.GD.Image
undoLast [] img = img
undoLast [a] img = img
undoLast (x:xs) img = do
  tmpimg <- img
  x tmpimg
  undoLast xs (return tmpimg)
