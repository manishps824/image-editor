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
  --create an action group with name AGR
  --actionGroupNew :: String -> IO ActionGroup
  agr <- actionGroupNew "AGR"
  -- add the actions to a group using actionGroupAddAction
  -- actionGroupAddAction :: ActionClass action => ActionGroup -> action -> IO ()
  -- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
  mapM_ (actionGroupAddAction agr) [fma, ema, hma]
  -- set no shortcut keys for all except exit
  mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing) [newa,opna,sava,svaa,cuta,copa,psta,hlpa,unda]
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
  mapM_ prAct [fma,ema,hma,newa,sava,svaa,cuta,copa,psta,hlpa]
  
  expand <- newIORef True
  changeList <- newIORef []
  fileName <- newIORef ""
  
------------------------------------------------------------------------------------------
  onActionActivate opna $ do
    fcd <- fileChooserDialogNew (Just "Open File") Nothing FileChooserActionSave [("Cancel", ResponseCancel),("Open", ResponseAccept)]
    widgetShow fcd
    response <- dialogRun fcd
    case response of
      ResponseCancel -> putStrLn "Cancel" -- user pressed Cancel
      ResponseAccept -> do 
        file <- fileChooserGetFilename fcd -- get filename of the file that has been selected
        case file of 
          Just fpath -> do
            writeIORef fileName fpath
            imageSetFromFile canvas fpath -- render the image from fpath on the canvas object
            putStrLn $ "Opening File: " ++ fpath
          Nothing -> putStrLn "error: No file was selected"
    widgetDestroy fcd
    
--------------------------------------------------------------------------------------------------
  --temporarily model cut as undo
  onActionActivate unda $ do    
    effectList <- readIORef changeList
    originalPath <- readIORef fileName
    path <- get canvas imageFile
    newimg <- applyEffects effectList (loadJpegFile originalPath) -- function that will apply all the effects present in the list again
    saveJpegFile (-1) path newimg
    imageSetFromFile canvas path
  
  
-------------------------------------------------------------------------------------------  
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
 
--------------------------------------------------------------------- 
  onClicked button1 $ do
    opList <- readIORef changeList 
    writeIORef changeList (opList++[grayscale])
    initpath <- get canvas imageFile
    basename <- return (takeBaseName initpath)
    do
      f <- readIORef fileName
      putStrLn $ "Intial file Name:" ++ f
      putStrLn $ initpath
      putStrLn $ "BOOM " ++ basename
      rv <- doesFileExist ("./"++basename++".jpeg") 
      case rv of
        True -> do 
          putStrLn "temp file exists! No need for another"
          myimg <- loadJpegFile ("./"++basename++".jpeg") -- load image from this location 
          grayscale myimg -- APPLY EFFECT
          saveJpegFile (-1) ("./"++basename++".jpeg") myimg
          imageSetFromFile canvas ("./"++basename++".jpeg")
        False -> do        
          putStrLn "temp file created!"
          myimg <- loadJpegFile initpath -- load image from its original location as a GD image
          saveJpegFile (-1) ("./"++basename++".jpeg") myimg -- save a local temp copy at a predefined location
          grayscale myimg -- APPLY EFFECT
          saveJpegFile (-1) ("./"++basename++".jpeg") myimg
          imageSetFromFile canvas ("./"++basename++".jpeg")
   
    



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
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\           </toolbar>\
\          </ui>"

prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)  
                                  
applyEffects :: [Graphics.GD.Image -> IO()] -> IO Graphics.GD.Image -> IO Graphics.GD.Image
applyEffects [a] img = img
applyEffects (x:xs) img = do
  tmpimg <- img
  x tmpimg
  applyEffects xs (return tmpimg)
