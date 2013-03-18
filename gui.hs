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
  --create an action group with name AGR
  --actionGroupNew :: String -> IO ActionGroup
  agr <- actionGroupNew "AGR"
  -- add the actions to a group using actionGroupAddAction
  -- actionGroupAddAction :: ActionClass action => ActionGroup -> action -> IO ()
  -- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
  mapM_ (actionGroupAddAction agr) [fma, ema, hma]
  -- set no shortcut keys for all except exit
  mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing) [newa,opna,sava,svaa,cuta,copa,psta,hlpa]
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
  actionSetSensitive cuta False
  onActionActivate exia (widgetDestroy window)
     --define the action handler for each action
     --right now it is same for each so using mapM_
<<<<<<< HEAD:gui.hs
  mapM_ prAct [fma,ema,hma,newa,sava,svaa,cuta,copa,psta]
=======
  mapM_ prAct [fma,ema,hma,newa,sava,svaa,cuta,copa,psta,hlpa]
  
  expand <- newIORef True
>>>>>>> 958282a60a69b4116147008dcbe0d6ced59e26a1:resizeAdded.hs
  -------------------------------------------------------------------------------------------
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
            imageSetFromFile canvas fpath -- render the image from fpath on the canvas object
            putStrLn $ "Opening File: " ++ fpath
          Nothing -> putStrLn "error: No file was selected"
    widgetDestroy fcd
  -------------------------------------------------------------------------------------------  
<<<<<<< HEAD:gui.hs
  onActionActivate sava $ do
    fcd <- fileChooserDialogNew (Just "Save File") Nothing FileChooserActionSave [("Cancel", ResponseCancel),("Save", ResponseAccept)]
    widgetShow fcd
    response <- dialogRun fcd
    case response of
      ResponseCancel -> putStrLn "Cancel" -- user pressed Cancel
      ResponseAccept -> do 
        file <- fileChooserGetFilename fcd -- get filename of the file that has been selected
        case file of 
          Just fpath -> do
            putStrLn $ "Saving File: " ++ fpath
            -- call save function here type sig shld be :: filepath -> IO ()
            -- it should save the image in the temp file at the location !!!
          Nothing -> putStrLn "error: No file was selected"
    widgetDestroy fcd
  --------------------------------------------------------------------------------------------  
  onActionActivate hlpa $ do
    dia <- dialogNew
    set dia [windowTitle := "Image-Editor 1.0 Help",windowDefaultWidth := 600, windowDefaultHeight := 600]
    upbox <- dialogGetUpper dia
    (dlabel,dframe)<- myLabelWithFrameNew
    boxPackStart upbox dframe PackNatural 0
    labelSetText dlabel "This is the help text"
    widgetShowAll dia
    
  -------------------------------------------------------------------------------------------    
=======
  onClicked button2 $ do
    initpath <- get canvas imageFile
    exp <- readIORef expand
    if exp then do
      writeIORef expand False
      pix <- pixbufNewFromFileAtScale initpath 1000 1000 True 
      imageSetFromPixbuf canvas pix
      putStrLn "DONE"
    else do
      writeIORef expand True
      imageSetFromFile canvas initpath 
--------------------------------------------------------------------- 
>>>>>>> 958282a60a69b4116147008dcbe0d6ced59e26a1:resizeAdded.hs
  onClicked button1 $ do
    initpath <- get canvas imageFile
    basename <- return (takeBaseName initpath)
    do
      putStrLn $ initpath
      putStrLn $ "BOOM " ++ basename
      rv <- doesFileExist ("./"++basename++".jpg") 
      case rv of
        True -> do 
          putStrLn "temp file exists! No need for another"
          myimg <- loadJpegFile ("./"++basename++".jpg") -- load image from this location 
          grayscale myimg -- APPLY EFFECT
          saveJpegFile (-1) ("./"++basename++".jpg") myimg
          imageSetFromFile canvas ("./"++basename++".jpg")
        False -> do        
          putStrLn "temp file created!"
          myimg <- loadJpegFile initpath -- load image from its original location as a GD image
          saveJpegFile (-1) ("./"++basename++".jpg") myimg -- save a local temp copy at a predefined location
          grayscale myimg -- APPLY EFFECT
          saveJpegFile (-1) ("./"++basename++".jpg") myimg
          imageSetFromFile canvas ("./"++basename++".jpg")
   
    



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
  onDestroy window mainQuit -- not quite correct. What if edited image is in buffer ? define a function here. this function will aagain be used to define the exit action made by mps
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
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\           </toolbar>\
\          </ui>"

prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)  
