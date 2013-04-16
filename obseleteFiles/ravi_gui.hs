module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Cursor
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Sifflet.GtkForeign
import Graphics.Filters.GD
import Graphics.GD
import System.FilePath.Posix
import Data.IORef
import System.Directory -- for doesFileExist
main = do
  initGUI
  Just xml <- xmlNew "editor2.glade"
  window <- xmlGetWidget xml castToWindow "window1" -- this is the main window
  set window [windowTitle := "Image Editor",windowDefaultWidth := 400,windowDefaultHeight := 400]
  button1 <- xmlGetWidget xml castToButton "button1"
  button2 <- xmlGetWidget xml castToButton "button2"
  button3 <- xmlGetWidget xml castToButton "button3"
  button4 <- xmlGetWidget xml castToButton "button4"
  menubox <- xmlGetWidget xml castToVBox "vbox3"
  viewPort <- xmlGetWidget xml castToViewport "viewport1"
  canvas <- imageNewFromFile "stock.jpg"
  scrolledwindow1 <- xmlGetWidget xml castToScrolledWindow "scrolledwindow1"
  
  eb <- eventBoxNew
  set eb[containerChild := canvas]
  set viewPort[containerChild := eb]
  
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
  tmpFileName1 <- newIORef "" -- why Amitesh??
  zoomAmount <- newIORef 0
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
            writeIORef tmpFileName1 (basename++"temp1"++".jpeg") -- remember temp file's name
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
    zAmnt <- readIORef zoomAmount
    writeIORef zoomAmount (zAmnt+1)
    zAmnt <- readIORef zoomAmount
    tmpFile <- readIORef tmpFileName
    pix <- pixbufNewFromFile tmpFile
    w <- pixbufGetWidth pix
    h <- pixbufGetHeight pix
    putStrLn $ show $ truncate $ (1.1**(fromIntegral zAmnt))* fromIntegral w
    pix <- pixbufScaleSimple pix (truncate $ (1.1**(fromIntegral zAmnt))*(fromIntegral w))(truncate $ (1.1**(fromIntegral zAmnt))*(fromIntegral h)) InterpBilinear
    imageSetFromPixbuf canvas pix

---------------------------------------------------------------------            
  onActionActivate zoua $ do         
    zAmnt <- readIORef zoomAmount
    writeIORef zoomAmount (zAmnt-1)
    zAmnt <- readIORef zoomAmount
    putStrLn $ show zAmnt
    tmpFile <- readIORef tmpFileName
    pix <- pixbufNewFromFile tmpFile
    w <- pixbufGetWidth pix
    h <- pixbufGetHeight pix
    putStrLn $ show $ truncate $ (1.1**(fromIntegral zAmnt)) * fromIntegral w
    pix <- pixbufScaleSimple pix (truncate $ (1.1**(fromIntegral $ zAmnt))*(fromIntegral w))(truncate $ (1.1**(fromIntegral $ zAmnt))*(fromIntegral h)) InterpBilinear
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
  -------------------------------------------------------------------
  {--onClicked button3 $ do
    setCursor window Crosshair
    onButtonPress eb (\x -> do  
							putStrLn "Pressed"
							p1 <- widgetGetPointer eb
							putStrLn ("p1 out: " ++ show p1)
							onButtonRelease eb $ releaseFun eb canvas p1 tmpFileName
							return (True))					 
    putStrLn "Hello" --}      
  -------------------------------------------------------------------
  onClicked button3 $ do
    setCursor window Crosshair
    onButtonPress eb (\x -> do  
							putStrLn "Pressed"
							p1 <- widgetGetPointer canvas
							putStrLn ("p1 out: " ++ show p1)
							onButtonRelease eb 
								(\x -> do
									p2 <- widgetGetPointer eb
									putStrLn ("second: " ++ show p2)
									tmpFile <- readIORef tmpFileName
									myimg <- loadJpegFile tmpFile -- load image from this location 
									putStrLn ("p1 in: " ++ show p1)
									--cropRect myimg p1 p2
									--saveJpegFile (-1) tmpFile myimg
									img1<-crop myimg p1 p2
									saveJpegFile (-1) tmpFile img1
									imageSetFromFile canvas tmpFile
									return True)
							return (True))					 
    putStrLn "Hello"

  -------------------------------------------------------------------
  onClicked button2 $ do
    bwindow  <- windowNew
    set bwindow [windowTitle := "Brightness-Contrast",
              windowDefaultHeight := 200,
              windowDefaultWidth := 300]
    mainbox <- vBoxNew False 10
    containerAdd bwindow mainbox
    containerSetBorderWidth mainbox 10
    box1 <- vBoxNew False 0
    boxPackStart mainbox box1 PackNatural 0
    adj1 <- adjustmentNew 0.0 (-100.0) 100.0 1.0 1.0 1.0
    adj2 <- adjustmentNew 0.0 (-100.0) 100.0 1.0 1.0 1.0
    hsc1 <- hScaleNew adj1
    hsc2 <- hScaleNew adj2
  
    hbox1 <- hBoxNew False 0
    containerSetBorderWidth hbox1 10
    label1 <- labelNew (Just "Brightness:")
    boxPackStart hbox1 label1 PackNatural 0
    boxPackStart hbox1 hsc1 PackGrow 0
    hbox2 <- hBoxNew False 0
    containerSetBorderWidth hbox2 10
    label2 <- labelNew (Just "Contrast:")
    boxPackStart hbox2 label2 PackNatural 0
    boxPackStart hbox2 hsc2 PackGrow 0
  
    hbox3 <- hBoxNew False 0
    containerSetBorderWidth hbox3 10
    okbutton <-buttonNewWithLabel "OK"
    onClicked okbutton $ do
      tmpFile1 <- readIORef tmpFileName1
      tmpFile <- readIORef tmpFileName
      myimg <- loadJpegFile tmpFile1
      saveJpegFile (-1) tmpFile myimg
      removeFile tmpFile1
      mainQuit
 
    cancelButton <- buttonNewWithLabel "Cancel"
    onClicked cancelButton $ do
      tmpFile1 <-readIORef tmpFileName1
      removeFile tmpFile1
      mainQuit
    
    boxPackStart hbox3 okbutton PackGrow 0
    boxPackStart hbox3 cancelButton PackGrow 0
    
  
    boxPackStart box1 hbox1 PackNatural 0
    boxPackStart box1 hbox2 PackNatural 0
    boxPackStart box1 hbox3 PackNatural 0
  
    boxPackStart mainbox box1 PackGrow 0
    onValueChanged adj1 $ do 
      tmpFile <- readIORef tmpFileName
      tmpFile1 <- readIORef tmpFileName1
      val <- adjustmentGetValue adj1
      --writeIORef tmpFileName tmpFile
      myimg <- loadJpegFile tmpFile -- load image from this location 
      brightness myimg $ truncate val
      saveJpegFile (-1) tmpFile1 myimg
      imageSetFromFile canvas tmpFile1
      
        
    onValueChanged adj2 $ do
      tmpFile <- readIORef tmpFileName
      tmpFile1 <- readIORef tmpFileName1
      val <- adjustmentGetValue adj2
      --writeIORef tmpFileName tmpFile
      myimg <- loadJpegFile tmpFile -- load image from this location 
      contrast myimg $ truncate val
      saveJpegFile (-1) tmpFile1 myimg
      imageSetFromFile canvas tmpFile1
      
    widgetShowAll bwindow
    onDestroy bwindow mainQuit
    mainGUI
      
   
    



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

releaseFun eb canvas p1 tmpFileName x = do 
  p2 <- widgetGetPointer eb
  putStrLn ("second: " ++ show p2)
  tmpFile <- readIORef tmpFileName
  myimg <- loadJpegFile tmpFile -- load image from this location 
  putStrLn ("p1 in: " ++ show p1)
  cropRect myimg p1 p2
  saveJpegFile (-1) tmpFile myimg
  --img1<-crop myimg p1 p
  --saveJpegFile (-1) tmpFile img1
  imageSetFromFile canvas tmpFile
  return True
                                  
undoLast :: [Graphics.GD.Image -> IO()] -> IO Graphics.GD.Image -> IO Graphics.GD.Image
undoLast [] img = img
undoLast [a] img = img
undoLast (x:xs) img = do
  tmpimg <- img
  x tmpimg
  undoLast xs (return tmpimg)

cropRect img (a,b) (c,d) = do
  Graphics.GD.drawLine (a,b) (c,b) (rgb 254 254 254) img
  Graphics.GD.drawLine (c,b) (c,d) (rgb 254 254 254) img
  Graphics.GD.drawLine (a,d) (c,d) (rgb 254 254 254) img
  Graphics.GD.drawLine (a,b) (a,d) (rgb 254 254 254) img
    
crop :: Graphics.GD.Image -> Graphics.GD.Point -> Graphics.GD.Point -> IO Graphics.GD.Image
crop img (a,b) (c,d) = do
  newimg <- newImage ((c-a),(d-b))
  copyRegion (a,b) ((c-a),(d-b)) img (0,0) newimg
  resizeImage (c-a) (d-b) newimg
      
{--
abs :: Int -> Int
abs n | n >= 0    = n
      | otherwise = -n
--}
