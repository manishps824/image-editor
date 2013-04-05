module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Filters.GD
import Graphics.Filters.Util
--import Graphics.UI.Sifflet.GtkForeign
import Graphics.GD
import Data.IORef
import System.FilePath.Posix
import System.Directory -- for doesFileExist
import Effects
import HelperFunctions
main = do
  initGUI
  Just xml <- xmlNew "editor.glade"
  window <- xmlGetWidget xml castToWindow "window1" -- this is the main window
  set window [windowTitle := "Image Editor",windowDefaultWidth := 1300,windowDefaultHeight := 600]
  button1 <- xmlGetWidget xml castToButton "button1"
  button2 <- xmlGetWidget xml castToButton "button2"
  button3 <- xmlGetWidget xml castToButton "button3"
  sepiaButton <- xmlGetWidget xml castToButton "button4"
  button5 <- xmlGetWidget xml castToButton "button5"
  button6 <- xmlGetWidget xml castToButton "button6"
  colorixeButton <- xmlGetWidget xml castToButton "button7"
  invertButton <- xmlGetWidget xml castToButton "button8"
  embossButton <- xmlGetWidget xml castToButton "button9"
  meanButton <- xmlGetWidget xml castToButton "button10"
  edgeButton <- xmlGetWidget xml castToButton "button11"
  
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
  unda <- actionNew "UNDA" "Undo" (Just "Undo") (Just stockGotoFirst)
  zina <- actionNew "ZINA" "Zoom In" (Just "Zoom In") (Just stockZoomIn)
  zoua <- actionNew "ZOUA" "Zoom Out" (Just "Zoom Out") (Just stockZoomOut)
  rraa <- actionNew "RRAA" "Rotate Right" (Just "Rotate Right") (Just stockUndo)
  rlaa <- actionNew "RLAA" "Rotate Left" (Just "Rotate Left") (Just stockRedo)
  nexa <- actionNew "NEXA" "Next" (Just "Next") (Just stockGoForward)
  baca <- actionNew "BACA" "Back" (Just "Back") (Just stockGoBack)
  --create an action group with name AGR
  --actionGroupNew :: String -> IO ActionGroup
  agr <- actionGroupNew "AGR"
  -- add the actions to a group using actionGroupAddAction
  -- actionGroupAddAction :: ActionClass action => ActionGroup -> action -> IO ()
  -- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
  mapM_ (actionGroupAddAction agr) [fma, ema, hma]
  -- set no shortcut keys for all except exit
  mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing) [newa,opna,sava,svaa,cuta,copa,psta,hlpa,unda,zina,zoua,rraa,rlaa,nexa,baca]
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
  mapM_ prAct [fma,ema,hma,newa,sava,svaa,cuta,copa,psta,hlpa,unda,zina,zoua,rraa,rlaa,baca,nexa] -- add any new button for menubar here for rendering
  
  expand <- newIORef True
  changeList <- newIORef []
  fileName <- newIORef ""
  tmpFileName <- newIORef ""
  tmpFileName1 <- newIORef "" -- why Amitesh??
  zoomAmount <- newIORef 0
  myFileList <- newIORef (FileList [] [])
  upLeft <- newIORef (0,0)
  downRight <- newIORef (0,0)
------------------------------------------------------------------------------------------
  {--
this requires fileName,tmpFilename,tmpFilename1,canvas for a function
--}
  onActionActivate opna $ openAction fileName tmpFileName tmpFileName1 canvas myFileList
    
----------------------------------------------------------------------------------    
  onActionActivate nexa $ do
    tempfileList <- readIORef myFileList
    putStrLn "NEXT PRESSED"
    fileList <- return (goForward tempfileList)
    writeIORef myFileList fileList
    printFileList fileList
    frontFile <- return $ getFrontFile fileList 
    case frontFile of 
      "" -> 
        putStrLn "End of List"
      _ ->
        do
          putStrLn frontFile 
          basename <- return (takeBaseName frontFile)
          myimg <- loadImgFile frontFile  -- load image from this location 
          saveImgFile (-1) (basename++"temp"++".jpeg") myimg -- save temp file in code ir for future use
          writeIORef tmpFileName (basename++"temp"++".jpeg") -- remember temp file's ame
          writeIORef tmpFileName1 (basename++"temp1"++".jpeg") -- remember temp file's name
          imageSetFromFile canvas (basename++"temp"++".jpeg") -- render the image from temp file on canvas
          putStrLn $ "Opening File: " ++ frontFile
    
    
    
  onActionActivate baca $ do
    tempfileList <- readIORef myFileList
    putStrLn "BACK PRESSED"
    printFileList tempfileList
    fileList <- return (goBackward tempfileList)
    writeIORef myFileList fileList
    printFileList fileList
    frontFile <- return $ getFrontFile fileList 
    case frontFile of 
      "" -> 
        putStrLn "End of List"
      _ ->
        do
          putStrLn frontFile 
          basename <- return (takeBaseName frontFile)
          myimg <- loadImgFile frontFile  -- load image from this location 
          saveImgFile (-1) (basename++"temp"++".jpeg") myimg -- save temp file in code ir for future use
          writeIORef tmpFileName (basename++"temp"++".jpeg") -- remember temp file's ame
          writeIORef tmpFileName1 (basename++"temp1"++".jpeg") -- remember temp file's name
          imageSetFromFile canvas (basename++"temp"++".jpeg") -- render the image from temp file on canvas
          putStrLn $ "Opening File: " ++ frontFile
       
            
    
--------------------------------------------------------------------------------------------------
  
  onActionActivate unda $ do    -- user pressed undo button
    effectList <- readIORef changeList -- get all the changes that have been made so far
    originalPath <- readIORef fileName -- pick the original image
    tmpPath <- readIORef tmpFileName -- read temp image path for overwriting
    newImg <- undoLast effectList (loadImgFile originalPath) -- function that will apply all but last of the effects present in the list 
    saveImgFile (-1) tmpPath newImg
    imageSetFromFile canvas tmpPath
    case effectList of
      [] -> do
        writeIORef changeList []
      _ -> do
        writeIORef changeList (init effectList)
      
  
--------------------------------------------------------------------- 
  onActionActivate zina $ zoomInOut zoomAmount tmpFileName canvas 1
  onActionActivate zoua $ zoomInOut zoomAmount tmpFileName canvas (-1)
---------------------------------------------------------------------
  onActionActivate rraa $ rotateA tmpFileName canvas 1
  onActionActivate rlaa $ rotateA tmpFileName canvas (-1)
---------------------------------------------------------------------    
  {--   
  Effects added : Grayscale,Brightness
  --}
  onClicked button1 $ noArgEffect grayscale changeList tmpFileName canvas -- add edgeDetect,emboss,meanRemoval,negative like this
-----------------------------------------------------------------------------
  onClicked button6 $ noArgEffect duoTone changeList tmpFileName canvas
  onClicked sepiaButton $ noArgEffect sepia changeList tmpFileName canvas    
  onClicked button5 $ noArgEffect gaussianBlur changeList tmpFileName canvas    
  onClicked invertButton $ noArgEffect negative changeList tmpFileName canvas    
  onClicked embossButton $ noArgEffect emboss changeList tmpFileName canvas    
  onClicked meanButton $ noArgEffect meanRemoval changeList tmpFileName canvas    
  onClicked edgeButton $ noArgEffect edgeDetect changeList tmpFileName canvas    
-----------------------------------------------------------------------------
  {-onClicked colorixeButton $ do
    bwindow <- windowNew
    set bwindow [windowTitle := "Color Selection",
				containerBorderWidth := 10 ]
    vb <- vBoxNew False 0
    containerAdd bwindow vb
    colb <- colorButtonNew
    boxPackStart vb colb PackGrow 0
    
    onColorSet colb $ do colour <- colorButtonGetColor colb
                         case colour of 
                           (Color r g b)->rr = fromIntegral r / 255							
                                          rg = fromIntegral g / 255									
                                          rb = fromIntegral b / 255							
                                          ra = fromIntegral 1 / 255
                                          --in rgb=(rr,rg,rb,ra)     
                         putStrLn (show  colour)

    
    
    widgetShowAll bwindow
    onDestroy bwindow mainQuit
    mainGUI
-}
----------------------------------------------------------------------------
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
    
    onClicked okbutton $  do
		val1 <- adjustmentGetValue adj1
		val2 <- adjustmentGetValue adj2
		opList <- readIORef changeList 
		writeIORef changeList (opList++[(flip (brightness) (truncate val1))]) -- add f to the list of effects applied so far
		writeIORef changeList (opList++[(flip (contrast)  (truncate val2))]) -- add f to the list of effects applied so far
		okAction tmpFileName tmpFileName1 bwindow
 
    cancelButton <- buttonNewWithLabel "Cancel"
    
    onClicked cancelButton $ cancelAction tmpFileName tmpFileName1 bwindow canvas
    
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
      myimg <- loadImgFile tmpFile -- load image from this location 
      brightness myimg $ truncate val
      saveImgFile (-1) tmpFile1 myimg
      imageSetFromFile canvas tmpFile1
      
        
    onValueChanged adj2 $ do
      tmpFile <- readIORef tmpFileName
      tmpFile1 <- readIORef tmpFileName1
      val <- adjustmentGetValue adj2
      --writeIORef tmpFileName tmpFile
      myimg <- loadImgFile tmpFile -- load image from this location 
      contrast myimg $ truncate val
      saveImgFile (-1) tmpFile1 myimg
      imageSetFromFile canvas tmpFile1
      
    widgetShowAll bwindow
    onDestroy bwindow mainQuit
    mainGUI
---------------------------------------------------
  onClicked button3 $ do
    tmpFile <- readIORef tmpFileName
    myimg <- loadJpegFile tmpFile
    (imWidth, imHeight) <- imageSize myimg
    myimgCopy <- copyImage myimg
    putStrLn (show imWidth)
    myWin <- windowNew
    set myWin [windowTitle := "Crop", windowDefaultWidth := truncate $ 1.2* (fromIntegral imWidth),
     windowDefaultHeight := truncate $ 1.2*(fromIntegral imHeight),
      containerBorderWidth := 10 ]
    vb <- vBoxNew False 0
    containerAdd myWin vb
    myCanvas <- imageNewFromFile tmpFile
    eb1 <- eventBoxNew
    boxPackStart vb eb1 PackGrow 0
    set eb1[containerChild := myCanvas]
    miscSetAlignment myCanvas 0 0
    
    sep <- hSeparatorNew
    boxPackStart vb sep PackGrow 10
    hb <- hBoxNew False 0
    boxPackStart vb hb PackGrow 0
     
    myok <- buttonNewWithLabel "Crop to Selection"
    boxPackStart hb myok PackGrow 0     
    mycancel <- buttonNewWithLabel "Cancel"
    boxPackStart hb mycancel PackGrow 0
    myclose <- buttonNewWithLabel "Close"
    boxPackStart hb myclose PackGrow 0
    widgetSetSensitivity myok False
    widgetSetSensitivity mycancel False
    --setCursor window Crosshair
    
    --set viewPort[containerChild := eb]
    onButtonPress eb1 (\x -> do  
							p5 <- miscGetAlignment myCanvas
							p1 <- widgetGetPointer myCanvas
							writeIORef upLeft p1
							onButtonRelease eb1 
								(\x -> do
									p2 <- widgetGetPointer myCanvas
									putStrLn ("Up Left: " ++ show p1)
									putStrLn ("Down Right: " ++ show p2)
									writeIORef downRight p2
									tmpFile <- readIORef tmpFileName
									myimg <- loadJpegFile tmpFile -- load image from this location 
									cropRect myimg p1 p2
									saveJpegFile (-1) tmpFile myimg
									widgetSetSensitivity myok True
									widgetSetSensitivity mycancel True
									--setCursor window Arrow
									imageSetFromFile myCanvas tmpFile
									return True)
							return (True))					 
    putStrLn "Hello"
    
    onClicked myok $ do
      p1 <- readIORef upLeft
      p2 <- readIORef downRight
      img1 <- crop myimg p1 p2
      saveJpegFile (-1) tmpFile img1
      imageSetFromFile canvas tmpFile
      widgetDestroy myWin
    onClicked mycancel $ do
      saveJpegFile (-1) tmpFile myimgCopy
      imageSetFromFile myCanvas tmpFile
      widgetSetSensitivity myok False
      widgetSetSensitivity mycancel False
    onClicked myclose $ do
      saveJpegFile (-1) tmpFile myimgCopy
      widgetDestroy myWin  

    widgetShowAll myWin
    onDestroy myWin mainQuit
    mainGUI      
   
    
----------------------------------------------------
  
--
  
{--
    tmpFile1 <- readIORef tmpFileName1
    tmpFile <- readIORef tmpFileName
    myimg <- loadImgFile tmpFile
    saveImgFile (-1) tmpFile1 myimg
    bwindow <- windowNew
    set bwindow [windowTitle := "Gaussian Blur",
              windowDefaultHeight := 100,
              windowDefaultWidth := 200]
    mainbox <- vBoxNew False 10
    containerAdd bwindow mainbox
    containerSetBorderWidth mainbox 10
    box1 <- vBoxNew False 0
    boxPackStart mainbox box1 PackNatural 0
    adj1 <- adjustmentNew 0.0 0 10.0 1.0 1.0 1.0
    hsc1 <- hScaleNew adj1
    hbox1 <- hBoxNew False 0
    hbox3 <- hBoxNew False 0
    containerSetBorderWidth hbox3 10
    boxPackStart hbox1 hsc1 PackGrow 0
    okbutton <-buttonNewWithLabel "OK"
    onClicked okbutton $ okAction tmpFileName tmpFileName1 bwindow
 
    cancelButton <- buttonNewWithLabel "Cancel"
    onClicked cancelButton $ cancelAction tmpFileName tmpFileName1 bwindow canvas
      
    boxPackStart hbox3 okbutton PackGrow 0
    boxPackStart hbox3 cancelButton PackGrow 0  
    boxPackStart box1 hbox1 PackNatural 0
    boxPackStart box1 hbox3 PackNatural 0
    onValueChanged adj1 $ do 
      tmpFile1 <- readIORef tmpFileName1
      --val <- adjustmentGetValue adj1
      --writeIORef tmpFileName tmpFile
      myimg <- loadImgFile tmpFile1 -- load image from this location 
      gaussianBlur myimg
      saveImgFile (-1) tmpFile1 myimg
      imageSetFromFile canvas tmpFile1
      
    widgetShowAll bwindow
    onDestroy bwindow mainQuit
    mainGUI  
--}

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
\              <menuitem action=\"NEXA\" />\
\              <menuitem action=\"BACA\" />\
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
\            <toolitem action=\"RRAA\" />\
\            <toolitem action=\"RLAA\" />\
\            <toolitem action=\"NEXA\" />\
\            <toolitem action=\"BACA\" />\
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\           </toolbar>\
\          </ui>"

{-
printColor::Graphics.UI.Gtk.Color -> RGBA
printColor (Color r g b) =let 
						 rgb=		((fromIntegral r / 255) 
									(fromIntegral g / 255)
									(fromIntegral b / 255)
-}									
