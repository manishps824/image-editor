module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk

main :: IO ()
main = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Menus and Toolbars",
                 windowDefaultWidth := 450, windowDefaultHeight := 200]

     box <- vBoxNew False 0
     containerAdd window box

     fma <- actionNew "FMA" "File" Nothing Nothing
     ema <- actionNew "EMA" "Edit" Nothing Nothing
     hma <- actionNew "HMA" "Help" Nothing Nothing

     newa <- actionNew "NEWA" "New"     (Just "Just a Stub") (Just stockNew)
     opna <- actionNew "OPNA" "Open"    (Just "Just a Stub") (Just stockOpen)
     sava <- actionNew "SAVA" "Save"    (Just "Just a Stub") (Just stockSave)
     svaa <- actionNew "SVAA" "Save As" (Just "Just a Stub") (Just stockSaveAs)
     exia <- actionNew "EXIA" "Exit"    (Just "Just a Stub") (Just stockQuit)
 
     cuta <- actionNew "CUTA" "Cut"   (Just "Just a Stub") (Just stockCut)    
     copa <- actionNew "COPA" "Copy"  (Just "Just a Stub") (Just stockCopy)
     psta <- actionNew "PSTA" "Paste" (Just "Just a Stub") (Just stockPaste)

     hlpa <- actionNew "HLPA" "Help"  (Just "Just a Stub") (Just stockHelp)

     --create an action group with name AGR
     --actionGroupNew :: String -> IO ActionGroup
     agr <- actionGroupNew "AGR"
     -- add the actions to a group using actionGroupAddAction
     -- actionGroupAddAction :: ActionClass action => ActionGroup -> action -> IO ()
     -- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
     mapM_ (actionGroupAddAction agr) [fma, ema, hma]
     -- set no shortcut keys for all except exit
     mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing) 
       [newa,opna,sava,svaa,cuta,copa,psta,hlpa]
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
     boxPackStart box menubar PackNatural 0

     maybeToolbar <- uiManagerGetWidget ui "/ui/toolbar"
     let toolbar = case maybeToolbar of
                        (Just x) -> x
                        Nothing -> error "Cannot get toolbar from string." 
     boxPackStart box toolbar PackNatural 0

     actionSetSensitive cuta False

     onActionActivate exia (widgetDestroy window)
     --define the action handler for each action
     --right now it is same for each so using mapM_
     mapM_ prAct [fma,ema,hma,newa,opna,sava,svaa,cuta,copa,psta,hlpa]

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

prAct :: ActionClass self => self -> IO (ConnectId self)
prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)