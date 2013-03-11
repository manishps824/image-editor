module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

main = do
  initGUI
  Just xml <- xmlNew "editor.glade"
  window <- xmlGetWidget xml castToWindow "window1"
  onDestroy window mainQuit
  --closeButton <- xmlGetWidget xml castToButton "button2"
  --onClicked closeButton $ do
  --  widgetDestroy window
  label <- xmlGetWidget xml castToLabel "label1"
  --entry <- xmlGetWidget xml castToEntry "entry1"
  toolButton1 <- xmlGetWidget xml castToButton "toolbutton1"
  toolButton2 <- xmlGetWidget xml castToButton "toolbutton2"
  toolButton3 <- xmlGetWidget xml castToButton "toolbutton3"
  toolButton4 <- xmlGetWidget xml castToButton "toolbutton4"
  
  onClicked toolButton1 $ do
    set label [ labelText := "Hello " ]
  widgetShowAll window
  mainGUI