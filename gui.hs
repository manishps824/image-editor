module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

main = do
  initGUI
  Just xml <- xmlNew "editor.glade"
  window <- xmlGetWidget xml castToWindow "window1"
  onDestroy window mainQuit
  label <- xmlGetWidget xml castToLabel "label1"
  button1 <- xmlGetWidget xml castToButton "button1"
  button2 <- xmlGetWidget xml castToButton "button2"
  button3 <- xmlGetWidget xml castToButton "button3"
  button4 <- xmlGetWidget xml castToButton "button4"
  
  onClicked button1 $ do
    fcd <- xmlGetWidget xml castToDialog "filechooserdialog1"
    result <- dialogRun fcd
    set label [ labelText := "Hello " ]
  widgetShowAll window
  mainGUI