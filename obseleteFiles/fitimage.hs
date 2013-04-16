module Main where

import Data.Char
import Graphics.UI.Gtk
import System.Directory
import System.Environment
import Text.Regex.Posix
import Data.IORef

main = do
    initGUI
    window <- windowNew
    onDestroy window mainQuit
    scroll <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
    vbox <- vBoxNew False 0
    hbox <- hBoxNew False 0
    set window [windowDefaultWidth := 600, windowDefaultHeight := 500,
                containerChild := vbox, containerBorderWidth := 0]
    image <- imageNew
    quit <- buttonNewFromStock stockQuit
    quitID <- on quit buttonActivated $ do mainQuit
    nextButton <- buttonNewFromStock stockGoForward
    backButton <- buttonNewFromStock stockGoBack
    scaleToggle <- toggleButtonNewWithLabel "Fit to Window"
    boxPackStart hbox backButton PackRepel 0
    boxPackStart hbox nextButton PackRepel 0
    boxPackEnd hbox quit PackRepel 0
    boxPackEnd hbox scaleToggle PackRepel 0
    boxPackEnd vbox hbox PackNatural 0
    boxPackStart vbox scroll PackGrow 0
    scrolledWindowAddWithViewport scroll image
    args <- getArgs
    imageLst <- dirContents args
    imageSetFromFile image (head imageLst)
    expand <- newIORef True --Used for deciding whether an image should be fullsize
    position <- newIORef 0
    on scaleToggle toggled $ do
        pos <- readIORef position
        let gifPattern = ".gif"
            filename = (imageLst !! pos) in
                if filename =~ gifPattern :: Bool
                    then imageSetFromFile image filename--possibly an animation, Ignores Scaling
                    else do --Static Image, Scale away!
                        exp <- readIORef expand
                        if exp then do
                            writeIORef expand False
                            windowSize <- windowGetSize window
                            pix <- pixbufNewFromFileAtScale filename ((fst windowSize)-10) ((snd windowSize)-32) True
                            imageSetFromPixbuf image pix
                        else do
                            writeIORef expand True
                            imageSetFromFile image filename
    on nextButton buttonActivated $ do
        oldPosition <- readIORef position
        if oldPosition < (length imageLst)-1
            then writeIORef position (1+oldPosition)
            else writeIORef position 0
        newPosition <- readIORef position
        let gifPattern = ".gif"
            filename = (imageLst !! newPosition) in
                if filename =~ gifPattern :: Bool
                    then imageSetFromFile image filename--possibly an animation, Ignores Scaling
                    else do --Static Image, Scale away!
                        exp <- readIORef expand
                        if not exp then do
                            windowSize <- windowGetSize window
                            pix <- pixbufNewFromFileAtScale filename ((fst windowSize)-10) ((snd windowSize)-32) True
                            imageSetFromPixbuf image pix
                        else imageSetFromFile image filename
    on backButton buttonActivated $ do
        oldPosition <- readIORef position
        if (oldPosition > 0)
            then writeIORef position (oldPosition-1)
            else writeIORef position ((length imageLst)-1)
        newPosition <- readIORef position
        let gifPattern = ".gif"
            filename = (imageLst !! newPosition) in
                if filename =~ gifPattern :: Bool
                    then imageSetFromFile image filename--possibly animation, Ignores Scaling
                    else do --Static Image, Scale away!
                        exp <- readIORef expand
                        if not exp then do
                            windowSize <- windowGetSize window
                            pix <- pixbufNewFromFileAtScale filename ((fst windowSize)-10) ((snd windowSize)-32) True
                            imageSetFromPixbuf image pix
                        else imageSetFromFile image filename
    widgetShowAll window
    mainGUI

dirContents [] = do
    cwd <- getCurrentDirectory
    lst <- getDirectoryContents cwd
    let flst = filter isImage lst in
        if flst == [] then error "No images"
        else return flst
dirContents (x:xs) = do
    cwd <- getCurrentDirectory
    lst <- getDirectoryContents cwd
    let flst = filter isImage lst in
        if flst == [] then error "No images"
        else
            if elem x flst
                then return (shift x flst)
                else return flst

isImage :: String -> Bool
isImage filename = let pattern = "(.png|.jpg|.gif|.bmp|.jpeg)" in
    filename =~ pattern :: Bool

shift :: String -> [String] -> [String]
shift item lst@(x:xs) = if item == x then lst else shift item (xs++[x])