#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-5.0 --package=turtle --package=brick

{-# LANGUAGE OverloadedStrings #-}

import Brick
import Brick.Types
import Brick.Widgets.Center
import Brick.Widgets.List
import qualified Control.Foldl as Fold
import Data.Maybe
import Data.Text
import Data.Vector
import Graphics.Vty
import Lens.Micro
import Turtle
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)



main :: IO ()
main = do
    (useDmenu, path) <- options "A simple file menu" $ liftA2 (,)
        (switch "dmenu" 'd' "Use dmenu")
        (optional (argPath "DIRECTORY" "The directory (default: working directory)"))
    let menu = if useDmenu then dmenu else brickmenu
    selectedFile <- menu (listFiles (fromMaybe "." path))
    case selectedFile of
        Just file -> echo file
        Nothing   -> exitFailure

listFiles :: FilePath -> Shell Text
listFiles dir = fmap (pack . encodeString) (ls dir)

exitFailure :: IO ()
exitFailure = exit (ExitFailure 1)

---------------------------------------------------------------------------

dmenu :: Shell Text -> IO (Maybe Text)
dmenu items =
    let dmenuOut = inproc "dmenu" [] items
    in  fold dmenuOut Fold.head

---------------------------------------------------------------------------

brickmenu :: Shell Text -> IO (Maybe Text)
brickmenu items = do
    itemsVector <- foldIO items Fold.vector
    finalList <- defaultMain app (list (Name "list") itemsVector 1)
    pure (finalList ^. listSelectedL >>= \index -> itemsVector !? index)

app :: App (List Text) Event
app = App
    { appDraw = \list -> [renderList list (const txt)]

    , appChooseCursor = showFirstCursor

    , appHandleEvent = \list event -> case event of
        EvKey KEnter [] -> halt list
        _               -> handleEvent event list >>= continue

    , appStartEvent = pure

    , appAttrMap = const $ attrMap defAttr
        [ (listAttr,         fg white)
        , (listSelectedAttr, black `on` white) ]

    , appLiftVtyEvent = id }
