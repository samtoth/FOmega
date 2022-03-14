{-# LANGUAGE OverloadedLabels, FlexibleContexts, OverloadedStrings, RecursiveDo, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Applicative (liftA2)
import Control.Monad (join, forM, forM_)
import qualified Data.Map.Lazy as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import GI.Gtk ( AttrOp(..))
import Reflex ( Dynamic
              , Event
              , (<@>)
              , current
              , holdDyn
              , list
              , listHoldWithKey
              , mergeList
              , performEvent_
              , performEvent
              , switch, (<@), foldDyn, updated, fmapMaybe
              )
import qualified GI.Gdk as Gdk
import Reflex.GI.Gtk ( ReactiveAttrOp((:==))
                     , MonadReflexGtk
                     , runGtk
                     , runReflexGtk
                     , eventOnSignal
                     , eventOnSignal0
                     , sink
                     , sinkBoxUniform, eventOnSignal1R, eventOnSignal0R
                     )
import System.Exit ( ExitCode(ExitFailure)
                   , die
                   , exitSuccess
                   , exitWith
                   )
import System.Environment ( getArgs
                          , getProgName
                          )
import Data.GI.Base.GType (gtypeString)
import qualified Reflex.GI.Gtk.Run as Gtk
import Lib (Treeable (name, children), formId)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word32)

stringInput :: (MonadReflexGtk t m)
            => m (Gtk.Widget, Dynamic t T.Text, Event t ())
stringInput = do
  ( input
    , deleteButton
    , inputW
    ) <- runGtk $ do
    box <- Gtk.boxNew Gtk.OrientationHorizontal 0
    input <- Gtk.entryNew
    deleteButton <- Gtk.buttonNewFromIconName (Just "list-remove") $
      fromIntegral $ fromEnum Gtk.IconSizeButton
    #packStart box input True True 0
    #packStart box deleteButton False False 0
    #showAll box
    inputW <- Gtk.toWidget box
    pure (input, deleteButton, inputW)
  newTextE <- eventOnSignal input #changed (Gtk.get input #text >>=)
  textDyn <- holdDyn T.empty newTextE
  delete <- eventOnSignal0 deleteButton #clicked
  pure (inputW, textDyn, delete)

main :: IO ()
main = do
  Gtk.applicationNew (Just "de.weltraumschlangen.reflex-test") []
    >>= maybe
    (die "Failed to initialize GTK")
    (\application -> do
        argv <- liftA2 (:) getProgName getArgs
        ret <- runReflexGtk application (Just argv) (mainLayout application)

        case ret of
          0 -> exitSuccess
          n -> exitWith $ ExitFailure $ fromIntegral n)


mainLayout :: (MonadReflexGtk t m) => Gtk.Application -> m ()
mainLayout application = do
          mainWindow <- runGtk $ Gtk.applicationWindowNew application
          Gtk.set mainWindow [#title := "Sam Toth ide"]
          Gtk.windowSetDefaultSize mainWindow 1260 980

          activate <- eventOnSignal0 application #activate

          keyPress <- eventOnSignal1R mainWindow #keyPressEvent True


          keyPress' <- performEvent $ (\e -> Gtk.get e #string) <$> keyPress

          actions <- globalActions (fmapMaybe id keyPress')

          propertiesPanel <- runGtk $ Gtk.labelNew (Just "Properties")

          treeViewModel <- runGtk $ genTree formId

          treeView <- runGtk $ createTreeView treeViewModel

          runGtk $ Gtk.unrefObject treeViewModel

          outerBox <- runGtk $ Gtk.panedNew Gtk.OrientationVertical
          mainPanel <- do
            panel <- runGtk $ Gtk.panedNew Gtk.OrientationHorizontal
            p2 <- runGtk $ Gtk.labelNew (Just "2")
            Gtk.panedAdd1 panel treeView
            Gtk.panedAdd2 panel p2
            return panel

          Gtk.panedAdd1 outerBox mainPanel
          Gtk.panedAdd2 outerBox propertiesPanel

          runGtk $ #add mainWindow outerBox

          performEvent_ $ runGtk (#showAll mainWindow) <$ activate

data Keys = K Modi Char
data Modi = NoMod | Ctr | Alt
data Action = Action

globalActions :: _ => Event t T.Text -> m (Event t Action)
globalActions e = do
      kphandler <- keyPressHandler e
      let commandFormer = \state -> Just Action

      return $ fmapMaybe commandFormer (updated kphandler)


keyPressHandler :: MonadReflexGtk t m => Event t T.Text ->  m (Dynamic t [Keys])
keyPressHandler e =
         let handler :: T.Text -> [Keys] -> [Keys]
             handler = \key state -> (K NoMod <$> T.unpack key) ++ state
        in   foldDyn handler [] e

createTreeView :: (Gtk.IsTreeModel model, Gtk.GObject model) => model -> IO Gtk.TreeView
createTreeView model = do
    view <- Gtk.treeViewNewWithModel model
    column <- Gtk.treeViewColumnNew

    Gtk.treeViewAppendColumn view column

    renderer <- Gtk.cellRendererTextNew

    Gtk.treeViewColumnPackStart column renderer True

    Gtk.treeViewColumnAddAttribute column renderer "text" 0

    return view

genTree :: Treeable tree => tree -> IO Gtk.TreeStore
genTree treeable = do
      treeStore <- Gtk.treeStoreNew [gtypeString]
      helper treeStore treeable Nothing
  where
    helper :: Treeable tree' => Gtk.TreeStore -> tree' -> Maybe Gtk.TreeIter -> IO Gtk.TreeStore
    helper treeStore treeable' parent = do
      parent' <- Gtk.treeStoreAppend treeStore parent
      parentLabel <- Gtk.toGValue @(Maybe T.Text) (Just $ name treeable')
      Gtk.treeStoreSet treeStore parent' [0] [parentLabel]

      forM_ (children treeable') (flip (helper treeStore) $ Just parent')

      return treeStore

reactiveApp :: _ => Gtk.Application -> m ()
reactiveApp application = do
          mainWindow <- runGtk $ Gtk.applicationWindowNew application
          activate <- eventOnSignal0 application #activate

          outerBox <- runGtk $ Gtk.boxNew Gtk.OrientationVertical 2
          inputBox <- runGtk $ Gtk.boxNew Gtk.OrientationVertical 0
          outputBox <- Gtk.boxNew Gtk.OrientationVertical 5
          addInputButton <- runGtk $ Gtk.buttonNewFromIconName (Just "list-add") $
            fromIntegral $ fromEnum Gtk.IconSizeButton
          Gtk.set addInputButton [#label Gtk.:= "Add"]
          runGtk $ do
            #add mainWindow outerBox
            #packStart outerBox inputBox False False 0
            #packStart outerBox addInputButton False False 0
            #packStart outerBox outputBox False False 0

          addInput <- eventOnSignal0 addInputButton #clicked

          rec
            let freeKey = maybe (minBound :: Word) (succ . fst) . M.lookupMax
                          <$> current inputWidgets
                inputWidgetUpdates = mconcat
                                     [ (\k -> M.singleton k . Just) <$> freeKey <@> addInput
                                     , M.fromList . map (,Nothing) . NE.toList <$> delete
                                     ]
            inputWidgets <-
              listHoldWithKey (M.singleton 0 ()) inputWidgetUpdates $ \k () -> do
              (\(widget, text, delete') -> (widget, text, k <$ delete')) <$> stringInput

            let delete = switch $ mergeList . map (\(_, _, d) -> d) . M.elems
                         <$> current inputWidgets

          sinkBoxUniform inputBox (M.map (\(w, _, _) -> w) <$> inputWidgets)
            False False 0 Gtk.PackTypeStart

          outputWidgets <- list (M.map (\(_, t, _) -> t) <$> inputWidgets)
            $ \textBB -> do
            let textB = join textBB
            label <- runGtk $ Gtk.labelNew Nothing
            sink label [#label :== textB]
            #show label
            pure label
          sinkBoxUniform outputBox outputWidgets True True 10 Gtk.PackTypeStart

          performEvent_ $ runGtk (#showAll mainWindow) <$ activate



