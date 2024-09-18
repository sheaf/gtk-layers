{-# LANGUAGE OverloadedLabels #-}

module Main where

-- base
import Control.Monad
  ( unless, void )
import Data.Foldable
  ( for_ )
import Data.Maybe
  ( catMaybes, fromMaybe, fromJust )
import Data.List
  ( elemIndex )
import Data.Word
  ( Word32, Word64 )
import Foreign.StablePtr
  ( StablePtr
  , newStablePtr, deRefStablePtr, freeStablePtr
  )
import GHC.Stack
  ( HasCallStack )
import System.Environment
  ( setEnv )
import System.Exit
  ( ExitCode(..), exitWith, exitSuccess )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import Data.Set
  ( Set )
import qualified Data.Set as Set
import Data.Sequence
  ( Seq )
import qualified Data.Sequence as Seq

-- directory
import qualified System.Directory as Directory
  ( canonicalizePath )

-- haskell-gi-base
import qualified Data.GI.Base as GI
import qualified Data.GI.Base.GObject as GI
import qualified Data.GI.Base.GType as GI
import qualified Data.GI.Base.Overloading as GI

-- gi-gdk
import qualified GI.Gdk as GDK

-- gi-gio
import qualified GI.Gio as GIO

-- gi-gobject
import qualified GI.GObject as GObject

-- gi-gtk
import qualified GI.Gtk as GTK

-- stm
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text

-- gtk-layers
import qualified Paths_gtk_layers as Cabal
  ( getDataFileName )

--------------------------------------------------------------------------------

(!) :: ( HasCallStack, Ord k, Show k ) => Map k a -> k -> a
m ! k = case Map.lookup k m of
  Nothing -> error $ "lookup failed; key = " ++ show k
  Just r -> r

-- | Create a new 'GTK.TreeListModel' from the given set of layers.
newLayersListModel :: STM.TVar History -> IO ( GIO.ListStore, GTK.TreeListModel )
newLayersListModel historyTVar = do
  itemType <- GI.glibType @LayerItem
  store <- GIO.listStoreNew itemType
  ( Content { layerHierarchy }, _ ) <- present <$> STM.readTVarIO historyTVar
  let topLayers = fromMaybe [] . snd $ layerHierarchy ! Nothing
  for_ topLayers $ \ layerUniq -> do
    let ( _parent, mbChildren ) = layerHierarchy ! ( Just layerUniq )
        layer = case mbChildren of
          Nothing -> Layer { layerUnique = layerUniq }
          Just {} -> Group { layerUnique = layerUniq }
    item <- GI.unsafeCastTo LayerItem =<< GObject.objectNewv itemType []
    GI.gobjectSetPrivateData item ( Just layer )
    GIO.listStoreAppend store item

  rootModel <- GIO.toListModel store
  let passthrough = False
      auto_expand = True

  model <- GTK.treeListModelNew rootModel passthrough auto_expand createChildModel

  return ( store, model )

    where
      createChildModel :: GObject.Object -> IO ( Maybe GIO.ListModel )
      createChildModel parent = do
        ( _parentDepth, layerItem ) <- treeListRowLayerItem parent
        mbData <- GI.gobjectGetPrivateData layerItem
        case mbData of
          Nothing -> error "createChildModel: LayerItem has no data"
          Just dat ->
            case dat of
              Layer  {} -> return Nothing
              Group { layerUnique = groupUnique } -> do
                ( Content { layerHierarchy }, _ ) <- present <$> STM.readTVarIO historyTVar
                let children = fromMaybe [] . snd $ layerHierarchy ! ( Just groupUnique )
                -- NB: create a simple GIO.ListStore, not a nested GTK.TreeListModel,
                -- as that would cause e.g. grand-children model to be created twice.
                itemType <- GI.glibType @LayerItem
                store <- GIO.listStoreNew itemType
                for_ children $ \ childUniq -> do
                  let mbChildChildren = snd $ layerHierarchy ! ( Just childUniq )
                      childLayer = case mbChildChildren of
                        Nothing -> Layer { layerUnique = childUniq }
                        Just {} -> Group { layerUnique = childUniq }
                  item <- GI.unsafeCastTo LayerItem =<< GObject.objectNewv itemType []
                  GI.gobjectSetPrivateData item ( Just childLayer )
                  GIO.listStoreAppend store item
                Just <$> GIO.toListModel store

-- | Gets the 'LayerItem' for a row in a 'GTK.TreeListModel'
-- with @passthrough = False@.
treeListItemLayerItem :: GTK.ListItem -> IO ( Int, LayerItem )
treeListItemLayerItem listItem = do
  mbListRow <- GTK.listItemGetItem listItem
  case mbListRow of
    Nothing -> error "treeListItemLayerItem: ListItem has no item"
    Just listRow -> do
      listRowItem <- treeListRowGetItem listRow
      treeListRowLayerItem listRowItem

-- | Helper function to get the item underlying a 'GTK.TreeListRow'.
treeListRowGetItem :: GObject.Object -> IO GObject.Object
treeListRowGetItem listRow = do
  mbListRowItem <- GTK.treeListRowGetItem =<< GTK.unsafeCastTo GTK.TreeListRow listRow
  case mbListRowItem of
    Nothing   -> error "treeListRowGetItem: TreeListRow has no item"
    Just item -> return item

-- | Recurse through the item of a 'GTK.TreeListRow' to get the 'LayerItem'.
--
-- Returns the depth at which the item was found.
treeListRowLayerItem :: GObject.Object -> IO ( Int, LayerItem )
treeListRowLayerItem = go 0
  where
    go !depth listRowItem = do
      mbLayerItem <- GTK.castTo LayerItem listRowItem
      case mbLayerItem of
        Nothing -> do
          listRow' <- treeListRowGetItem listRowItem
          go ( depth + 1 ) listRow'
        Just layerItem ->
          return ( depth, layerItem )

-- | The generic widget used to display a list item.
--
-- Structure:
--
--  - ListItem
--    - TreeExpander
--      - ContentBox
--        - CheckBox
--        - Label
data LayerViewWidget =
  LayerViewWidget
    { layerViewContentBox  :: GTK.Box
    , layerViewCheckButton :: GTK.CheckButton
    , layerViewLabel       :: GTK.EditableLabel
    }

newLayerViewWidget :: IO GTK.TreeExpander
newLayerViewWidget = do

  expander <- GTK.treeExpanderNew

  GTK.treeExpanderSetIndentForIcon  expander True
  GTK.treeExpanderSetIndentForDepth expander True
  GTK.treeExpanderSetHideExpander   expander False

  contentBox <- GTK.boxNew GTK.OrientationHorizontal 20
  GTK.treeExpanderSetChild expander ( Just contentBox )

  checkBox <- GTK.checkButtonNew
  GTK.boxAppend contentBox checkBox
  itemLabel <- editableLabelNew
  GTK.boxAppend contentBox itemLabel

  return expander

-- | Create a new editable label, but remove any 'DragSource' or 'DropTarget'
-- controllers attached to it, as we don't want the label to participate in
-- drag-and-drop operations, especially because having it participate in
-- drag-and-drop operations triggers segfaults due to a GTK bug.
editableLabelNew :: IO GTK.EditableLabel
editableLabelNew = do
  label <- GTK.editableLabelNew " "
  widget <- GTK.toWidget label
  removeControllers widget
  return label

  where
    removeControllers widget = do
      controllers <- GTK.widgetObserveControllers widget
      nbControllers <- GIO.listModelGetNItems controllers
      unless ( nbControllers == 0 ) $
        for_ [ 0 .. nbControllers - 1 ] $ \ i -> do
          mbController <- GIO.listModelGetItem controllers i
          for_ mbController $ \ controller -> do
            mbDrag <- GTK.castTo GTK.DragSource controller
            mbDrop <- GTK.castTo GTK.DropTarget controller
            for_ mbDrag $ GTK.widgetRemoveController widget
            for_ mbDrop $ GTK.widgetRemoveController widget
      mbChild <- GTK.widgetGetFirstChild widget
      case mbChild of
        Nothing -> return ()
        Just c -> do
          removeControllers c
          removeControllersSiblings c
    removeControllersSiblings c = do
      mbNext <- GTK.widgetGetNextSibling c
      case mbNext of
        Nothing -> return ()
        Just next -> do
          removeControllers next
          removeControllersSiblings next

-- | Get the widget hierarchy for a list item, so that we can modify
-- the wdigets to display the appropriate content.
getLayerViewWidget :: GTK.TreeExpander -> IO LayerViewWidget
getLayerViewWidget expander = do
  mbContentBox <- GTK.treeExpanderGetChild expander
  case mbContentBox of
    Nothing -> error "getLayerViewWidget: expected ListItem->Expander->Box"
    Just contentBox0 -> do
      contentBox <- GTK.unsafeCastTo GTK.Box contentBox0
      mbCheckButton <- traverse ( GTK.unsafeCastTo GTK.CheckButton ) =<< GTK.widgetGetFirstChild contentBox
      case mbCheckButton of
        Nothing -> error "getLayerViewWidget: expected ListItem->Expander->Box->CheckButton"
        Just checkButton -> do
          mbLayerLabel <- traverse ( GTK.unsafeCastTo GTK.EditableLabel ) =<< GTK.widgetGetNextSibling checkButton
          case mbLayerLabel of
            Nothing -> error "getLayerViewWidget: expected ListItem->Expander->Box->{CheckButton,LayerLabel}"
            Just layerLabel ->
              return $
                LayerViewWidget
                  { layerViewContentBox = contentBox
                  , layerViewCheckButton = checkButton
                  , layerViewLabel = layerLabel
                  }

-- | Create a new 'GTK.ListView' that displays 'LayerItem's.
newLayerView :: GTK.ApplicationWindow
             -> STM.TVar Unique
             -> STM.TVar History
             -> GTK.Label
             -> GIO.ListStore
             -> GTK.TreeListModel
             -> IO GTK.ListView
newLayerView window _uniqueTVar historyTVar layersContentDebugLabel rootStore layersListModel = do
  layersListFactory <- GTK.signalListItemFactoryNew

  -- Connect to "setup" signal to create generic widgets for viewing the tree.
  _ <- GTK.onSignalListItemFactorySetup layersListFactory $ \ listItem0 -> do

        listItem <- GTK.unsafeCastTo GTK.ListItem listItem0
        GTK.listItemSetFocusable listItem False

        expander <- newLayerViewWidget
        GTK.listItemSetChild listItem ( Just expander )
        LayerViewWidget
          { layerViewLabel = label }
            <- getLayerViewWidget expander

        -- Connect a signal for editing the layer name.
        --
        -- NB: we don't use the 'onEditableChanged' signal, as that updates
        -- after every key stroke.
        void $ GI.after label ( GI.PropertyNotify #hasFocus ) $ \ _ -> do
          newText <- GTK.editableGetText label
          ( _, layerItem ) <- treeListItemLayerItem listItem
          dat <- getLayerData layerItem
          History { present = ( newContent, newMetadata ) } <- STM.atomically $ do
            hist@History { present = ( layers, meta@( Meta { names } ) ) } <- STM.readTVar historyTVar
            let names' = case dat of { Layer { layerUnique = u } -> Map.insert u newText names
                                     ; Group { layerUnique = u } -> Map.insert u newText names }
                meta' = meta { names = names' }
                hist' = hist { present = ( layers, meta' ) }
            STM.writeTVar historyTVar hist'
            return hist'
          let newDebugText = Text.intercalate "\n" $ prettyLayers newContent newMetadata
          GTK.labelSetText layersContentDebugLabel newDebugText

        -- Connect signals for starting a drag from this widget.
        dragSource <- GTK.dragSourceNew

        void $ GTK.onDragSourcePrepare dragSource $ \ _x _y -> do
          ( _, layerItem ) <- treeListItemLayerItem listItem
          dat <- getLayerData layerItem

          mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
          treeListRow <- case mbTreeListRow of
              Nothing -> error "newLayerView ListItem onSetup: no TreeListRow"
              Just r -> return r

          srcPos <- GTK.treeListRowGetPosition treeListRow
          mbSrcPar <- GTK.treeListRowGetParent treeListRow
          mbParSrcPos <- traverse GTK.treeListRowGetPosition mbSrcPar

          let dragSourceData =
                DND_Data
                  { dnd_sourceUnique = layerUnique dat
                  , dnd_sourceGlobalIndex = srcPos
                  , dnd_sourceParentGlobalIndex = mbParSrcPos
                  }
          dragSourceDataPtr <- newStablePtr dragSourceData
          val <- GDK.contentProviderNewForValue =<< GIO.toGValue @( StablePtr DND_Data ) dragSourceDataPtr
          GTK.widgetAddCssClass window "dragging-item"
          return $ Just val
        void $ GTK.onDragSourceDragBegin dragSource $ \ _drag -> do
{- To set a cursor icon for the drag, write the x/y coordinates in the
   'prepare' signal handler to an IORef, and then use the following:
          ( x, y ) <- readIORef dragPosRef
          paintable <- GDK.widgetPaintableNew ( Just expander )
          GTK.dragSourceSetIcon ?self ( Just paintable ) ( round x ) ( round y )
-}
          noPaintable <- GDK.paintableNewEmpty 0 0
          GTK.dragSourceSetIcon ?self ( Just noPaintable) 0 0
          GTK.widgetAddCssClass expander "dragged"
          -- TODO: add "dragged" class for all descendants as well.
        void $ GTK.onDragSourceDragCancel dragSource $ \ _drag _reason ->
          return True
              -- ^^^^ Important. Setting this to 'False' stops GDK
              -- from properly clearing the drag cursor.
        void $ GTK.onDragSourceDragEnd dragSource $ \ _drag _deleteData -> do
          GTK.widgetRemoveCssClass window "dragging-item"
          GTK.widgetRemoveCssClass expander "dragged"

        -- Connect signals for receiving a drop on this widget.
        dropTarget <- GTK.dropTargetNew GI.gtypeStablePtr [ GDK.DragActionCopy ]

        let dropTargetCleanup = do
              GTK.widgetRemoveCssClass expander "drag-over"
              GTK.widgetRemoveCssClass expander "drag-top"
              GTK.widgetRemoveCssClass expander "drag-bot"
              mbNextItem <- getNextItem_maybe expander
              for_ mbNextItem $ \ nextItem -> do
                GTK.widgetRemoveCssClass nextItem "drag-top"
        void $ GTK.onDropTargetAccept dropTarget $ \ _drop -> do
          ( _, layerItem ) <- treeListItemLayerItem listItem
          dat <- getLayerData layerItem
          case dat of
            Group {} -> return True
            Layer {} -> return True
        void $ GTK.onDropTargetDrop dropTarget $ \ val _x y -> do

          ( _, layerItem ) <- treeListItemLayerItem listItem
          dstLayer <- getLayerData layerItem
          let dropTgtUniq = layerUnique dstLayer

          dragSrcDataPtr <- GIO.fromGValue @( StablePtr DND_Data ) val
          DND_Data
            { dnd_sourceUnique = dragSrcUniq
            , dnd_sourceGlobalIndex = dragSrcIx
            , dnd_sourceParentGlobalIndex = mbDragSrcParentIx
            } <- deRefStablePtr dragSrcDataPtr
          freeStablePtr dragSrcDataPtr

          mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
          treeListRow <- case mbTreeListRow of
              Nothing -> error "newLayerView ListItem onSetup: no TreeListRow"
              Just r -> return r

          isDescendent <- isDescendentOf dragSrcUniq listItem
          if isDescendent
          then return False
            -- Don't allow a drag onto ourselves or any of our descendents.
            -- TODO: if we are the last item in a group, a drag on the bottom
            -- part of ourselves could be used to mean "move out of group",
            -- which would be useful.
          else do
            h <- GTK.widgetGetHeight expander
            let droppedAbove = y < 0.5 * fromIntegral h

            expanded <- GTK.treeListRowGetExpanded treeListRow

            -- Retrieve the global index of the destination parent.
            -- The destination parent is usually the parent of the drop target,
            -- but when dropping below an expanded group, it is the group itself.
            dstPos <- GTK.treeListRowGetPosition treeListRow
            mbDstParPos <-
              if expanded && not droppedAbove
              then return $ Just dstPos
              else do
                mbDstPar <- GTK.treeListRowGetParent treeListRow
                traverse GTK.treeListRowGetPosition mbDstPar

            ( History { present = ( newContent, newMetadata ) }, ( srcChildIx, dstChildIx ) )
              <- STM.atomically $ do
                History { past, present = ( content@Content { layerHierarchy = hierarchy }, meta ) } <- STM.readTVar historyTVar
                let mbSrcParent = fst $ hierarchy ! Just dragSrcUniq
                    ( mbDstParent, mbDropTgtUniq )
                      | not droppedAbove && expanded
                      = ( Just dropTgtUniq, Nothing )
                      | otherwise
                      = ( fst $ hierarchy ! Just dropTgtUniq, Just dropTgtUniq )
                let ( hierarchy', dndCtxt ) = dragAndDropLayerUpdate dragSrcUniq ( mbDropTgtUniq, droppedAbove ) mbSrcParent mbDstParent hierarchy
                    content' = content { layerHierarchy = hierarchy' }
                    hist' = History { past = past Seq.:|> content, present = ( content', meta ), future = [] }
                STM.writeTVar historyTVar hist'
                return ( hist', dndCtxt )
            -- TODO: probably need to take a lock here to avoid funny business?
            putStrLn $ unlines
              [ "dnd"
              , "dragSrcUniq: " ++ show dragSrcUniq
              , "dropTgtUniq: " ++ show dropTgtUniq
              , "drop inside group: " ++ show ( expanded && not droppedAbove )
              , "newContent: " ++ show newContent
              ]
            dragAndDropListModelUpdate rootStore layersListModel ( dragSrcIx, mbDragSrcParentIx, srcChildIx ) ( mbDstParPos, dstChildIx )
            let newDebugText = Text.intercalate "\n" $ prettyLayers newContent newMetadata
            GTK.labelSetText layersContentDebugLabel newDebugText

            return True

        GTK.widgetAddCssClass expander "layer-item"
        void $ GTK.onDropTargetEnter dropTarget $ \ _x y -> do
          GTK.widgetAddCssClass expander "drag-over"
          h <- GTK.widgetGetHeight expander
          if y < 0.5 * fromIntegral h
          then GTK.widgetAddCssClass expander "drag-top"
          else do
            GTK.widgetAddCssClass expander "drag-bot"
            mbNextItem <- getNextItem_maybe expander
            for_ mbNextItem $ \ nextItem -> do
              GTK.widgetAddCssClass nextItem "drag-top"
          return [ GDK.DragActionCopy ]
        void $ GTK.onDropTargetMotion dropTarget $ \ _x y -> do
          h <- GTK.widgetGetHeight expander
          if y < 0.5 * fromIntegral h
          then do
            GTK.widgetRemoveCssClass expander "drag-bot"
            GTK.widgetAddCssClass expander "drag-top"
            mbNextItem <- getNextItem_maybe expander
            for_ mbNextItem $ \ nextItem -> do
              GTK.widgetRemoveCssClass nextItem "drag-top"
          else do
            GTK.widgetRemoveCssClass expander "drag-top"
            GTK.widgetAddCssClass expander "drag-bot"
            mbNextItem <- getNextItem_maybe expander
            for_ mbNextItem $ \ nextItem -> do
              GTK.widgetAddCssClass nextItem "drag-top"
          return [ GDK.DragActionCopy ]
        void $ GTK.onDropTargetLeave dropTarget $
          dropTargetCleanup

        GTK.widgetAddController expander dragSource
        GTK.widgetAddController expander dropTarget

  -- Connect to "bind" signal to modify the generic widget to display the data for this list item.
  _ <- GTK.onSignalListItemFactoryBind layersListFactory $ \ listItem0 -> do

        listItem <- GTK.unsafeCastTo GTK.ListItem listItem0
        mbExpander <- GTK.listItemGetChild listItem
        expander <-
          case mbExpander of
            Nothing -> error "layerView onBind: list item has no child"
            Just expander0 -> GTK.unsafeCastTo GTK.TreeExpander expander0

        LayerViewWidget
          { layerViewCheckButton = checkButton
          , layerViewLabel       = layerLabel
          } <- getLayerViewWidget expander

        ( _, layerItem ) <- treeListItemLayerItem listItem
        dat <- getLayerData layerItem
        ( _, meta ) <- present <$> STM.readTVarIO historyTVar

        let ( layerText, checkBoxStatusVisible ) =
              case dat of
                Layer { layerUnique } ->
                  layerNameAndVisible meta layerUnique
                Group { layerUnique } ->
                  layerNameAndVisible meta layerUnique

        mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
        treeListRow <- case mbTreeListRow of
          Nothing -> error "newLayerView ListItem onBind: no TreeListRow"
          Just r -> return r
        GTK.treeExpanderSetListRow expander ( Just treeListRow )

        case dat of
          Layer {} -> do
            GTK.widgetSetVisible checkButton True
            GTK.checkButtonSetActive checkButton checkBoxStatusVisible
          Group {} -> do
            GTK.widgetSetVisible checkButton True
            GTK.checkButtonSetActive checkButton checkBoxStatusVisible
        GTK.editableSetText layerLabel layerText

  selectionModel <- GTK.noSelectionNew ( Just layersListModel )
  GTK.listViewNew ( Just selectionModel ) ( Just layersListFactory )

getNextItem_maybe :: GTK.TreeExpander -> IO ( Maybe ( GTK.TreeExpander ) )
getNextItem_maybe expander = do
  mbParent <- GTK.widgetGetParent expander
  case mbParent of
    Nothing -> error "nextItem: item has no parent"
    Just parent -> do
      mbNextItemParent <- GTK.widgetGetNextSibling parent
      case mbNextItemParent of
        Nothing -> return Nothing
        Just nextItemParent -> do
          mbNextItem <- GTK.widgetGetFirstChild nextItemParent
          case mbNextItem of
            Nothing -> error "nextItem: next item has no child"
            Just nextItem0 -> do
              nextItem <- GTK.unsafeCastTo GTK.TreeExpander nextItem0
              return $ Just nextItem

isDescendentOf :: Unique -> GTK.ListItem -> IO Bool
isDescendentOf u listItem = do
  mbListRow <- GTK.listItemGetItem listItem
  case mbListRow of
    Nothing -> error "isDescendentOf: ListItem has no item"
    Just listRow0 -> do
      listRow <- GTK.unsafeCastTo GTK.TreeListRow listRow0
      go listRow
  where
    go :: GTK.TreeListRow -> IO Bool
    go listRow = do
      mbItem <- GTK.treeListRowGetItem listRow
      case mbItem of
        Nothing -> error "isDescendentOf: TreeListRow has no item"
        Just i -> do
          l <- GTK.unsafeCastTo LayerItem i
          mbDat <- GI.gobjectGetPrivateData l
          case mbDat of
            Nothing -> error "isDescendentOf: no private data"
            Just dat ->
              if layerUnique dat == u
              then return True
              else do
                mbPar <- GTK.treeListRowGetParent listRow
                case mbPar of
                  Nothing -> return False
                  Just par -> go par

--------------------------------------------------------------------------------

main :: IO ()
main = do
  setEnv "GDK_SCALE" "2"
  application <- GTK.applicationNew ( Just "com.layers" ) [ ]
  GIO.applicationRegister application ( Nothing @GIO.Cancellable )
  void $ GIO.onApplicationActivate application ( runApplication application )
  exitCode <- GIO.applicationRun application Nothing
  GObject.objectUnref application
  case exitCode of
    0 -> exitSuccess
    _ -> exitWith ( ExitFailure $ fromIntegral exitCode )

runApplication :: GTK.Application -> IO ()
runApplication application = do

  window <- GTK.applicationWindowNew application
  GTK.setWindowTitle window "Layers"
  GTK.windowSetDefaultSize window 1024 768

  display     <- GTK.rootGetDisplay window
  cssProvider <- GTK.cssProviderNew
  themePath   <- Directory.canonicalizePath =<< Cabal.getDataFileName "theme.css"
  GTK.cssProviderLoadFromPath cssProvider themePath
  GTK.styleContextAddProviderForDisplay display cssProvider 1000

  GTK.widgetAddCssClass window "gtk-layers"

  contentBox <- GTK.boxNew GTK.OrientationHorizontal 100
  layersBox  <- GTK.boxNew GTK.OrientationVertical 0
  GTK.boxAppend contentBox layersBox

  internalLayersLabel <- GTK.labelNew ( Just $ Text.intercalate "\n" $ uncurry prettyLayers $ present testInitialHistory )
  GTK.boxAppend contentBox internalLayersLabel


  uniqueTVar  <- STM.newTVarIO @Unique  testInitialUnique
  historyTVar <- STM.newTVarIO @History testInitialHistory

  ( rootStore, layers ) <- newLayersListModel historyTVar
  layersView <- newLayerView window uniqueTVar historyTVar internalLayersLabel rootStore layers
  GTK.listViewSetShowSeparators layersView False

  GTK.boxAppend layersBox layersView

  GTK.windowSetChild window ( Just contentBox )
  GTK.widgetSetVisible layersView True
  GTK.widgetSetVisible window True

  -- TODO: implement an undo/redo feature and make it update the UI appropriately.

  -- TODO: implement "new layer", "new group", "delete layer/group".

  void $ GTK.onApplicationQueryEnd application $
    GTK.applicationRemoveWindow application window

--------------------------------------------------------------------------------

--------------
-- App data --
--------------

newtype Unique = Unique { unique :: Word64 }
  deriving newtype ( Eq, Ord, Enum, GTK.IsGValue )
instance Show Unique where { show ( Unique i ) = "%" ++ show i }

type LayerHierarchy = Map ( Maybe Unique ) ( Maybe Unique, Maybe [ Unique ] )

data Content =
  Content
    { layerHierarchy :: !LayerHierarchy
    }
  deriving stock Show

data Meta =
  Meta { names :: Map Unique Text
       , invisibles :: Set Unique
       }

data Position
  = Top
  | TopOfGroup Unique
  -- | Below an item. For a group: below the whole group.
  | Below Unique

data History
  = History
  { past    :: !( Seq Content )
  , present :: !( Content, Meta )
  , future  :: ![ Content ]
  }

-----------------
-- GTK UI data --
-----------------

data DND_Data =
  DND_Data
    { dnd_sourceUnique :: !Unique
    , dnd_sourceGlobalIndex :: !Word32
    , dnd_sourceParentGlobalIndex :: !( Maybe Word32 )
    }
  deriving stock ( Show, Eq )

data Layer
  = Group { layerUnique :: !Unique }
  | Layer { layerUnique :: !Unique }

layerNameAndVisible :: Meta -> Unique -> ( Text, Bool )
layerNameAndVisible ( Meta { names, invisibles } ) unique =
  ( names ! unique, unique `notElem` invisibles )

prettyLayers :: Content -> Meta -> [ Text ]
prettyLayers ( Content { layerHierarchy } ) meta =
  concatMap go ( fromMaybe [] . snd $ layerHierarchy ! Nothing )
    where
      go :: Unique -> [ Text ]
      go i =
        let ( name, visible ) = layerNameAndVisible meta i
        in
          case snd $ layerHierarchy ! ( Just i ) of
            Nothing ->
              [ "- Layer { layerName = \"" <> name <> "\", layerVisible = " <> Text.pack ( show visible ) <> " } " ]
            Just cs ->
              [ "- Group { layerName = \"" <> name <> "\", layerVisible = " <> Text.pack ( show visible ) <> " }" ]
              ++ concatMap ( map ( "  " <> ) . go ) cs

newtype LayerItem = LayerItem ( GTK.ManagedPtr LayerItem )

instance GI.TypedObject LayerItem  where
  glibType = GI.registerGType LayerItem

instance GI.GObject LayerItem

instance GI.HasParentTypes LayerItem
type instance GI.ParentTypes LayerItem = '[ GObject.Object ]

instance GI.DerivedGObject LayerItem where
  type GObjectParentType  LayerItem = GObject.Object
  type GObjectPrivateData LayerItem = Maybe Layer
  objectTypeName = "ListStoreExample-LayerItem"
  objectClassInit _ = return ()
  objectInstanceInit _ _ = return Nothing
  objectInterfaces = [ ]

getLayerData :: HasCallStack => LayerItem -> IO Layer
getLayerData item = do
  mbDat <- GI.gobjectGetPrivateData item
  case mbDat of
    Nothing -> error "getPrivateData: no private data"
    Just dat -> return dat

--------------------------------------------------------------------------------
-- Drag and drop update.

-- TODO

dragAndDropLayerUpdate :: Unique -> ( Maybe Unique, Bool ) -> Maybe Unique -> Maybe Unique -> LayerHierarchy -> ( LayerHierarchy, ( Word32, Word32 ) )
dragAndDropLayerUpdate srcUniq ( mbTgtUniq, dropAbove ) mbSrcParent mbDstParent hierarchy =

  let
    ( oldPar_p, fromJust -> oldPar_cs ) = hierarchy ! mbSrcParent
    ( newPar_p, fromJust -> newPar_cs ) = hierarchy ! mbDstParent
    ( oldParent', oldChildPos ) =
      ( ( oldPar_p, Just $ ( filter ( /= srcUniq ) ) oldPar_cs )
      , case elemIndex srcUniq oldPar_cs of
          Nothing -> error $ unlines
            [ "dragAndDropLayerUpdate: old child not found"
            , "oldPar_cs: " ++ show oldPar_cs
            , "srcUniq: " ++ show srcUniq
            ]
          Just i -> fromIntegral i )
    ( newParent', newChildPos ) =
      case mbTgtUniq of
        -- Drop as first child of group.
        Nothing ->
          ( ( newPar_p, Just ( srcUniq : filter ( /= srcUniq ) newPar_cs ) ), 0 )
        -- Drop (before or after) given child.
        Just tgtUniq ->
          let ( bef, aft ) = break ( == tgtUniq ) $ filter ( /= srcUniq ) newPar_cs
          in ( ( newPar_p, Just $
                             if dropAbove
                             then bef ++ [ srcUniq ] ++ aft
                             else bef ++ take 1 aft ++ [ srcUniq ] ++ drop 1 aft
               ), fromIntegral $ length ( takeWhile ( /= tgtUniq ) newPar_cs ) + ( if dropAbove then 0 else 1 ) )

    hierarchy'
        -- Add the item to its new parent.
      = Map.insert mbDstParent newParent'
        -- Remove the item from its old parent.
      $ Map.insert mbSrcParent oldParent'
        -- Update the parent of the item.
      $ Map.adjust ( \ ( _, cs ) -> ( mbDstParent, cs ) ) ( Just srcUniq )
          hierarchy

  in
    ( hierarchy', ( oldChildPos, newChildPos ) )

dragAndDropListModelUpdate :: GIO.ListStore -> GTK.TreeListModel ->( Word32, Maybe Word32, Word32 ) -> ( Maybe Word32, Word32 ) -> IO ()
dragAndDropListModelUpdate rootStore layersListModel ( srcIx, mbSrcParentPos, srcChildIx ) ( mbDstParentPos, dstChildIx ) = do

  putStrLn $ unlines
    [ "dragAndDropListModelUpdate"
    , "srcIx: " ++ show srcIx
    , "mbSrcParentPos: " ++ show mbSrcParentPos
    , "srcChildIx: " ++ show srcChildIx
    , "mbDstParentPos: " ++ show mbDstParentPos
    , "dstChildIx: " ++ show dstChildIx
    ]

  treeListRow <- GTK.unsafeCastTo GTK.TreeListRow =<< fmap fromJust ( GIO.listModelGetItem layersListModel srcIx )
  item <- GTK.unsafeCastTo LayerItem =<< fmap fromJust ( GTK.treeListRowGetItem treeListRow )

  if ( mbSrcParentPos, srcChildIx ) < ( mbDstParentPos, dstChildIx )
  then do { insertDst item; removeSrc }
  else do { removeSrc     ; insertDst item }

  where
    removeSrc =
      case mbSrcParentPos of
        Nothing -> do
          GIO.listStoreRemove rootStore srcChildIx
        Just parentPos -> do
          parentRow <- fromJust <$> GTK.treeListModelGetRow layersListModel parentPos
          parentModel <- fromJust <$> GTK.treeListRowGetChildren parentRow
          parentStore <- GTK.unsafeCastTo GIO.ListStore parentModel
          GIO.listStoreRemove parentStore srcChildIx

    insertDst item =
      case mbDstParentPos of
        Nothing -> do
          GIO.listStoreInsert rootStore dstChildIx item
        Just parentPos -> do
          parentRow <- fromJust <$> GTK.treeListModelGetRow layersListModel parentPos
          parentModel <- fromJust <$> GTK.treeListRowGetChildren parentRow
          parentStore <- GTK.unsafeCastTo GIO.ListStore parentModel
          GIO.listStoreInsert parentStore dstChildIx item

--------------------------------------------------------------------------------
-- Sample data for illustration.

testContent :: Content
testContent =
  Content
    { layerHierarchy =
        Map.fromList
          [ ( Nothing           , ( Nothing, Just [ Unique 1, Unique 2, Unique 11 ] ) )
          , ( Just ( Unique 1  ), ( Nothing, Nothing ) )
          , ( Just ( Unique 2  ), ( Nothing, Just [ Unique 3, Unique 4, Unique 5, Unique 10 ] ) )
          , ( Just ( Unique 3  ), ( Just ( Unique 2 ), Nothing ) )
          , ( Just ( Unique 4  ), ( Just ( Unique 2 ), Nothing ) )
          , ( Just ( Unique 5  ), ( Just ( Unique 2 ), Just [ Unique 6, Unique 7, Unique 8, Unique 9 ] ) )
          , ( Just ( Unique 6  ), ( Just ( Unique 5 ), Nothing ) )
          , ( Just ( Unique 7  ), ( Just ( Unique 5 ), Nothing ) )
          , ( Just ( Unique 8  ), ( Just ( Unique 5 ), Just [ ] ) )
          , ( Just ( Unique 9  ), ( Just ( Unique 5 ), Nothing ) )
          , ( Just ( Unique 10 ), ( Just ( Unique 2 ), Nothing ) )
          , ( Just ( Unique 11 ), ( Nothing, Nothing ) )
          ]
    }

testInitialUnique :: Unique
testInitialUnique =
  succ $ maximum $ catMaybes $ Map.keys $ layerHierarchy testContent

testMeta :: Meta
testMeta = Meta
        { names = Map.fromList
                    [ ( Unique 1, "layer 1" )
                    , ( Unique 2, "group 1" )
                    ,   ( Unique 3, "layer 1.1" )
                    ,   ( Unique 4, "layer 1.2" )
                    ,   ( Unique 5, "group 1.3" )
                    ,     ( Unique 6, "layer 1.3.1" )
                    ,     ( Unique 7, "layer 1.3.2" )
                    ,     ( Unique 8, "group 1.3.3" )
                    ,     ( Unique 9, "layer 1.3.4" )
                    ,   ( Unique 10, "layer 1.4" )
                    , ( Unique 11, "layer 2" )
                    ]
        , invisibles = Set.fromList [ Unique 4, Unique 7 ]
        }
testInitialHistory :: History
testInitialHistory =
  History
    { past = mempty
    , present = ( testContent, testMeta )
    , future = mempty
    }

--------------------------------------------------------------------------------
