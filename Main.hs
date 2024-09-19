{-# LANGUAGE OverloadedLabels #-}

module Main where

-- base
import Control.Monad
  ( unless, void )
import Data.Foldable
  ( for_ )
import Data.List
  ( elemIndex )
import Data.Maybe
  ( catMaybes, fromMaybe, fromJust )
import Data.Word
  ( Word32, Word64 )
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
import qualified Data.GI.Base.GValue as GI
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
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Monad.STM as STM

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text

-- gtk-layers
import qualified Paths_gtk_layers as Cabal
  ( getDataFileName )

--------------------------------------------------------------------------------
-- Main application --
----------------------

main :: IO ()
main = do
  setEnv "GDK_SCALE" "2"
  --setEnv "GTK_DEBUG" "actions,tree,layout"
  --setEnv "GDK_DEBUG" "misc,events,input,dnd"
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
  GTK.windowSetDefaultSize window 640 480

  display     <- GTK.rootGetDisplay window
  cssProvider <- GTK.cssProviderNew
  themePath   <- Directory.canonicalizePath =<< Cabal.getDataFileName "theme.css"
  GTK.cssProviderLoadFromPath cssProvider themePath
  GTK.styleContextAddProviderForDisplay display cssProvider 1000

  GTK.widgetAddCssClass window "gtk-layers"

  contentBox <- GTK.boxNew GTK.OrientationHorizontal 100
  layersScrolledWindow <- GTK.scrolledWindowNew
  GTK.scrolledWindowSetPolicy layersScrolledWindow GTK.PolicyTypeNever GTK.PolicyTypeAutomatic

  GTK.boxAppend contentBox layersScrolledWindow

  internalLayersLabel <- GTK.labelNew ( Just $ uncurry prettyLayers $ present testInitialHistory )
  GTK.boxAppend contentBox internalLayersLabel


  uniqueTVar  <- STM.newTVarIO @Unique  testInitialUnique
  historyTVar <- STM.newTVarIO @History testInitialHistory

  ( rootStore, layers ) <- newLayersListModel historyTVar
  layersView <- newLayerView layersScrolledWindow uniqueTVar historyTVar internalLayersLabel rootStore layers
  GTK.listViewSetShowSeparators layersView False

  GTK.scrolledWindowSetChild layersScrolledWindow ( Just layersView )

  GTK.windowSetChild window ( Just contentBox )
  GTK.widgetSetVisible layersView True
  GTK.widgetSetVisible window True

  -- TODO: implement an undo/redo feature and make it update the UI appropriately.

  -- TODO: implement "new layer", "new group", "delete layer/group".

  void $ GTK.onApplicationQueryEnd application $
    GTK.applicationRemoveWindow application window

--------------------------------------------------------------------------------
-- Application and UI state --
------------------------------

--------------
-- App data --
--------------

-- | A unique identifier; can be used for any kind of object.
newtype Unique = Unique { unique :: Word64 }
  deriving newtype ( Eq, Ord, Enum )
instance Show Unique where { show ( Unique i ) = "%" ++ show i }

-- | The layer hierarchy.
--
-- This representation allows easy modifications, e.g. to move an item around
-- one simply needs to edit the map in two places, removing the item from its
-- old parent and adding it to its new parent.
--
-- This representation is chosen over a recursive data structure such as:
--
-- > type LayerHierarchy = [ Layer ]
-- > data Layer = Layer { .. } | Group { .., groupChildren :: LayerHierarchy }
--
-- as such a representation is more difficult to edit (in particular when
-- deeply nested items are involved).
type LayerHierarchy = Map ( Parent Unique ) ( Maybe [ Unique ] )
  -- TODO: use IntMap

-- | The application state, with history for undo/redo.
data History
  = History
  { past    :: !( Seq ( Content, Diff ) )
  , present :: !( Content, Meta )
  , future  :: ![ ( Diff, Content ) ]
  }

-- | Main application content, subject to undo/redo.
data Content =
  Content
    { layerHierarchy :: !LayerHierarchy
    -- layerData :: !( Map Unique SomeData )
    }
  deriving stock Show

-- | Auxilary metadata about the application, not subject to undo/redo.
data Meta =
  Meta
    { names :: !( Map Unique Text )
    , invisibles :: !( Set Unique )
    }

-- | A change in state that can be undone/redone.
--
-- This is used to keep the GTK state in sync when we move around the history.
data Diff
  -- | Move a layer or group.
  = Move
    { moveSrc :: !( FlatIndex, Child )
    , moveDst :: !( FlatIndex, Child )
    }
  -- TODO
  -- | New
  -- | Delete

-- | A parent of an item (layer or group).
data Parent a
  -- | The item is at the top level.
  = Root
  -- | The item is in this group.
  | InGroup !a
  deriving stock ( Show, Eq, Ord )

-- | A global index in a flattened tree list.
--
-- Used to refer to elements in the GTK TreeListModel; which flattens out
-- the whole hierarchy.
newtype FlatIndex = FlatIndex { flatIndex :: Word32 }
  deriving ( Eq, Ord )
  deriving stock Show

-- | The position of a child relative to a parent.
data Child
  = Child
    { childParent :: !( Parent FlatIndex )
        -- ^ The index of the parent in the TreeListModel.
    , childIndexInParent :: !Word32
        -- ^ The index of the child within the parent.
    }
  deriving stock ( Show, Eq, Ord )

-----------------
-- GTK UI data --
-----------------

-- | Data that gets passed during a drag-and-drop operation, describing
-- various properties of the **source** of the drag-and-drop operation.
data DND_Data =
  DND_Data
    { dnd_sourceUnique :: !Unique
    , dnd_sourceParentUnique :: !( Parent Unique )
    , dnd_sourceFlatIndex :: !FlatIndex
    , dnd_sourceParentFlatIndex :: !( Parent FlatIndex )
    }
  deriving stock ( Show, Eq )

-- | Look up the name and visibility of a layer from the metadata.
layerNameAndVisible :: Meta -> Unique -> ( Text, Bool )
layerNameAndVisible ( Meta { names, invisibles } ) unique =
  ( names Map.! unique, unique `notElem` invisibles )

-- | Display the layer hierarchy (for debugging purposes).
prettyLayers :: Content -> Meta -> Text
prettyLayers ( Content { layerHierarchy } ) meta =
  Text.intercalate "\n" $
    concatMap go ( fromMaybe [] $ layerHierarchy Map.! Root )
    where
      go :: Unique -> [ Text ]
      go i =
        let ( name, visible ) = layerNameAndVisible meta i
        in
          case layerHierarchy Map.! ( InGroup i ) of
            Nothing ->
              [ "- Layer { layerName = \"" <> name <> "\", layerVisible = " <> Text.pack ( show visible ) <> " } " ]
            Just cs ->
              [ "- Group { layerName = \"" <> name <> "\", layerVisible = " <> Text.pack ( show visible ) <> " }" ]
              ++ concatMap ( map ( "  " <> ) . go ) cs

-- | Custom GTK object used to hold layer data.
--
-- These are the items that will get stored in the ListModel used by GTK
-- to store the layer hierarchy data.
newtype LayerItem = LayerItem ( GTK.ManagedPtr LayerItem )

instance GI.TypedObject LayerItem  where
  glibType = GI.registerGType LayerItem

instance GI.GObject LayerItem

instance GI.HasParentTypes LayerItem
type instance GI.ParentTypes LayerItem = '[ GObject.Object ]


-- | Data associated to a 'LayerItem', for use in the GTK UI state.
data LayerID
  = GroupID { layerUnique :: !Unique }
  | LayerID { layerUnique :: !Unique }

instance GI.DerivedGObject LayerItem where
  type GObjectParentType  LayerItem = GObject.Object
  type GObjectPrivateData LayerItem = Maybe LayerID
  objectTypeName = "ListStoreExample-LayerItem"
  objectClassInit _ = return ()
  objectInstanceInit _ _ = return Nothing
  objectInterfaces = [ ]

--------------------------------------------------------------------------------
-- GTK TreeListModel --
-----------------------

-- | Create a new 'GTK.TreeListModel' from the given set of layers.
newLayersListModel :: STM.TVar History -> IO ( GIO.ListStore, GTK.TreeListModel )
newLayersListModel historyTVar = do
  itemType <- GI.glibType @LayerItem
  store <- GIO.listStoreNew itemType
  ( Content { layerHierarchy }, _ ) <- present <$> STM.readTVarIO historyTVar
  let topLayers = fromMaybe [] $ layerHierarchy Map.! Root
  for_ topLayers $ \ layerUniq -> do
    let mbChildren = layerHierarchy Map.! InGroup layerUniq
        layer = case mbChildren of
          Nothing -> LayerID { layerUnique = layerUniq }
          Just {} -> GroupID { layerUnique = layerUniq }
    item <- GI.unsafeCastTo LayerItem =<< GObject.objectNewv itemType []
    GI.gobjectSetPrivateData item ( Just layer )
    GIO.listStoreAppend store item

  rootModel <- GIO.toListModel store
  let passthrough = False
      auto_expand = True

  -- Pass a copy of the (reference to the) root model to the
  -- 'treeListModelNew' function to ensure we retain ownership of it.
  model <- GI.withManagedPtr rootModel $ \ rmPtr ->
           GI.withNewObject rmPtr $ \ rmCopy ->
             GTK.treeListModelNew rmCopy passthrough auto_expand createChildModel

  return ( store, model )

    where
      createChildModel :: GObject.Object -> IO ( Maybe GIO.ListModel )
      createChildModel parent = do
        dat <- getLayerData =<< GTK.unsafeCastTo LayerItem parent
        case dat of
          LayerID  {} -> return Nothing
          GroupID { layerUnique = groupUnique } -> do
            ( Content { layerHierarchy }, _ ) <- present <$> STM.readTVarIO historyTVar
            let children = fromMaybe [] $ layerHierarchy Map.! ( InGroup groupUnique )
            -- NB: create a simple GIO.ListStore, not a nested GTK.TreeListModel,
            -- as that would cause e.g. grand-children model to be created twice.
            itemType <- GI.glibType @LayerItem
            store <- GIO.listStoreNew itemType
            for_ children $ \ childUniq -> do
              let mbChildChildren = layerHierarchy Map.! ( InGroup childUniq )
                  childLayer = case mbChildChildren of
                    Nothing -> LayerID { layerUnique = childUniq }
                    Just {} -> GroupID { layerUnique = childUniq }
              item <- GI.unsafeCastTo LayerItem =<< GObject.objectNewv itemType []
              GI.gobjectSetPrivateData item ( Just childLayer )
              GIO.listStoreAppend store item
            Just <$> GIO.toListModel store

-- | Gets the 'LayerItem' for a row in a 'GTK.TreeListModel'
-- with @passthrough = False@.
treeListItemLayerItem :: GTK.ListItem -> IO LayerItem
treeListItemLayerItem listItem = do
  mbListRow <- GTK.listItemGetItem listItem
  case mbListRow of
    Nothing -> error "treeListItemLayerItem: ListItem has no item"
    Just listRow ->
      treeListRowLayerItem =<< GTK.unsafeCastTo GTK.TreeListRow listRow

-- | Retrieve the 'LayerItem' underlying a 'GTK.TreeListRow'.
treeListRowLayerItem :: GTK.TreeListRow -> IO LayerItem
treeListRowLayerItem listRow = do
  mbListRowItem <- GTK.treeListRowGetItem listRow
  case mbListRowItem of
    Nothing   -> error "treeListRowLayerItem: TreeListRow has no item"
    Just item -> GTK.unsafeCastTo LayerItem item
      -- NB: if you made the mistake of passing a 'createChildModel' function
      -- which recursively creates a TreeListModel, you would have to recurse
      -- into the row using 'treeListRowGetItem' to eventually get at the
      -- underlying data.

-- | Class for objects which wrap a 'LayerItem'.
class HasLayerData a where
  -- | Get the layer data associated to a 'LayerItem' or object
  -- containing a 'LayerItem'.
  getLayerData :: HasCallStack => a -> IO LayerID
instance HasLayerData LayerItem where

  getLayerData item = do
    mbDat <- GI.gobjectGetPrivateData item
    case mbDat of
      Nothing -> error "getLayerData: no private data"
      Just dat -> return dat
instance HasLayerData GTK.TreeListRow where
  getLayerData row = do
    parLayerItem <- treeListRowLayerItem row
    getLayerData parLayerItem
instance HasLayerData GTK.ListItem where
  getLayerData listItem = do
    layerItem <- treeListItemLayerItem listItem
    getLayerData layerItem

-----------------------
-- GTK layer widgets --
-----------------------

-- | The generic widget used to display a list item.
--
-- Structure:
--
--  - ListItem
--    - TreeExpander
--      - ContentBox
--        - CheckButton
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

------------------
-- GTK ListView --
------------------

-- | Create a new 'GTK.ListView' that displays 'LayerItem's.
newLayerView :: GTK.IsWidget layerViewContainer
             => layerViewContainer
             -> STM.TVar Unique
             -> STM.TVar History
             -> GTK.Label
             -> GIO.ListStore
             -> GTK.TreeListModel
             -> IO GTK.ListView
newLayerView layersContainer _uniqueTVar historyTVar layersContentDebugLabel rootStore layersListModel = do

  -- This TMVar is used to ensure that the layer hierarchy data
  -- is in sync between the application and the UI (GTK TreeListModel).
  listModelUpToDateTMVar <- STM.newTMVarIO ()

  layersListFactory <- GTK.signalListItemFactoryNew

  -- Connect to "setup" signal to create generic widgets for viewing the tree.
  --
  -- We attach a collection of signals to each widget,
  -- to handle e.g. drag-and-drop operations.
  --
  -- We attach the signals in the "setup" phase, because we don't want
  -- to have to keep attaching/removing event controllers to the widgets
  -- (in the "bind" and "unbind" stages).
  --
  -- However, in the "setup" phase, we don't yet know which underlying ListModel
  -- item we are displaying (this is only set on "bind").
  -- So: how can we set signal handlers in the "setup" phase? The answer is that
  -- each signal handler will read the private data associated to the widget;
  -- this data gets set when binding the widget to its ListModel item.
  _ <- GTK.onSignalListItemFactorySetup layersListFactory $ \ listItem0 -> do

        listItem <- GTK.unsafeCastTo GTK.ListItem listItem0
        GTK.listItemSetFocusable listItem False

        expander <- newLayerViewWidget
        GTK.listItemSetChild listItem ( Just expander )
        GTK.widgetAddCssClass expander "layer-item"

        LayerViewWidget
          { layerViewLabel = label }
            <- getLayerViewWidget expander

        -------------------
        -- EditableLabel --
        -------------------

        -- Connect a signal for editing the layer name.
        --
        -- NB: we don't use the 'onEditableChanged' signal, as that updates
        -- after every key stroke.
        void $ GI.after label ( GI.PropertyNotify #hasFocus ) $ \ _ -> do
          newText <- GTK.editableGetText label
          dat <- getLayerData listItem
          History { present = ( newContent, newMetadata ) } <- STM.atomically $ do
            hist@History { present = ( layers, meta@( Meta { names } ) ) } <- STM.readTVar historyTVar
            let names' = case dat of { LayerID { layerUnique = u } -> Map.insert u newText names
                                     ; GroupID { layerUnique = u } -> Map.insert u newText names }
                meta' = meta { names = names' }
                hist' = hist { present = ( layers, meta' ) }
            STM.writeTVar historyTVar hist'
            return hist'
          let newDebugText = prettyLayers newContent newMetadata
          GTK.labelSetText layersContentDebugLabel newDebugText

        ----------------
        -- DragSource --
        ----------------

        -- Connect signals for starting a drag from this widget.
        dragSource <- GTK.dragSourceNew
        GTK.dragSourceSetActions dragSource [ GDK.DragActionCopy ]

        void $ GTK.onDragSourcePrepare dragSource $ \ _x _y -> do
          dat <- getLayerData listItem

          mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
          treeListRow <- case mbTreeListRow of
              Nothing -> error "newLayerView ListItem onSetup: no TreeListRow"
              Just r -> return r

          srcPos <- FlatIndex <$> GTK.treeListRowGetPosition treeListRow
          mbSrcPar <- GTK.treeListRowGetParent treeListRow
          mbSrcParUniq <- traverse ( fmap layerUnique . getLayerData ) mbSrcPar
          mbParSrcPos <- fmap FlatIndex <$> traverse GTK.treeListRowGetPosition mbSrcPar

          let dragSourceData =
                DND_Data
                  { dnd_sourceUnique = layerUnique dat
                  , dnd_sourceParentUnique = maybe Root InGroup mbSrcParUniq
                  , dnd_sourceFlatIndex = srcPos
                  , dnd_sourceParentFlatIndex = maybe Root InGroup mbParSrcPos
                  }
          val <- GDK.contentProviderNewForValue =<< GIO.toGValue ( GI.HValue dragSourceData )
          GTK.widgetAddCssClass layersContainer "dragging-item"
          return $ Just val
        void $ GTK.onDragSourceDragBegin dragSource $ \ _drag -> do

{- To set a cursor icon for the drag, write the x/y coordinates in the
   'prepare' signal handler to an IORef, and then use the following:
          ( x, y ) <- readIORef dragPosRef
          paintable <- GDK.widgetPaintableNew ( Just expander )
          GTK.dragSourceSetIcon ?self ( Just paintable ) ( round x ) ( round y )
-}
          noPaintable <- GDK.paintableNewEmpty 0 0
          GTK.dragSourceSetIcon ?self ( Just noPaintable ) 0 0
          GTK.widgetAddCssClass expander "dragged"
          -- TODO: add "dragged" class for all descendants as well.
        void $ GTK.onDragSourceDragCancel dragSource $ \ _drag _reason -> do
          GTK.widgetRemoveCssClass layersContainer "dragging-item"
          GTK.widgetRemoveCssClass expander "dragged"
          return True
              -- ^^^^ Important. Setting this to 'False' stops GDK
              -- from properly clearing the drag cursor.
        void $ GTK.onDragSourceDragEnd dragSource $ \ _drag _deleteData -> do
          GTK.widgetRemoveCssClass layersContainer "dragging-item"
          GTK.widgetRemoveCssClass expander "dragged"

        ----------------
        -- DropTarget --
        ----------------

        -- Connect signals for receiving a drop on this widget.
        dropTarget <- GTK.dropTargetNew GI.gtypeHValue [ GDK.DragActionCopy ]

        let dropTargetCleanup = do
              GTK.widgetRemoveCssClass expander "drag-over"
              GTK.widgetRemoveCssClass expander "drag-top"
              GTK.widgetRemoveCssClass expander "drag-bot"
              mbNextItem <- getNextItem_maybe expander
              for_ mbNextItem $ \ nextItem -> do
                GTK.widgetRemoveCssClass nextItem "drag-top"
        void $ GTK.onDropTargetAccept dropTarget $ \ _drop -> do
          return True
          --dat <- getLayerData listItem
          --case dat of
          --  GroupID {} -> return True
          --  LayerID {} -> return True
        void $ GTK.onDropTargetDrop dropTarget $ \ val _x y -> do
          dropTargetCleanup
          dstLayer <- getLayerData listItem
          let dropTgtUniq = layerUnique dstLayer

          GI.HValue
            ( DND_Data
              { dnd_sourceUnique = dragSrcUniq
              , dnd_sourceParentUnique = dragSrcParUniq
              , dnd_sourceFlatIndex = dragSrcIx
              , dnd_sourceParentFlatIndex = dragSrcParPos
              } ) <- GIO.fromGValue @( GI.HValue DND_Data ) val

          mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
          treeListRow <- case mbTreeListRow of
              Nothing -> error "newLayerView ListItem onSetup: no TreeListRow"
              Just r -> return r

          isDescendent <- isDescendentOf dragSrcUniq listItem
          if isDescendent
          then do
            return False
            -- Don't allow a drag onto ourselves or any of our descendents.
            -- TODO: if we are the last item in a group, a drag on the bottom
            -- part of ourselves could be used to mean "move out of group",
            -- which would be useful.
          else do
            h <- GTK.widgetGetHeight expander
            let droppedAbove = y < 0.5 * fromIntegral h

            expanded <- GTK.treeListRowGetExpanded treeListRow

            -- Retrieve the flat index of the destination parent.
            -- The destination parent is usually the parent of the drop target,
            -- but when dropping below an expanded group, it is the group itself.
            dstPos <- FlatIndex <$> GTK.treeListRowGetPosition treeListRow
            ( mbDstParUniq, dragDstIx, dstParPos ) <-
              if expanded && not droppedAbove
              then return ( Nothing, FlatIndex 0, InGroup dstPos )
              else do
                mbDstPar <- GTK.treeListRowGetParent treeListRow
                mbParPos <- fmap FlatIndex <$> traverse GTK.treeListRowGetPosition mbDstPar
                mbDstParUniq <- traverse ( fmap layerUnique . getLayerData ) mbDstPar
                return ( mbDstParUniq, dstPos, maybe Root InGroup mbParPos )

            ( History { present = ( newContent, newMetadata ) }, ( srcChildIx, dstChildIx ) )
              <- STM.atomically $ do
                -- Ensure the GTK ListModel is up to date before continuing
                -- (just a precaution).
                STM.takeTMVar listModelUpToDateTMVar
                History { past, present = ( content@Content { layerHierarchy = hierarchy }, meta ) } <- STM.readTVar historyTVar
                let ( mbDstParent, mbDropTgtUniq )
                      | not droppedAbove && expanded
                      = ( InGroup dropTgtUniq, Nothing )
                      | otherwise
                      = ( maybe Root InGroup mbDstParUniq, Just dropTgtUniq )
                let ( hierarchy', ( srcChildIx, dstChildIx ) ) =
                      dragAndDropLayerUpdate
                        ( dragSrcParUniq, dragSrcUniq )
                        ( mbDstParent, mbDropTgtUniq, droppedAbove )
                        hierarchy
                    content' = content { layerHierarchy = hierarchy' }
                    diff = Move
                           { moveSrc = ( dragSrcIx, Child dragSrcParPos srcChildIx )
                           , moveDst = ( dragDstIx, Child dstParPos dstChildIx )
                           }
                    hist' = History { past = past Seq.:|> ( content, diff )
                                    , present = ( content', meta )
                                    , future = []
                                    }
                STM.writeTVar historyTVar hist'
                return ( hist', ( srcChildIx, dstChildIx ) )

            dragAndDropListModelUpdate rootStore layersListModel ( dragSrcIx, Child dragSrcParPos srcChildIx ) ( Child dstParPos dstChildIx )
            let newDebugText = prettyLayers newContent newMetadata
            GTK.labelSetText layersContentDebugLabel newDebugText
            STM.atomically $ STM.writeTMVar listModelUpToDateTMVar ()
            return True

        void $ GTK.onDropTargetEnter dropTarget $ \ _x y -> do
          GTK.widgetAddCssClass expander "drag-over"
          h <- GTK.widgetGetHeight expander
          if y < 0.5 * fromIntegral h
          then do
            GTK.widgetAddCssClass expander "drag-top"
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
        void $ GTK.onDropTargetLeave dropTarget $ do
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

        dat <- getLayerData listItem
        ( _content, meta ) <- present <$> STM.readTVarIO historyTVar

        -- All we do is set the name and visibility of this layer/group.

        let ( layerText, checkBoxStatusVisible ) =
              case dat of
                LayerID { layerUnique } ->
                  layerNameAndVisible meta layerUnique
                GroupID { layerUnique } ->
                  layerNameAndVisible meta layerUnique

        mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
        treeListRow <- case mbTreeListRow of
          Nothing -> error "newLayerView ListItem onBind: no TreeListRow"
          Just r -> return r
        GTK.treeExpanderSetListRow expander ( Just treeListRow )

        case dat of
          LayerID {} -> do
            GTK.widgetSetVisible checkButton True
            GTK.checkButtonSetActive checkButton checkBoxStatusVisible
          GroupID {} -> do
            GTK.widgetSetVisible checkButton True
            GTK.checkButtonSetActive checkButton checkBoxStatusVisible
        GTK.editableSetText layerLabel layerText

  -- Pass a copy of the (reference to the) tree list model to the
  -- selection model creation function to ensure we retain ownership of it.
  selectionModel <- GI.withManagedPtr layersListModel $ \ lmPtr ->
                    GI.withNewObject lmPtr $ \ lmCopy ->
                    GTK.noSelectionNew ( Just lmCopy )
  GTK.listViewNew ( Just selectionModel ) ( Just layersListFactory )

-- | Get the next item in the flattened tree, if any.
-- Ignores any notion of parent/child.
--
-- This is used to style the "item below" the current drop target,
-- to account for the fact that the item below is rendered on top of the item
-- above it, which overdraws any shadow/glow extending downwards on the latter.
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

-- | Is this list item a descendent of the item with the given unique?
isDescendentOf :: Unique -- ^ are we a descendent of this?
               -> GTK.ListItem -- ^ item we are querying
               -> IO Bool
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
      dat <- getLayerData listRow
      if layerUnique dat == u
      then return True
      else do
        mbPar <- GTK.treeListRowGetParent listRow
        case mbPar of
          Nothing -> return False
          Just par -> go par

--------------------------------------------------------------------------------
-- Drag and drop update.

-- | Update the 'LayerHierarchy' after a drag-and-drop operation.
--
-- This handles the application side logic.
-- The UI side is handled in 'dragAndDropListModelUpdate'.
--
-- Returns an updated 'LayerHierarchy', together with the relative indices
-- of the source and destination items within their respective parents.
-- This information is then used by GTK to update the underlying 'ListModel'.
dragAndDropLayerUpdate
  :: ( Parent Unique, Unique )
    -- ^ source
  -> ( Parent Unique, Maybe Unique, Bool )
    -- ^ destination (boolean: "dropped above")
  -> LayerHierarchy
    -- ^ hierarchy to update
  -> ( LayerHierarchy, ( Word32, Word32 ) )
dragAndDropLayerUpdate ( mbSrcParent, srcUniq ) ( mbDstParent, mbTgtUniq, dropAbove ) hierarchy =

  let
    ( fromJust -> oldPar_cs ) = hierarchy Map.! mbSrcParent
    ( fromJust -> newPar_cs ) = hierarchy Map.! mbDstParent
    ( oldParent', oldChildPos ) =
      ( ( Just $ ( filter ( /= srcUniq ) ) oldPar_cs )
      , fromIntegral $ fromJust $ elemIndex srcUniq oldPar_cs )
    ( newParent', newChildPos ) =
      case mbTgtUniq of
        -- Drop as first child of group.
        Nothing ->
          ( Just ( srcUniq : filter ( /= srcUniq ) newPar_cs )
          , 0 )
        -- Drop (before or after) given child.
        Just tgtUniq ->
          let ( bef, aft ) = break ( == tgtUniq ) $ filter ( /= srcUniq ) newPar_cs
          in ( Just $
                if dropAbove
                then bef ++ [ srcUniq ] ++ aft
                else bef ++ take 1 aft ++ [ srcUniq ] ++ drop 1 aft
              , fromIntegral $ length ( takeWhile ( /= tgtUniq ) $ newPar_cs ) + ( if dropAbove then 0 else 1 ) )

    hierarchy'
        -- Add the item to its new parent.
      = Map.insert mbDstParent newParent'
        -- Remove the item from its old parent.
      $ Map.insert mbSrcParent oldParent' hierarchy

  in
    ( hierarchy', ( oldChildPos, newChildPos ) )

-- | Update the GTK ListModel backing the layer hierarchy after a drag-and-drop operation.
--
-- See also 'dragAndDropLayerUpdate', which updates the application state
-- instead of the UI state.
dragAndDropListModelUpdate :: GIO.ListStore -> GTK.TreeListModel -> ( FlatIndex, Child ) -> Child -> IO ()
dragAndDropListModelUpdate rootStore layersListModel ( srcIx, src@( Child srcParentPos srcChildIx) ) dst@( Child dstParentPos dstChildIx ) = do

{-
  putStrLn $ unlines
    [ "dragAndDropListModelUpdate"
    , "srcIx: " ++ show srcIx
    , "srcParentPos: " ++ show srcParentPos
    , "srcChildIx: " ++ show srcChildIx
    , "dstParentPos: " ++ show dstParentPos
    , "dstChildIx: " ++ show dstChildIx
    ]
-}

  treeListRow <- fromJust <$> GIO.listModelGetItem layersListModel ( flatIndex srcIx )
  item <- treeListRowLayerItem =<< GTK.unsafeCastTo GTK.TreeListRow treeListRow

  if src < dst
  then do { insertDst item; removeSrc }
  else do { removeSrc; insertDst item }

  where
    removeSrc =
      case srcParentPos of
        Root -> do
          GIO.listStoreRemove rootStore srcChildIx
        InGroup parentPos -> do
          parentRow <- fromJust <$> GTK.treeListModelGetRow layersListModel ( flatIndex parentPos )
          parentModel <- fromJust <$> GTK.treeListRowGetChildren parentRow
          parentStore <- GTK.unsafeCastTo GIO.ListStore parentModel
          GIO.listStoreRemove parentStore srcChildIx

    insertDst item =
      case dstParentPos of
        Root -> do
          GIO.listStoreInsert rootStore dstChildIx item
        InGroup parentPos -> do
          parentRow <- fromJust <$> GTK.treeListModelGetRow layersListModel ( flatIndex parentPos )
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
          -- NB: this should never be constructed by hand (too error-prone);
          -- this is just for demonstration purposes.
          [ ( Root                 , Just [ Unique 1, Unique 2, Unique 11, Unique 12, Unique 13, Unique 14 ] )
          , ( InGroup ( Unique 1  ), Nothing )
          , ( InGroup ( Unique 2  ), Just [ Unique 3, Unique 4, Unique 5, Unique 10 ] )
          , ( InGroup ( Unique 3  ), Nothing )
          , ( InGroup ( Unique 4  ), Nothing )
          , ( InGroup ( Unique 5  ), Just [ Unique 6, Unique 7, Unique 8, Unique 9 ] )
          , ( InGroup ( Unique 6  ), Nothing )
          , ( InGroup ( Unique 7  ), Nothing )
          , ( InGroup ( Unique 8  ), Just [ ] )
          , ( InGroup ( Unique 9  ), Nothing )
          , ( InGroup ( Unique 10 ), Nothing )
          , ( InGroup ( Unique 11 ), Nothing )
          , ( InGroup ( Unique 12 ), Nothing )
          , ( InGroup ( Unique 13 ), Nothing )
          , ( InGroup ( Unique 14 ), Just [ Unique 15, Unique 16, Unique 17 ] )
          , ( InGroup ( Unique 15 ), Nothing )
          , ( InGroup ( Unique 16 ), Nothing )
          , ( InGroup ( Unique 17 ), Just [ Unique 18 ] )
          , ( InGroup ( Unique 18 ), Nothing )
          ]
    }

testInitialUnique :: Unique
testInitialUnique
  = succ
  $ maximum
  $ catMaybes
  $ fmap ( \ case { Root -> Nothing; InGroup i -> Just i } )
  $ Map.keys
  $ layerHierarchy testContent

testMeta :: Meta
testMeta = Meta
        { names = Map.fromList
                    [ ( Unique 1, "layer 1" )
                    , ( Unique 2, "group 2" )
                    ,   ( Unique 3, "layer 2.1" )
                    ,   ( Unique 4, "layer 2.2" )
                    ,   ( Unique 5, "group 2.3" )
                    ,     ( Unique 6, "layer 2.3.1" )
                    ,     ( Unique 7, "layer 2.3.2" )
                    ,     ( Unique 8, "group 2.3.3" )
                    ,     ( Unique 9, "layer 2.3.4" )
                    ,   ( Unique 10, "layer 2.4" )
                    , ( Unique 11, "layer 3" )
                    , ( Unique 12, "layer 4" )
                    , ( Unique 13, "layer 5" )
                    , ( Unique 14, "group 6" )
                    ,   ( Unique 15, "layer 6.1" )
                    ,   ( Unique 16, "layer 6.2" )
                    ,   ( Unique 17, "group 6.3" )
                    ,     ( Unique 18, "layer 6.3.1" )
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
