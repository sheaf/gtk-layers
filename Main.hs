{-# LANGUAGE OverloadedLabels #-}

module Main where

-- base
import Control.Monad
  ( unless, void )
import Data.Foldable
  ( for_, traverse_ )
import Data.List
  ( elemIndex )
import Data.Maybe
  ( catMaybes, fromMaybe, fromJust )
import Data.Traversable
  ( for )
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
  --setEnv "GSK_RENDERER" "gl"
  --setEnv "GTK_DEBUG" "" --"text,actions,geometry,tree,layout"
  --setEnv "GDK_DEBUG" "events" --"misc,events,frames,dnd"
  --setEnv "GSK_DEBUG" "full-redraw"
  application <- GTK.applicationNew ( Just "com.layers" ) [ ]
  GIO.applicationRegister application ( Nothing @GIO.Cancellable )
  void $ GIO.onApplicationActivate application ( runApplication application )
  exitCode <- GIO.applicationRun application Nothing
  GObject.objectUnref application
  case exitCode of
    0 -> exitSuccess
    _ -> exitWith ( ExitFailure $ fromIntegral exitCode )

-- | UI elements that might need to be dynamically edited.
data UIElements
  = UIElements
  { layersContainer :: !GTK.ScrolledWindow
  , layersDebugLabel :: !GTK.Label
  , undoButton, redoButton :: !GTK.Button
  , layersListModel :: !GTK.TreeListModel
  , layersRootStore :: !GIO.ListStore
  }

-- | Global state of the application.
data Variables
  = Variables
  { uniqueTVar  :: !( STM.TVar Unique )
  , historyTVar :: !( STM.TVar History )

    -- | This TMVar is used to ensure that the layer hierarchy data
    -- is in sync between the application and the UI (GTK TreeListModel).
  , listModelUpToDateTMVar :: !( STM.TMVar () )
  }

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

  uniqueTVar  <- STM.newTVarIO @Unique  testInitialUnique
  historyTVar <- STM.newTVarIO @History testInitialHistory
  listModelUpToDateTMVar <- STM.newTMVarIO ()
  let variables = Variables { uniqueTVar, historyTVar, listModelUpToDateTMVar }

  contentPane <- GTK.panedNew GTK.OrientationHorizontal
  -- Set the paned handle to "wide", otherwise the handle can eat input around it.
  GTK.panedSetWideHandle contentPane True
  layersScrolledWindow <- GTK.scrolledWindowNew
  GTK.scrolledWindowSetPolicy layersScrolledWindow GTK.PolicyTypeNever GTK.PolicyTypeAutomatic

  leftBox <- GTK.boxNew GTK.OrientationVertical 10
  buttonsBox <- GTK.boxNew GTK.OrientationHorizontal 10
  GTK.widgetSetValign buttonsBox GTK.AlignEnd
  GTK.widgetSetHalign buttonsBox GTK.AlignCenter
  GTK.widgetSetMarginBottom leftBox 10

  layersDebugLabel <- GTK.labelNew ( Just $ uncurry prettyLayers $ present testInitialHistory )
  GTK.widgetSetVexpand layersDebugLabel True
  GTK.boxAppend leftBox layersDebugLabel
  GTK.boxAppend leftBox buttonsBox

  GTK.panedSetStartChild contentPane ( Just leftBox )
  GTK.panedSetEndChild   contentPane ( Just layersScrolledWindow )

  newLayerButton    <- GTK.buttonNewWithLabel "New layer"
  newGroupButton    <- GTK.buttonNewWithLabel "New group"
  deleteLayerButton <- GTK.buttonNewWithLabel "Delete"
  undoButton        <- GTK.buttonNewWithLabel "Undo"
  redoButton        <- GTK.buttonNewWithLabel "Redo"

  traverse_ ( GTK.boxAppend buttonsBox )
    [ newLayerButton, newGroupButton, deleteLayerButton, undoButton, redoButton ]

  -- Undo/redo buttons should only be active when there is something to undo/redo.
  traverse_ ( \ button -> GTK.widgetSetSensitive button False )
    [ undoButton, redoButton ]

  ( layersRootStore, layersListModel ) <- newLayersListModel historyTVar

  let uiElts =
        UIElements { layersContainer = layersScrolledWindow
                   , layersDebugLabel
                   , undoButton, redoButton
                   , layersListModel, layersRootStore
                   }

  layersView <- newLayerView uiElts variables
  GTK.listViewSetShowSeparators layersView False

  GTK.scrolledWindowSetChild layersScrolledWindow ( Just layersView )

  void $ GTK.onButtonClicked newLayerButton $ do
    u <- getUnique uniqueTVar
    mbSelectedPos <- getSelectedPosition uiElts layersView
    let change = NewLayer { newUnique = u, newIsGroup = False, newSelected = mbSelectedPos }
    updateLayerHierarchy uiElts variables ( DoChange change )
  void $ GTK.onButtonClicked newGroupButton $ do
    u <- getUnique uniqueTVar
    mbSelectedPos <- getSelectedPosition uiElts layersView
    let change = NewLayer { newUnique = u, newIsGroup = True, newSelected = mbSelectedPos }
    updateLayerHierarchy uiElts variables ( DoChange change )
  void $ GTK.onButtonClicked deleteLayerButton $ do
    mbSelectedPos <- getSelectedPosition uiElts layersView
    case mbSelectedPos of
      Nothing ->
        -- TODO: the delete button should not be active if no item is selected.
        putStrLn "No item selected; not deleting."
      Just pos -> do
        let change = Delete { deletePosition = pos }
        updateLayerHierarchy uiElts variables ( DoChange change )
  void $ GTK.onButtonClicked undoButton $ do
    updateLayerHierarchy uiElts variables UndoChange
  void $ GTK.onButtonClicked redoButton $ do
    updateLayerHierarchy uiElts variables RedoChange

  GTK.windowSetChild window ( Just contentPane )
  GTK.widgetSetVisible layersView True
  GTK.widgetSetVisible window True

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

-- | Get the next 'Unique', incrementing the counter by one.
getUnique :: STM.TVar Unique -> IO Unique
getUnique uniqueTVar =
  STM.atomically $
    STM.stateTVar uniqueTVar ( \ old -> ( old, succ old ) )

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

-- | A parent of an item.
data Parent a
  -- | The item is at the top level.
  = Root
  -- | The item has this parent.
  | Parent !a
  deriving stock ( Show, Eq, Ord, Functor )

-- | A global index in a flattened tree list.
--
-- Used to refer to elements in the GTK TreeListModel; which flattens out
-- the whole hierarchy.
--
-- These indices are ephemereal, as expanding/collapsing tree expanders can
-- cause these to change. Use 'getItemListStore' to turn these ephemereal
-- indices into something more stable.
newtype EphemerealFlatIndex = EphemerealFlatIndex { ephemerealFlatIndex :: Word32 }
  deriving ( Eq, Ord, Enum )
  deriving stock Show

-- | The position of a child relative to a parent.
data ChildPosition
  = ChildPosition
    { childParent :: !GIO.ListStore
        -- ^ The parent list store for this item.
    , childIndexInParent :: !Word32
        -- ^ The index of the child within the parent.
    }
  deriving stock Eq

-----------------
-- GTK UI data --
-----------------

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
          case layerHierarchy Map.! Parent i of
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
  deriving stock ( Eq, Show )

instance GI.DerivedGObject LayerItem where
  type GObjectParentType  LayerItem = GObject.Object
  type GObjectPrivateData LayerItem = Maybe LayerID
  objectTypeName = "gtk-layers-LayerItem"
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
    let mbChildren = layerHierarchy Map.! Parent layerUniq
        layer = case mbChildren of
          Nothing -> LayerID { layerUnique = layerUniq }
          Just {} -> GroupID { layerUnique = layerUniq }
    item <- GI.unsafeCastTo LayerItem =<< GI.new LayerItem []
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
            let children = fromMaybe [] $ layerHierarchy Map.! Parent groupUnique
            -- NB: create a simple GIO.ListStore, not a nested GTK.TreeListModel,
            -- as that would cause e.g. grand-children model to be created twice.
            itemType <- GI.glibType @LayerItem
            store <- GIO.listStoreNew itemType
            for_ children $ \ childUniq -> do
              let mbChildChildren = layerHierarchy Map.! Parent childUniq
                  childLayer = case mbChildChildren of
                    Nothing -> LayerID { layerUnique = childUniq }
                    Just {} -> GroupID { layerUnique = childUniq }
              item <- GI.unsafeCastTo LayerItem =<< GI.new LayerItem []
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
newLayerView :: UIElements -> Variables -> IO GTK.ListView
newLayerView uiElts@( UIElements { .. } ) variables@( Variables { .. } )  = do

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
          { layerViewLabel = label
          , layerViewCheckButton = visibleButton }
            <- getLayerViewWidget expander

        ----------------------------
        -- Visibility CheckButton --
        ----------------------------

        void $ GTK.onCheckButtonToggled visibleButton $ do
          dat <- getLayerData listItem
          visible <- GTK.checkButtonGetActive ?self
          let uniq = layerUnique dat
          ( newContent, newMetadata ) <- STM.atomically $ do
            hist@History { present = ( content, meta@( Meta { invisibles } ) ) } <- STM.readTVar historyTVar
            let invisibles'
                  | visible
                  = Set.delete uniq invisibles
                  | otherwise
                  = Set.insert uniq invisibles
                meta' = meta { invisibles = invisibles' }
                hist' = hist { present = ( content, meta' ) }
            STM.writeTVar historyTVar hist'
            return ( content, meta' )
          let newDebugText = prettyLayers newContent newMetadata
          GTK.labelSetText layersDebugLabel newDebugText

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
          GTK.labelSetText layersDebugLabel newDebugText

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
          ( srcParPos, srcParStore ) <- getParentPosition uiElts treeListRow

          let dnd_sourcePosition =
                Position
                  { parentPosition = ( srcParPos, srcParStore )
                  , position = dat
                  }

          val <- GDK.contentProviderNewForValue =<< GIO.toGValue ( GI.HValue dnd_sourcePosition )
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
          dropTgtID <- getLayerData listItem

          GI.HValue dragSrcPos <- GIO.fromGValue @( GI.HValue Position ) val
          let dragSrcID = position dragSrcPos

          mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
          treeListRow <- case mbTreeListRow of
              Nothing -> error "newLayerView ListItem onSetup: no TreeListRow"
              Just r -> return r

          isDescendent <- isDescendentOf ( layerUnique dragSrcID ) listItem
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

            dragTgtPosIx <- EphemerealFlatIndex <$> GTK.treeListRowGetPosition treeListRow
            dropDst <-
              if expanded && not droppedAbove
              then do
                dragTgtStore <- getItemListStore uiElts ( Parent dragTgtPosIx )
                return $
                     MoveToTopOfGroup ( layerUnique dropTgtID, dragTgtStore )
              else do
                ( dstPar, dstParStore ) <- getParentPosition uiElts treeListRow
                return $
                  MoveAboveOrBelowPosition
                    { moveDstPosition =
                        Position
                          { parentPosition = ( dstPar, dstParStore )
                          , position = dropTgtID
                          }
                    , moveAbove = droppedAbove
                    }

            updateLayerHierarchy uiElts variables $
              DoChange $
                Move
                  { moveSrc = dragSrcPos
                  , moveDst = dropDst
                  }
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
                    GTK.singleSelectionNew ( Just lmCopy )
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

-- | Retrieve the position of the parent of the current 'GTK.TreeListRow', if any.
getParentPosition :: UIElements -> GTK.TreeListRow -> IO ( Parent Unique, GIO.ListStore )
getParentPosition uiElts treeListRow = do
  mbPar <- GTK.treeListRowGetParent treeListRow
  par <-
    case mbPar of
      Nothing ->
        return Root
      Just p -> do
        parUniq <- layerUnique <$> getLayerData p
        parPos <- EphemerealFlatIndex <$> GTK.treeListRowGetPosition p
        return ( Parent ( parUniq, parPos ) )
  parStore <- getItemListStore uiElts ( fmap snd par )
  return ( fmap fst par, parStore )

-- | Get the position of the selected item in the 'ListView', if any.
getSelectedPosition :: UIElements -> GTK.ListView -> IO ( Maybe Position )
getSelectedPosition uiElts layersView = do
  mbSelectionModel <- GTK.listViewGetModel layersView
  case mbSelectionModel of
    Nothing -> error "getSelectedPosition: no SelectionModel"
    Just selModel0 -> do
      selModel <- GTK.unsafeCastTo GTK.SingleSelection selModel0
      mbRow <- GTK.singleSelectionGetSelectedItem selModel
      for mbRow $ \ row0 -> do
        row <- GTK.unsafeCastTo GTK.TreeListRow row0
        mbItem <- GTK.treeListRowGetItem row
        case mbItem of
          Nothing -> error "getSelectedPosition: row has no item"
          Just item0 -> do
            layer     <- GTK.unsafeCastTo LayerItem item0
            layerData <- getLayerData layer
            ( parent, parentStore ) <- getParentPosition uiElts row
            return $
              Position
                { parentPosition = ( parent, parentStore )
                , position = layerData
                }

--------------------------------------------------------------------------------
-- Updating the layer hierarchy --
----------------------------------

-- | Do or undo?
data Do = Do | Undo
  deriving stock ( Eq, Show )

-- | Do, undo or redo?
data DoChange = DoChange !Change | RedoChange | UndoChange

-- | The position of an item, both in terms of its 'Unique' (for the application)
-- and in terms of its parent 'GIO.ListStore'.
data Position =
  Position
    { parentPosition :: !( Parent Unique, GIO.ListStore )
    , position       :: !LayerID
    }
  deriving stock Eq

-- | Destination of a move operation.
data MoveDst
  = MoveToTopOfGroup !( Unique, GIO.ListStore )
  | MoveAboveOrBelowPosition
    { moveDstPosition :: !Position
    , moveAbove :: !Bool
    }

-- | Description of a change to the layer hierarchy.
data Change
  = Move
      { moveSrc :: !Position
      , moveDst :: !MoveDst
      }
  | NewLayer
     { newUnique   :: !Unique
     , newIsGroup  :: !Bool
     , newSelected :: !( Maybe Position )
     }
  | Delete
      { deletePosition :: !Position
      }

-- | Data needed to modify the 'GIO.ListModel' underlying the layer hierarchy
-- corresponding to a 'Change'.
data Diff
  = DiffMove
      { diffMoveSrc :: !ChildPosition
      , diffMoveDst :: !ChildPosition
      }
  | DiffNew
      { diffNewDst  :: !ChildPosition
      , diffNewData :: !LayerID
      }
  | DiffDelete
      { diffDelSrc   :: !ChildPosition
      , diffDelData  :: !LayerID
      }
  -- TODO: all 'Diff's store a 'ChildPosition', which currently stores a
  -- GIO.ListStore.
  --
  -- I believe this is risky, as we could:
  --  1. create a new group
  --  2. add some items to the group
  --  3. end up with positions that store the ListModel that was created for this new group
  --  4. undo until we undo the "group creation"
  --  5. do various things until the ListModel in (3) is garbage collected
  --  6. redo until we try to put items back into the ListModel
  --     which no longer exists... causing a crash.

-- | Update the layer hierarchy, keeping both the application state and
-- the GTK ListModel in sync.
updateLayerHierarchy :: UIElements -> Variables -> DoChange -> IO ()
updateLayerHierarchy
  uiElts@( UIElements { .. } )
  ( Variables { historyTVar, listModelUpToDateTMVar } )
  doOrUndo = do
    ( newHistory, mbDiff ) <- STM.atomically $ do

      -- Ensure the GTK ListModel is up to date before continuing
      -- (just a precaution).
      STM.takeTMVar listModelUpToDateTMVar

      history@( History
        { past
        , present = ( presentContent@Content { layerHierarchy = hierarchy }, meta@( Meta { names } ) )
        , future
        } ) <- STM.readTVar historyTVar

      ( history', mbDiff ) <- case doOrUndo of
        DoChange change -> do
          let !( !hierarchy', !newNames, diff ) = applyChangeToLayerHierarchy uiElts change hierarchy
              !content' = presentContent { layerHierarchy = hierarchy' }
              !meta' = meta { names = newNames <> names }
              mkHistory' =
                History
                  { past = past Seq.:|> ( presentContent, diff )
                  , present = ( content', meta' )
                  , future = []
                  }
          return ( mkHistory', Just ( Do, diff ) )
        UndoChange -> case past of
          past' Seq.:|> ( present', diff ) -> do
            let !history' =
                  History
                    { past = past'
                    , present = ( present', meta )
                    , future = ( diff, presentContent ) : future
                    }
            return ( history', Just ( Undo, diff ) )
          Seq.Empty ->
            return ( history, Nothing )
        RedoChange -> case future of
          ( diff, present' ) : future' -> do
            let history' =
                  History
                    { past = past Seq.:|> ( presentContent, diff )
                    , present = ( present', meta )
                    , future = future'
                    }
            return ( history', Just ( Do, diff ) )
          [] ->
            return ( history, Nothing )

      STM.writeTVar historyTVar history'
      return ( history', mbDiff )

    for_ mbDiff applyDiffToListModel

    let newDebugText = uncurry prettyLayers $ present newHistory
        noFuture = null $ future newHistory
        noPast   = null $ past   newHistory

    GTK.widgetSetSensitive redoButton $ not noFuture
    GTK.widgetSetSensitive undoButton $ not noPast
    GTK.labelSetText layersDebugLabel newDebugText

    STM.atomically $
      STM.writeTMVar listModelUpToDateTMVar ()

-- | Apply a change to the application layer hierarchy.
--
-- The change to the GTK ListModel is done in 'applyDiffToListModel'.
applyChangeToLayerHierarchy :: UIElements -> Change -> LayerHierarchy -> ( LayerHierarchy, Map Unique Text, Diff )
applyChangeToLayerHierarchy ( UIElements { layersRootStore } ) change hierarchy =
  case change of
    Move
      { moveSrc = Position ( srcParUniq, srcParStore ) srcPosID
      , moveDst } ->
      let ( dstParUniq, dstParStore, mbDstPosUniq ) =
            case moveDst of
              MoveToTopOfGroup ( dstParUniq_, dstParStore_ ) ->
                ( Parent dstParUniq_, dstParStore_, Nothing )
              MoveAboveOrBelowPosition
                { moveDstPosition = Position ( parUniq, parStore ) tgtID
                , moveAbove } ->
                  ( parUniq, parStore, Just ( layerUnique tgtID, moveAbove ) )
          !( !hierarchy', ( srcChildIx, dstChildIx ) ) =
            moveLayerUpdate
              ( srcParUniq, layerUnique srcPosID )
              ( dstParUniq, mbDstPosUniq )
              hierarchy
      in ( hierarchy'
         , Map.empty
         , DiffMove
             ( ChildPosition srcParStore srcChildIx )
             ( ChildPosition dstParStore dstChildIx )
         )
    NewLayer { newUnique = u, newIsGroup, newSelected } ->
      let ( ( dstParUniq, dstParStore ), dstChildPos ) = case newSelected of
            Nothing  -> ( ( Root, layersRootStore ), Nothing )
            Just ( Position { parentPosition = par, position = dstID } ) ->
              -- TODO: this means we always create a new layer **above** the
              -- selected item.
              ( par, Just ( layerUnique dstID, True ) )
          !( !hierarchy', dstChildIx ) = insertLayer hierarchy ( dstParUniq, dstChildPos ) u
          !hierarchy'' = Map.insert ( Parent u ) ( if newIsGroup then Just [] else Nothing ) hierarchy'
      in
        ( hierarchy''
        , Map.singleton u ( if newIsGroup then "Group" else "Layer" )
        , DiffNew
            { diffNewDst = ChildPosition dstParStore dstChildIx
            , diffNewData = if newIsGroup then GroupID u else LayerID u
            }
        )
    Delete { deletePosition = Position ( parUniq, parStore ) posID } ->
      let u = layerUnique posID
          !( !hierarchy', childIx ) = removeLayer hierarchy ( parUniq, u )
          !hierarchy'' = Map.delete ( Parent u ) hierarchy'
      in
        ( hierarchy''
        , Map.empty
        , DiffDelete
            { diffDelSrc  = ChildPosition parStore childIx
            , diffDelData = posID
            }
        )

-- | Apply a change to the GTK ListModel.
--
-- The change to the application layer hierarchy is done in 'applyChangeToLayerHierarchy'.
applyDiffToListModel :: ( Do, Diff ) -> IO ()
applyDiffToListModel ( doOrUndo, diff ) =
  case diff of
    DiffMove ( ChildPosition srcStore srcIx ) ( ChildPosition dstStore dstIx ) ->
      case doOrUndo of
        Do -> do
          item <- fromJust <$> GIO.listModelGetItem srcStore srcIx
          GIO.listStoreRemove srcStore srcIx
          GIO.listStoreInsert dstStore dstIx item
        Undo -> do
          item <- fromJust <$> GIO.listModelGetItem dstStore dstIx
          GIO.listStoreRemove dstStore dstIx
          GIO.listStoreInsert srcStore srcIx item
    DiffNew { diffNewDst = ChildPosition dstStore dstIx, diffNewData } ->
      case doOrUndo of
        Do -> do
          item <- GI.new LayerItem []
          GI.gobjectSetPrivateData item ( Just diffNewData )
          GIO.listStoreInsert dstStore dstIx item
        Undo ->
          GIO.listStoreRemove dstStore dstIx
    DiffDelete { diffDelSrc = ChildPosition srcStore srcIx, diffDelData } ->
      case doOrUndo of
        Do -> GIO.listStoreRemove srcStore srcIx
        Undo -> do
          item <- GI.new LayerItem []
          GI.gobjectSetPrivateData item ( Just diffDelData )
          GIO.listStoreInsert srcStore srcIx item

-- | Update the 'LayerHierarchy' after a drag-and-drop operation,
-- moving one layer or group around.
--
-- This handles the application side logic.
-- The UI side is handled in 'dragAndDropListModelUpdate'.
--
-- Returns an updated 'LayerHierarchy', together with the relative indices
-- of the source and destination items within their respective parents.
-- This information is then used by GTK to update the underlying 'ListModel'.
moveLayerUpdate
  :: ( Parent Unique, Unique )
    -- ^ source
  -> ( Parent Unique, Maybe ( Unique, Bool ) )
    -- ^ destination
    --
    --  - @Nothing@: drop as first element of group
    --  - @Just (u, above)@ drop above/below u
  -> LayerHierarchy
    -- ^ hierarchy to update
  -> ( LayerHierarchy, ( Word32, Word32 ) )
moveLayerUpdate src@( _, srcUniq ) dst hierarchy =
  let
    -- Remove the child from its old parent.
    ( hierarchy' , oldChildPos ) = removeLayer hierarchy src
    -- Add the child to its new parent.
    ( hierarchy'', newChildPos ) = insertLayer hierarchy' dst srcUniq
  in
    ( hierarchy'', ( oldChildPos, newChildPos ) )

-- | Remove a layer or group from its parent in the 'LayerHierarchy',
-- returning the updated 'LayerHierarchy' together with the index the item
-- was found within its parent.
--
-- NB: does not delete the layer itself.
removeLayer :: LayerHierarchy
            -> ( Parent Unique, Unique )
            -> ( LayerHierarchy, Word32 )
removeLayer hierarchy ( parent, u ) =
  let ( fromJust -> oldPar_cs ) = hierarchy Map.! parent
      newChildren = Just $ ( filter ( /= u ) ) oldPar_cs
      oldChildPos = fromIntegral $ fromJust $ elemIndex u oldPar_cs
  in ( Map.insert parent newChildren hierarchy, oldChildPos )

-- | Add a layer to a parent in the 'LayerHierarchy', returning the updated
-- 'LayerHierarchy' together with the index the item was placed within its parent.
--
-- NB: does not add the layer itself.
insertLayer :: LayerHierarchy
            -> ( Parent Unique, Maybe ( Unique, Bool ) )
                -- ^ destination
                --
                --  - @Nothing@: drop as first element of group
                --  - @Just (u, above)@ drop above/below u
            -> Unique
            -> ( LayerHierarchy, Word32 )
insertLayer hierarchy ( par, mbTgtUniq ) srcUniq =
  let
    ( fromJust -> newPar_cs ) = hierarchy Map.! par
    ( parent', newChildPos ) =
      case mbTgtUniq of
        -- Drop as first child of group.
        Nothing ->
          ( Just ( srcUniq : filter ( /= srcUniq ) newPar_cs )
          , 0
          )
        -- Drop (before or after) given child.
        Just ( tgtUniq, dropAbove ) ->
          let ( bef, aft ) = break ( == tgtUniq ) $ filter ( /= srcUniq ) newPar_cs
          in  ( Just $
                  if dropAbove
                  then bef ++ [ srcUniq ] ++ aft
                  else bef ++ take 1 aft ++ [ srcUniq ] ++ drop 1 aft
              , fromIntegral ( length ( takeWhile ( /= tgtUniq ) $ newPar_cs ) )
              + if dropAbove then 0 else 1
              )
  in ( Map.insert par parent' hierarchy, newChildPos )

-- | Retrieve the 'GIO.ListStore' associated with a parent at the given
-- 'EphemerealFlatIndex'.
getItemListStore :: UIElements -> Parent EphemerealFlatIndex -> IO GIO.ListStore
getItemListStore ( UIElements { layersRootStore, layersListModel } ) parentPos =
  case parentPos of
    Root ->
      return layersRootStore
    Parent parentIx -> do
      parentRow <- fromJust <$> GTK.treeListModelGetRow layersListModel ( ephemerealFlatIndex parentIx )
      parentModel <- fromJust <$> GTK.treeListRowGetChildren parentRow
      parentStore <- GTK.unsafeCastTo GIO.ListStore parentModel
      return parentStore

--------------------------------------------------------------------------------
-- Sample data for illustration.

testContent :: Content
testContent =
  Content
    { layerHierarchy =
        Map.fromList $
          -- NB: this should never be constructed by hand (too error-prone);
          -- this is just for demonstration purposes.
            ( Root, Just [ Unique i | i <- [ 1 .. 5 ] ] )
          : [ ( Parent ( Unique i ), Nothing ) | i <- [ 1 .. 5 ] ]

{-
          [ ( Root                , Just [ Unique 1, Unique 2, Unique 11, Unique 12, Unique 13, Unique 14 ] )
          , ( Parent ( Unique 1  ), Nothing )
          , ( Parent ( Unique 2  ), Just [ Unique 3, Unique 4, Unique 5, Unique 10 ] )
          , ( Parent ( Unique 3  ), Nothing )
          , ( Parent ( Unique 4  ), Nothing )
          , ( Parent ( Unique 5  ), Just [ Unique 6, Unique 7, Unique 8, Unique 9 ] )
          , ( Parent ( Unique 6  ), Nothing )
          , ( Parent ( Unique 7  ), Nothing )
          , ( Parent ( Unique 8  ), Just [ ] )
          , ( Parent ( Unique 9  ), Nothing )
          , ( Parent ( Unique 10 ), Nothing )
          , ( Parent ( Unique 11 ), Nothing )
          , ( Parent ( Unique 12 ), Nothing )
          , ( Parent ( Unique 13 ), Nothing )
          , ( Parent ( Unique 14 ), Just [ Unique 15, Unique 16, Unique 17 ] )
          , ( Parent ( Unique 15 ), Nothing )
          , ( Parent ( Unique 16 ), Nothing )
          , ( Parent ( Unique 17 ), Just [ Unique 18 ] )
          , ( Parent ( Unique 18 ), Nothing )
          ]
-}
    }

testInitialUnique :: Unique
testInitialUnique
  = succ
  $ maximum
  $ catMaybes
  $ fmap ( \ case { Root -> Nothing; Parent i -> Just i } )
  $ Map.keys
  $ layerHierarchy testContent

testMeta :: Meta
testMeta = Meta
        { names = Map.fromList
                    [ ( Unique i, Text.pack $ "layer " ++ show i ) | i <- [ 1 .. 5 ] ]
{-
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
-}
        , invisibles = Set.fromList [ Unique 4, Unique 7 ]
        }
testInitialHistory :: History
testInitialHistory =
  History
    { past = mempty
    , present = ( testContent, testMeta )
    , future = mempty
    }
