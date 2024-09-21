{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

-- base
import Control.Monad
  ( unless, void, when )
import Data.Foldable
  ( for_, traverse_ )
import Data.List
  ( elemIndex )
import Data.Maybe
  ( catMaybes, fromMaybe, fromJust, isNothing )
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
  -- Some useful environment variables:
  --
  setEnv "GDK_SCALE"    "2"                   -- scale the UI up by a factor
  --setEnv "GSK_RENDERER" "ngl"               -- choose rendering backend
  --setEnv "GTK_DEBUG"    "actions,layout"    -- GTK debug info
  --setEnv "GDK_DEBUG"    "events,opengl,dnd" -- GDK debug info
  --setEnv "GSK_DEBUG"    "full-redraw"       -- force redraws
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
  { layersContainer        :: !GTK.ScrolledWindow
      -- ^ Container for the layer hierarchy display.
  , layersListModel        :: !GTK.TreeListModel
      -- ^ The underlying 'GTK.TreeListModel' of the layer hierarchy.
  , layersDebugLabel       :: !GTK.Label
     -- ^ Label that displays the layer hierarchy textually.
  , undoButton, redoButton :: !GTK.Button

  }

-- | Global state of the application.
data Variables
  = Variables
  { uniqueTVar  :: !( STM.TVar Unique )
     -- ^ 'STM.TVar' used as an unique supply.
  , historyTVar :: !( STM.TVar History )
     -- ^ The application state, enabling undo/redo.

    -- | This TVar allows us to look up which 'GIO.ListStore' is used
    -- for the children of a given parent.
    --
    -- This allows us to know, given a parent and a child index,
    -- how to insert/delete from the 'GTK.TreeListModel'.
  , parStoreFromUniqTVar :: !( STM.TVar ( Map ( Parent Unique ) GIO.ListStore ) )

    -- | This TMVar is used to ensure that the layer hierarchy data
    -- is kept in-sync between the application and the UI's 'GTK.TreeListModel'.
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
  parStoreFromUniqTVar <- STM.newTVarIO Map.empty
  listModelUpToDateTMVar <- STM.newTMVarIO ()
  let variables = Variables { uniqueTVar, historyTVar, parStoreFromUniqTVar, listModelUpToDateTMVar }

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

  layersListModel <- newLayersListModel variables

  let uiElts =
        UIElements
          { layersContainer = layersScrolledWindow
          , layersListModel
          , layersDebugLabel
          , undoButton, redoButton
          }

  layersView <- newLayerView uiElts variables
  GTK.listViewSetShowSeparators layersView False

  GTK.scrolledWindowSetChild layersScrolledWindow ( Just layersView )

  void $ GTK.onButtonClicked newLayerButton $ do
    u <- getUnique uniqueTVar
    mbSelectedPos <- getSelectedPosition layersView
    let change = NewLayer { newUnique = u, newIsGroup = False, newSelected = mbSelectedPos }
    updateLayerHierarchy uiElts variables ( DoChange change )
  void $ GTK.onButtonClicked newGroupButton $ do
    u <- getUnique uniqueTVar
    mbSelectedPos <- getSelectedPosition layersView
    let change = NewLayer { newUnique = u, newIsGroup = True, newSelected = mbSelectedPos }
    updateLayerHierarchy uiElts variables ( DoChange change )
  void $ GTK.onButtonClicked deleteLayerButton $ do
    mbSelectedPos <- getSelectedPosition layersView
    case mbSelectedPos of
      Nothing ->
        -- The SingleSelection SelectionModel defaults to autoselect=true,
        -- so there should always be an item selected.
        return ()
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
  , present :: !( Content, Metadata )
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
data Metadata =
  Metadata
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

-----------------
-- GTK UI data --
-----------------

-- | Look up the name and visibility of a layer from the metadata.
layerNameAndVisible :: Metadata -> Unique -> ( Text, Bool )
layerNameAndVisible ( Metadata { names, invisibles } ) unique =
  ( names Map.! unique, unique `notElem` invisibles )

-- | Display the layer hierarchy (for debugging purposes).
prettyLayers :: Content -> Metadata -> Text
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
newLayersListModel :: Variables
                   -> IO GTK.TreeListModel
newLayersListModel ( Variables { .. } ) = do
  itemType <- GI.glibType @LayerItem
  store <- GIO.listStoreNew itemType
  STM.atomically $
    STM.modifyTVar' parStoreFromUniqTVar ( Map.insert Root store )

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

  -- Pass a copy of the (reference to the) root GIO.ListStore to the
  -- 'treeListModelNew' function to ensure we retain ownership of it.
  model <- GI.withManagedPtr rootModel $ \ rmPtr ->
           GI.withNewObject rmPtr $ \ rmCopy ->
             GTK.treeListModelNew rmCopy passthrough auto_expand createChildModel

  return model

    where
      createChildModel :: GObject.Object -> IO ( Maybe GIO.ListModel )
      createChildModel parent = do
        dat <- getLayerData =<< GTK.unsafeCastTo LayerItem parent
        case dat of
          LayerID  {} -> return Nothing
          GroupID { layerUnique = groupUnique } -> do
            ( Content { layerHierarchy }, _ ) <- present <$> STM.readTVarIO historyTVar
            let children = fromMaybe [] $ layerHierarchy Map.! Parent groupUnique

            -- Try to re-use an existing list store, if there is one.
            mbOldChildStore <-
              STM.atomically $ do
                mbOldStore <-
                  Map.lookup ( Parent groupUnique ) <$> STM.readTVar parStoreFromUniqTVar
                when ( isNothing mbOldStore ) $
                  -- Take a lock to avoid creating multiple child stores
                  -- for the same group.
                  STM.takeTMVar listModelUpToDateTMVar
                return mbOldStore

            newChildStore <-
              case mbOldChildStore of
                Just oldStore -> do
                  return oldStore
                Nothing -> do
                  -- Otherwise, create a new child ListModel.
                  -- NB: create a simple GIO.ListStore, not a nested GTK.TreeListModel,
                  -- as that would cause e.g. grand-children models to be created twice.
                  itemType <- GI.glibType @LayerItem
                  childStore <- GIO.listStoreNew itemType

                  for_ children $ \ childUniq -> do
                    let mbChildChildren = layerHierarchy Map.! Parent childUniq
                        childLayer = case mbChildChildren of
                          Nothing -> LayerID { layerUnique = childUniq }
                          Just {} -> GroupID { layerUnique = childUniq }
                    item <- GI.unsafeCastTo LayerItem =<< GI.new LayerItem []
                    GI.gobjectSetPrivateData item ( Just childLayer )
                    GIO.listStoreAppend childStore item

                  -- Store the child store in our mapping from parent unique to
                  -- ListStore, so that we know where to insert children.
                  STM.atomically $ do
                    STM.modifyTVar parStoreFromUniqTVar
                      ( Map.insert ( Parent groupUnique ) childStore )
                    STM.putTMVar listModelUpToDateTMVar ()

                  return childStore

            -- Pass a copy of the (reference to the) child store
            -- to ensure we retain ownership of it.
            childModelCopy <- GI.withManagedPtr newChildStore $ \ childStorePtr ->
                              GI.withNewObject childStorePtr $ \ childStoreCopy ->
                              GIO.toListModel childStoreCopy

            return $ Just childModelCopy

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
newLayerView
  uiElts@( UIElements { layersListModel, layersContainer, layersDebugLabel } )
  variables@( Variables { .. } ) = mdo

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
          uniq <- layerUnique <$> getLayerData listItem
          visible <- GTK.checkButtonGetActive ?self
          ( newContent, newMetadata ) <- STM.atomically $ do
            hist@History { present = ( content, meta@( Metadata { invisibles } ) ) } <- STM.readTVar historyTVar
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
            hist@History { present = ( layers, meta@( Metadata { names } ) ) } <- STM.readTVar historyTVar
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
          srcUniq <- layerUnique <$> getLayerData listItem

          mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
          treeListRow <- case mbTreeListRow of
              Nothing -> error "newLayerView ListItem onSetup: no TreeListRow"
              Just r -> return r
          srcParPos <- getParentPosition treeListRow

          rowPos <- GTK.treeListRowGetPosition treeListRow
          GTK.singleSelectionSetSelected selectionModel rowPos

          let dnd_sourcePosition =
                Position
                  { parentPosition = fmap snd srcParPos
                  , position = srcUniq
                  }

          val <- GDK.contentProviderNewForValue =<< GIO.toGValue ( GI.HValue dnd_sourcePosition )
          GTK.widgetAddCssClass layersContainer "dragging-item"
          return $ Just val
        void $ GTK.onDragSourceDragBegin dragSource $ \ _drag -> do

{- To set a cursor icon for the drag, write the x/y coordinates in the
   'prepare' signal handler to an IORef, and then use the following:
          ( x, y ) <- readIORef dragPosRef
          paintable <- GTK.widgetPaintableNew ( Just expander )
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
          dropTgtUniq <- layerUnique <$> getLayerData listItem

          GI.HValue dragSrcPos <- GIO.fromGValue @( GI.HValue Position ) val
          let dragSrcUniq = position dragSrcPos

          mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
          treeListRow <- case mbTreeListRow of
              Nothing -> error "newLayerView ListItem onSetup: no TreeListRow"
              Just r -> return r

          dstFlatIndex <- GTK.treeListRowGetPosition treeListRow
          h <- GTK.widgetGetHeight expander
          let droppedAbove = y < 0.5 * fromIntegral h
          expanded <- GTK.treeListRowGetExpanded treeListRow

          dstPar <- getParentPosition treeListRow
          isDescendant <- isDescendantOf dragSrcUniq listItem

          let mbDropIntoGroup
                | expanded
                , not droppedAbove
                , not isDescendant
                = Just treeListRow
                | otherwise
                = Nothing
              mbDropOutsideGroup
                | dragSrcUniq == dropTgtUniq
                , Parent par <- dstPar
                , not droppedAbove
                = Just par
                | otherwise
                = Nothing

          if isDescendant && isNothing mbDropOutsideGroup
          then do
            return False
          else do
            -- Compute the destination parent.
            -- Usually, the destination parent is the parent of the drop target.
            -- BUT:
            --  1. when dropping an item into the first position of an
            --     expanded group, the destination parent is the drop target itself,
            --     not the parent of the drop target.
            --  2. when an item is at the bottom of a group, dropping it on its
            --     lower half moves the item out of the group, so the
            --     destination parent is the grand-parent of the drop target.
            ( dropDst, newPosInTree ) <-
              if
                -- (1)
                | Just dstParRow <- mbDropIntoGroup
                -> do
                    dstParFlatIndex <- GTK.treeListRowGetPosition dstParRow
                    return $
                      ( MoveToTopOfGroup dropTgtUniq
                      , dstParFlatIndex + 1
                      )
                -- (2)
                | Just ( dstParRow, dstParUniq ) <- mbDropOutsideGroup
                -> do
                    grandPar <- getParentPosition dstParRow
                    return $
                      ( MoveItemOutsideGroupIfLastItemInGroup
                          { itemUnique        = dropTgtUniq
                          , parentUnique      = dstParUniq
                          , grandParentUnique = fmap snd grandPar
                          , itemExpanded      = expanded
                          }
                      , dstFlatIndex + 1
                      )
                | otherwise
                -> do
                  return $
                    ( MoveAboveOrBelowPosition
                        { moveDstPosition =
                            Position
                              { parentPosition = fmap snd dstPar
                              , position = dropTgtUniq
                              }
                        , moveAbove = droppedAbove
                        }
                    , if droppedAbove then dstFlatIndex else dstFlatIndex + 1
                    )

            -- Compute the position that the item we are moving will have
            -- at the end of the move.
            --
            -- First, we compute whether we moved up or down.
            -- NB: we need to compute the source item position now (using 'treeListRowGetPosition'),
            -- at the end of the drag-and-drop operation, because TreeExpander nodes
            -- might have expanded/collapsed in the meantime.
            mbSelItem <- GTK.singleSelectionGetSelectedItem selectionModel
            mbSelIx <- for mbSelItem $ \ selItem -> do
              selRow <- GTK.unsafeCastTo GTK.TreeListRow selItem
              GTK.treeListRowGetPosition selRow

            -- Now compute the final destination position.
            mbDstPosAfterShift <-
              case mbSelIx of
                Nothing ->
                  return Nothing
                Just selIx
                  -- If we moved up, simply use the destination position.
                  | selIx >= newPosInTree
                  -> return $ Just newPosInTree
                  | otherwise
                  -> do
                  -- If we moved down, we need to substract the number of items
                  -- moved. Note that this depends on which TreeExpander nodes
                  -- are expanded.
                  mbSelRow <- GTK.treeListModelGetRow layersListModel selIx
                  case mbSelRow of
                    Nothing -> return Nothing
                    Just selRow0 -> do
                      selRow <- GTK.unsafeCastTo GTK.TreeListRow selRow0
                      nbDescendants <- getNbExpandedDescendants layersListModel selRow
                      return $
                        if newPosInTree < nbDescendants
                        then Nothing
                        else Just $ newPosInTree - nbDescendants

            updateLayerHierarchy uiElts variables $
              DoChange $
                Move
                  { moveSrc = dragSrcPos
                  , moveDst = dropDst
                  }

            -- After moving, update the selected item to be the moved item.
            case mbDstPosAfterShift of
              Nothing -> return ()
              Just dstPos ->
                GTK.singleSelectionSetSelected selectionModel dstPos
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

  -- Pass copies of (references to) the TreeListModel and SelectionModel,
  -- in order to retain ownership over them.
  selectionModel <- GI.withManagedPtr layersListModel $ \ lmPtr ->
                    GI.withNewObject lmPtr $ \ lmCopy ->
                    GTK.singleSelectionNew ( Just lmCopy )
  listView <- GI.withManagedPtr selectionModel $ \ smPtr ->
              GI.withNewObject smPtr $ \ smCopy ->
              GTK.listViewNew ( Just smCopy ) ( Just layersListFactory )
  return listView

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

-- | Is this list item a descendant of the item with the given unique?
isDescendantOf :: Unique -- ^ are we a descendant of this?
               -> GTK.ListItem -- ^ item we are querying
               -> IO Bool
isDescendantOf u listItem = do
  mbListRow <- GTK.listItemGetItem listItem
  case mbListRow of
    Nothing -> error "isDescendantOf: ListItem has no item"
    Just listRow0 -> do
      listRow <- GTK.unsafeCastTo GTK.TreeListRow listRow0
      go listRow
  where
    go :: GTK.TreeListRow -> IO Bool
    go listRow = do
      u' <- layerUnique <$> getLayerData listRow
      if u' == u
      then return True
      else do
        mbPar <- GTK.treeListRowGetParent listRow
        case mbPar of
          Nothing -> return False
          Just par -> go par

-- | Get the number of expanded descendants of the given 'GTK.TreeListRow',
-- including the row itself.
getNbExpandedDescendants :: GTK.TreeListModel -> GTK.TreeListRow -> IO Word32
getNbExpandedDescendants layersListModel = fmap fst . go
  where
    go :: GTK.TreeListRow -> IO ( Word32, Word32 )
    go row0 = do
      pos0 <- GTK.treeListRowGetPosition row0
      expanded <- GTK.treeListRowGetExpanded row0
      if not expanded
      then do
        return ( 1, pos0 )
      else do
        ( nbChildItems, lastPosChecked ) <- goChildren ( row0, pos0 )
        return ( 1 + nbChildItems, lastPosChecked )
    goChildren :: ( GTK.TreeListRow, Word32 ) -> IO ( Word32, Word32 )
    goChildren ( row, lastPosChecked ) = do
      mbRow' <- GTK.treeListModelGetRow layersListModel ( lastPosChecked + 1 )
      case mbRow' of
        Nothing -> do
          return ( 0, lastPosChecked + 1 )
        Just nextRow0 -> do
          nextRow <- GTK.unsafeCastTo GTK.TreeListRow nextRow0
          mbNextParent <- GTK.treeListRowGetParent nextRow
          case mbNextParent of
            Just nextParent
              | nextParent == row
              -> do
                ( nbChildren, lastChecked ) <- go nextRow
                ( n, lastChecked' ) <- goChildren ( row, lastChecked )
                return ( nbChildren + n, lastChecked' )
            _ -> return ( 0, lastPosChecked )


-- | Retrieve the position of the parent of the current 'GTK.TreeListRow', if any.
getParentPosition :: GTK.TreeListRow -> IO ( Parent ( GTK.TreeListRow, Unique ) )
getParentPosition treeListRow = do
  mbPar <- GTK.treeListRowGetParent treeListRow
  case mbPar of
    Nothing ->
      return Root
    Just p -> do
      parUniq <- layerUnique <$> getLayerData p
      return ( Parent ( p, parUniq ) )

-- | Get the position of the selected item in the 'ListView', if any.
getSelectedPosition :: GTK.ListView -> IO ( Maybe Position )
getSelectedPosition layersView = do
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
            layerData <- layerUnique <$> getLayerData layer
            parent <- getParentPosition row
            return $
              Position
                { parentPosition = fmap snd parent
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
    { parentPosition :: !( Parent Unique )
    , position       :: !Unique
    }
  deriving stock Eq

-- | The position of a child relative to a parent.
data ChildPosition
  = ChildPosition
    { childParent :: !( Parent Unique )
        -- ^ The parent for this item.
    , childIndexInParent :: !Word32
        -- ^ The index of the child within the parent.
    }
  deriving stock Eq

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

-- | Destination of a move operation.
data MoveDst
  -- | Move an item into the top position of a group.
  = MoveToTopOfGroup
    { dstParUnique :: !Unique
      -- ^ The group to drop into.
    }
  -- | Move an item outside and below its current parent,
  -- but only if it is the last item in its parent.
  | MoveItemOutsideGroupIfLastItemInGroup
    { itemUnique        :: !Unique
    , itemExpanded      :: !Bool
    , parentUnique      :: !Unique
    , grandParentUnique :: !( Parent Unique )
    }
  -- | Move an item above or below another item.
  | MoveAboveOrBelowPosition
    { moveDstPosition :: !Position
    , moveAbove :: !Bool
      -- ^ Whether to move above or below the destination.
    }

-- | Information needed to apply (or unapply) a 'Change' to the
-- 'GTK.TreeListModel' underlying the layer hierarchy.
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

-- | Update the layer hierarchy, keeping both the application state and
-- the GTK ListModel in sync.
updateLayerHierarchy :: UIElements -> Variables -> DoChange -> IO ()
updateLayerHierarchy
  ( UIElements { .. } )
  ( Variables { historyTVar, parStoreFromUniqTVar, listModelUpToDateTMVar } )
  doOrUndo = do
    ( newHistory, mbDiff ) <- STM.atomically $ do

      -- Ensure the GTK ListModel is up to date before continuing
      -- (just a precaution).
      STM.takeTMVar listModelUpToDateTMVar

      history@( History
        { past
        , present = ( presentContent@Content { layerHierarchy = hierarchy }, meta@( Metadata { names } ) )
        , future
        } ) <- STM.readTVar historyTVar

      !( !history', mbDiff ) <- case doOrUndo of
        DoChange change -> do
          let !( !hierarchy', !newNames, mbDiff ) = applyChangeToLayerHierarchy change hierarchy
              !content' = presentContent { layerHierarchy = hierarchy' }
              !meta' = meta { names = newNames <> names }
              !( !history', mbDoOrUndo ) =
                case mbDiff of
                  Nothing ->
                    ( History
                        { past = past
                        , present = ( content', meta' )
                        , future = future
                        }
                    , Nothing )
                  Just diff ->
                    ( History
                        { past = past Seq.:|> ( presentContent, diff )
                        , present = ( content', meta' )
                        , future = []
                        }
                    , Just ( Do, diff ) )
          return ( history', mbDoOrUndo )
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

    for_ mbDiff ( applyDiffToListModel parStoreFromUniqTVar )

    STM.atomically $
      STM.writeTMVar listModelUpToDateTMVar ()

    let newDebugText = uncurry prettyLayers $ present newHistory
        noFuture = null $ future newHistory
        noPast   = null $ past   newHistory

    GTK.widgetSetSensitive redoButton $ not noFuture
    GTK.widgetSetSensitive undoButton $ not noPast
    GTK.labelSetText layersDebugLabel newDebugText


-- | Apply a change to the application layer hierarchy.
--
-- The change to the GTK ListModel is done in 'applyDiffToListModel'.
applyChangeToLayerHierarchy :: Change -> LayerHierarchy -> ( LayerHierarchy, Map Unique Text, Maybe Diff )
applyChangeToLayerHierarchy change hierarchy =
  case change of
    Move
      { moveSrc = Position srcParUniq srcPosUniq
      , moveDst } ->
      let mbDst =
            case moveDst of
              MoveAboveOrBelowPosition
                { moveAbove
                , moveDstPosition = Position parUniq tgtUniq
                } ->
                  Just ( parUniq
                       , Just ( tgtUniq , moveAbove )
                       )
              MoveToTopOfGroup
                { dstParUnique } ->
                  Just ( Parent dstParUnique, Nothing )
              MoveItemOutsideGroupIfLastItemInGroup
                { itemUnique, itemExpanded
                , parentUnique, grandParentUnique
                }
                  | Just siblings <- hierarchy Map.! ( Parent parentUnique )
                  , last siblings == itemUnique
                  -- Only allow this for a group when the group is not expanded
                  -- or it has no children.
                  , let expandedGroupWithChildren
                          | itemExpanded
                          , Just cs <- hierarchy Map.! ( Parent itemUnique )
                          , not ( null cs )
                          = True
                          | otherwise
                          = False
                  , not expandedGroupWithChildren
                  -> Just ( grandParentUnique, Just ( parentUnique, False ) )
                  | otherwise
                  -> Nothing
      in case mbDst of
        Nothing ->
          ( hierarchy, Map.empty, Nothing )
        Just ( dstParUniq, mbDstPosUniq ) ->
          let
            !( !hierarchy', mbChildIxs ) =
              moveLayerUpdate
                ( srcParUniq, srcPosUniq )
                ( dstParUniq, mbDstPosUniq )
                hierarchy
          in ( hierarchy'
             , Map.empty
             , case mbChildIxs of
                Just ( srcChildIx, dstChildIx ) ->
                  Just $
                    DiffMove
                      ( ChildPosition srcParUniq srcChildIx )
                      ( ChildPosition dstParUniq dstChildIx )
                Nothing -> Nothing
             )
    NewLayer { newUnique = u, newIsGroup, newSelected } ->
      let ( dstParUniq, dstChildPos ) = case newSelected of
            Nothing  -> ( Root, Nothing )
            Just ( Position { parentPosition = par, position = dstUniq } ) ->
              -- TODO: this means we always create a new layer **above** the
              -- selected item. It would make sense to customise this.
              ( par, Just ( dstUniq, True ) )
          !( !hierarchy', dstChildIx ) = insertLayer hierarchy ( dstParUniq, dstChildPos ) u
          !hierarchy'' = Map.insert ( Parent u ) ( if newIsGroup then Just [] else Nothing ) hierarchy'
      in
        ( hierarchy''
        , Map.singleton u ( if newIsGroup then "Group" else "Layer" )
        , Just $
            DiffNew
              { diffNewDst = ChildPosition dstParUniq dstChildIx
              , diffNewData = if newIsGroup then GroupID u else LayerID u
              }
        )
    Delete { deletePosition = Position parUniq posUniq } ->
      let !( !hierarchy', childIx ) = removeLayer hierarchy ( parUniq, posUniq )
          !( fromJust -> mbHadChildren, !hierarchy'' ) =
              Map.updateLookupWithKey ( \ _ _ -> Nothing )
                ( Parent posUniq )
                hierarchy'
      in
        ( hierarchy''
        , Map.empty
        , Just $
            DiffDelete
              { diffDelSrc  = ChildPosition parUniq childIx
              , diffDelData =
                  case mbHadChildren of
                    Just {} -> GroupID posUniq
                    Nothing -> LayerID posUniq
              }
        )

-- | Apply a change to the 'GTK.TreeListModel' underlying the UI
-- representation of the layer hierarchy.
--
-- The change to the application 'LayerHierarchy' is done beforehand,
-- in 'applyChangeToLayerHierarchy'.
applyDiffToListModel :: STM.TVar ( Map ( Parent Unique ) GIO.ListStore )
                     -> ( Do, Diff )
                     -> IO ()
applyDiffToListModel parStoreFromUniqTVar ( doOrUndo, diff ) = do
  -- All modifications to the GTK.TreeListModel are done by editing
  -- an appropriate child GIO.ListModel.
  --
  -- We **do not** use the flattened indices returned by GTK.treeListRowGetPosition,
  -- as those are ephemeral (they change when tree expanders are expanded or collapsed).
  -- Instead, we keep track of a mapping
  --
  --   parent --> list store used to hold its children
  --
  -- and use that child list store to perform updates.
  parStoreFromUniq <- STM.readTVarIO parStoreFromUniqTVar
  case diff of
    DiffMove ( ChildPosition srcPar srcIx ) ( ChildPosition dstPar dstIx ) -> do
      let srcStore = parStoreFromUniq Map.! srcPar
          dstStore = parStoreFromUniq Map.! dstPar
      case doOrUndo of
        Do -> do
          item <- fromJust <$> GIO.listModelGetItem srcStore srcIx
          GIO.listStoreRemove srcStore srcIx
          GIO.listStoreInsert dstStore dstIx item
        Undo -> do
          item <- fromJust <$> GIO.listModelGetItem dstStore dstIx
          GIO.listStoreRemove dstStore dstIx
          GIO.listStoreInsert srcStore srcIx item
    DiffNew { diffNewDst = ChildPosition dstPar dstIx, diffNewData } -> do
      let dstStore = parStoreFromUniq Map.! dstPar
      case doOrUndo of
        Do -> do
          item <- GI.new LayerItem []
          GI.gobjectSetPrivateData item ( Just diffNewData )
          GIO.listStoreInsert dstStore dstIx item
        Undo ->
          GIO.listStoreRemove dstStore dstIx
    DiffDelete { diffDelSrc = ChildPosition srcPar srcIx, diffDelData } -> do
      let srcStore = parStoreFromUniq Map.! srcPar
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
  -> ( LayerHierarchy, Maybe ( Word32, Word32 ) )
moveLayerUpdate src@( srcPar, srcUniq ) dst@( dstPar, _ ) hierarchy =
  let
    -- Remove the child from its old parent.
    ( hierarchy' , oldChildPos ) = removeLayer hierarchy src
    -- Add the child to its new parent.
    ( hierarchy'', newChildPos ) = insertLayer hierarchy' dst srcUniq
  in
    ( hierarchy''
      -- If the move is a no-op, then return 'Nothing' to avoid
      -- updating the 'GTK.TreeListModel'.
    , if srcPar == dstPar && oldChildPos == newChildPos
      then Nothing
      else Just ( oldChildPos, newChildPos )
    )

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

--------------------------------------------------------------------------------
-- Sample data for illustration.

testContent :: Content
testContent =
  Content
    { layerHierarchy =
        Map.fromList $
          -- NB: this should never be constructed by hand (too error-prone);
          -- this is just for demonstration purposes.
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
    }

testInitialUnique :: Unique
testInitialUnique
  = succ
  $ maximum
  $ catMaybes
  $ fmap ( \ case { Root -> Nothing; Parent i -> Just i } )
  $ Map.keys
  $ layerHierarchy testContent

testMetadata :: Metadata
testMetadata = Metadata
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
    { past    = mempty
    , present = ( testContent, testMetadata )
    , future  = mempty
    }
