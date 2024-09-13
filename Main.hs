{-# LANGUAGE RecursiveDo #-}

module Main where

-- base
import Prelude
  hiding
    ( drop )
import Control.Monad
  ( void )
import Data.Foldable
  ( for_ )
import Data.Int
  ( Int64 )
--import Data.IORef
--  ( IORef, newIORef, readIORef, writeIORef )
import Data.Word
  ( Word64 )
--import Foreign
--  ( FunPtr, malloc, free )
--import Foreign.C
--  ( CString, CSize(..) )
--import Foreign.Ptr
--  ( Ptr, castPtr )
--import Foreign.Storable
--  ( Storable(..) )
import GHC.Stack
  ( HasCallStack )
import System.Environment
  ( setEnv )
import System.Exit
  ( ExitCode(..), exitWith, exitSuccess )
--import System.IO.Unsafe
--  ( unsafePerformIO )

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
--import qualified Data.GI.Base.Utils as GI

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

-- | Create a new 'GTK.TreeListModel' from the given set of layers.
newLayersListModel :: Layers -> IO GTK.TreeListModel
newLayersListModel layers = do
  itemType <- GI.glibType @LayerItem
  store <- GIO.listStoreNew itemType
  for_ layers $ \ layer -> do
    item <- GI.unsafeCastTo LayerItem =<< GObject.objectNewv itemType []
    GI.gobjectSetPrivateData item ( Just layer )
    GIO.listStoreAppend store item

  rootModel <- GIO.toListModel store
  let passthrough = False
      auto_expand = True
  GTK.treeListModelNew rootModel passthrough auto_expand createChildModel

    where
      createChildModel :: GObject.Object -> IO ( Maybe GIO.ListModel )
      createChildModel parent = do
        ( _parentDepth, layerItem ) <- treeListRowLayerItem parent
        mbData <- GI.gobjectGetPrivateData layerItem
        case mbData of
          Nothing -> error "createChildModel: LayerItem has no data"
          Just dat ->
            case dat of
              Cursor {} -> return Nothing
              Layer  {} -> return Nothing
              Group { groupChildren } -> do
                -- NB: create a simple GIO.ListStore, not a nested GTK.TreeListModel,
                -- as that would cause e.g. grand-children model to be created twice.
                itemType <- GI.glibType @LayerItem
                store <- GIO.listStoreNew itemType
                for_ groupChildren $ \ childLayer -> do
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
    { layerViewExpander    :: GTK.TreeExpander
    , layerViewContentBox  :: GTK.Box
    , layerViewCheckButton :: GTK.CheckButton
    , layerViewLabel       :: GTK.Label
    }

setupNewLayerViewWidget :: GTK.ListItem -> IO ()
setupNewLayerViewWidget listItem = do

  expander <- GTK.treeExpanderNew
  GTK.listItemSetFocusable listItem False
  GTK.treeExpanderSetIndentForIcon  expander True
  GTK.treeExpanderSetIndentForDepth expander True
  GTK.treeExpanderSetHideExpander   expander False

  GTK.listItemSetChild listItem ( Just expander )

  contentBox <- GTK.boxNew GTK.OrientationHorizontal 20
  GTK.treeExpanderSetChild expander ( Just contentBox )

  checkBox <- GTK.checkButtonNew
  GTK.boxAppend contentBox checkBox
  itemLabel <- GTK.labelNew Nothing
  GTK.boxAppend contentBox itemLabel

-- | Given a 'GTK.ListItem' widget that displays a row, get the
-- widget hierarchy so that we can modify them to display the appropriate
-- content.
getLayerViewWidget :: GTK.ListItem -> IO LayerViewWidget
getLayerViewWidget listItem = do
  mbExpander <- GTK.listItemGetChild listItem
  case mbExpander of
    Nothing -> error "getLayerViewWidget: expected ListItem->Expander"
    Just expander0 -> do
      expander <- GTK.unsafeCastTo GTK.TreeExpander expander0
      mbContentBox <- GTK.treeExpanderGetChild expander
      case mbContentBox of
        Nothing -> error "getLayerViewWidget: expected ListItem->Expander->Box"
        Just contentBox0 -> do
          contentBox <- GTK.unsafeCastTo GTK.Box contentBox0
          mbCheckButton <- traverse ( GTK.unsafeCastTo GTK.CheckButton ) =<< GTK.widgetGetFirstChild contentBox
          case mbCheckButton of
            Nothing -> error "getLayerViewWidget: expected ListItem->Expander->Box->CheckButton"
            Just checkButton -> do
              mbLayerLabel <- traverse ( GTK.unsafeCastTo GTK.Label ) =<< GTK.widgetGetNextSibling checkButton
              case mbLayerLabel of
                Nothing -> error "getLayerViewWidget: expected ListItem->Expander->Box->{CheckButton,LayerLabel}"
                Just layerLabel ->
                  return $
                    LayerViewWidget
                      { layerViewExpander = expander
                      , layerViewContentBox = contentBox
                      , layerViewCheckButton = checkButton
                      , layerViewLabel = layerLabel
                      }

-- | Create a new 'GTK.ListView' that displays 'LayerItem's.

newLayerView :: GTK.ApplicationWindow
             -> STM.TVar Unique
             -> STM.TVar History
             -> GTK.Label
             -> GTK.TreeListModel
             -> IO GTK.ListView
newLayerView window _uniqueTVar historyTVar layersContentDebugLabel layersListModel = mdo
  layersListFactory <- GTK.signalListItemFactoryNew

  -- Connect to "setup" signal to create generic widgets for viewing the tree.
  _ <- GTK.onSignalListItemFactorySetup layersListFactory $ \ listItem0 -> do

        listItem <- GTK.unsafeCastTo GTK.ListItem listItem0
        setupNewLayerViewWidget listItem

        LayerViewWidget
          { layerViewExpander = expander
          , layerViewLabel = _label }
            <- getLayerViewWidget listItem

{-
        -- Connect a signal for editing the layer name.
        void $ GTK.onEditableChanged label $ do
          newLabel <- GTK.editableGetText ?self
          ( _, layerItem ) <- treeListItemLayerItem listItem
          dat <- getLayerData layerItem
          let dat' = case dat of { l@( Layer {} ) -> l { layerName = newLabel }
                                 ; g@( Group {} ) -> g { groupName = newLabel }
                                 ; c@( Cursor {} ) -> c }
          History { present = ( newLayers, newMetaData ) } <- STM.atomically $ do
            hist@History { present = ( layers, meta@( Meta { names } ) ) } <- STM.readTVar historyTVar
            let names' = case dat of { Layer { layerUnique = u } -> Map.insert u newLabel names
                                     ; Group { groupUnique = u } -> Map.insert u newLabel names
                                     ; Cursor -> names }
                meta' = meta { names = names' }
                hist' = hist { present = ( layers, meta' ) }
            STM.writeTVar historyTVar hist'
            return hist
          GI.gobjectSetPrivateData layerItem ( Just dat' )
          let newDebugText = Text.intercalate "\n" $ prettyLayers $ layerDataForUI newMetaData ( contentLayers newLayers )
          GTK.labelSetText layersContentDebugLabel newDebugText
-}

        -- Connect signals for starting a drag from this widget.
        dragSource <- GTK.dragSourceNew

        void $ GTK.onDragSourcePrepare dragSource $ \ _x _y -> do
          ( _, layerItem ) <- treeListItemLayerItem listItem
          dat <- getLayerData layerItem
          let dragSourceData = layerDragSource dat
          gval <- GIO.toGValue @DragSourceData dragSourceData
          val <- GDK.contentProviderNewForValue gval
          --GTK.widgetAddCssClass window "dragging-item"
          return $ Just val
        void $ GTK.onDragSourceDragBegin dragSource $ \ _drag -> do
          paintable <- GTK.widgetPaintableNew ( Just expander )
          GTK.dragSourceSetIcon ?self ( Just paintable ) 0 0
          GTK.widgetAddCssClass expander "dragged"
        void $ GTK.onDragSourceDragCancel dragSource $ \ _drag _reason ->
          return False
        void $ GTK.onDragSourceDragEnd dragSource $ \ drag _deleteData -> do
          GTK.widgetRemoveCssClass window "dragging-item"
          GTK.widgetRemoveCssClass expander "dragged"


          -- The docs say the following isn't necessary, but I'm getting a
          -- lingering drag item widget if I don't destroy the GDK surface manually.
          mbSurf <- GDK.dragGetDragSurface drag
          for_ mbSurf $ GDK.surfaceDestroy


        -- Connect signals for receiving a drop on this widget.
        dragSourceDataGType <- return GI.gtypeInt64 -- readIORef dragSourceGTypeRef
        dropTarget <- GTK.dropTargetNew dragSourceDataGType [ GDK.DragActionCopy ]

        let dropTargetCleanup :: IO ()
            dropTargetCleanup = do
              GTK.widgetRemoveCssClass expander "drag-over"
              GTK.widgetRemoveCssClass expander "drag-top"
              GTK.widgetRemoveCssClass expander "drag-bot"
              mbNextItem <- getNextItem_maybe expander
              for_ mbNextItem $ \ nextItem -> do
                GTK.widgetRemoveCssClass nextItem "drag-top"
        void $ GTK.onDropTargetAccept dropTarget $ \ _drop -> do
          ( _, layerItem ) <- treeListItemLayerItem listItem
          dat <- getLayerData layerItem
          -- Don't allow drops on the cursor element.
          case dat of
            Cursor -> return False
            _      -> return True
        void $ GTK.onDropTargetDrop dropTarget $ \ val _x y -> do
          ( _, layerItem ) <- treeListItemLayerItem listItem
          dat <- getLayerData layerItem
          let dropTargetData = layerDragSource dat
          dragSourceData <- GIO.fromGValue @DragSourceData val
          if dragSourceData == dropTargetData
          then return False -- Don't allow a drag onto ourselves.
          else do
            h <- GTK.widgetGetHeight expander
            let droppedAbove = y < 0.5 * fromIntegral h

            mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
            treeListRow <- case mbTreeListRow of
                Nothing -> error "newLayerView ListItem onSetup: no TreeListRow"
                Just r -> return r
            expanded <- GTK.treeListRowGetExpanded treeListRow

            putStrLn $ unlines
              [ "DND"
              , "source: " ++ show dragSourceData
              , "target: " ++ show dropTargetData
              , "droppedAbove: " ++ show droppedAbove
              , "expanded:" ++ show expanded
              ]

            History { present = ( newLayers, newMetaData ) } <- STM.atomically $ do
              hist@History { past, present = ( content@Content { contentLayers = layers }, meta ) } <- STM.readTVar historyTVar
              let layers' = dragAndDropLayerUpdate dragSourceData dropTargetData droppedAbove expanded layers
                  content' = content { contentLayers = layers' }
                  hist' = History { past = past Seq.:|> content, present = ( content', meta ), future = [] }
              STM.writeTVar historyTVar hist'
              return hist
            dragAndDropListModelUpdate dragSourceData dropTargetData droppedAbove expanded listView
            let newDebugText = Text.intercalate "\n" $ prettyLayers $ layerDataForUI newMetaData ( contentLayers newLayers )
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
        LayerViewWidget
          { layerViewExpander    = expander
          , layerViewCheckButton = checkButton
          , layerViewLabel       = layerLabel
          } <- getLayerViewWidget listItem

        ( _, layerItem ) <- treeListItemLayerItem listItem
        dat <- getLayerData layerItem

        let ( layerText, checkBoxStatusVisible ) =
              case dat of
                Cursor -> ( "+", True )
                Layer { layerName, layerVisible } ->
                  ( layerName, layerVisible )
                Group { groupName, groupVisible } ->
                  ( groupName, groupVisible )

        mbTreeListRow <- traverse ( GTK.unsafeCastTo GTK.TreeListRow ) =<< GTK.listItemGetItem listItem
        treeListRow <- case mbTreeListRow of
          Nothing -> error "newLayerView ListItem onBind: no TreeListRow"
          Just r -> return r
        GTK.treeExpanderSetListRow expander ( Just treeListRow )

        case dat of
          Cursor -> do
            GTK.widgetAddCssClass expander "cursor"
            GTK.widgetSetVisible checkButton False
          _ -> do
            GTK.widgetRemoveCssClass expander "cursor"
            GTK.widgetSetVisible checkButton True
            GTK.checkButtonSetActive checkButton checkBoxStatusVisible
        GTK.labelSetText layerLabel layerText

  selectionModel <- GTK.noSelectionNew ( Just layersListModel )
  listView <- GTK.listViewNew ( Just selectionModel ) ( Just layersListFactory )
  return listView

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

--------------------------------------------------------------------------------

main :: IO ()
main = do
  --setEnv "GTK_DEBUG" "actions,tree"
  setEnv "GDK_DEBUG" "dnd"
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

  GTK.widgetAddCssClass window "list-store"

  --newDragSourceDataGType

  contentBox <- GTK.boxNew GTK.OrientationHorizontal 100
  layersBox  <- GTK.boxNew GTK.OrientationVertical 0
  GTK.boxAppend contentBox layersBox

  internalLayersLabel <- GTK.labelNew ( Just $ Text.intercalate "\n" $ prettyLayers testLayers )
  GTK.boxAppend contentBox internalLayersLabel


  uniqueTVar  <- STM.newTVarIO @Unique $ succ $ maximum ( foldMap layerSpecUniques testLayerSpecs )
  historyTVar <- STM.newTVarIO @History testInitialHistory

  layers     <- newLayersListModel testLayers
  layersView <- newLayerView window uniqueTVar historyTVar internalLayersLabel layers
  GTK.listViewSetShowSeparators layersView False


  GTK.boxAppend layersBox layersView

  GTK.windowSetChild window ( Just contentBox )
  GTK.widgetSetVisible layersView True
  GTK.widgetSetVisible window True


  -- TODO: connect up GTK signals to modify 'historyTVar'.
  -- Every time it is updated, also update 'internalLayersLabel'.

  -- TODO: implement an undo/redo feature and make it update the UI appropriately.

  -- TODO: implement a "new layer" button.

  void $ GTK.onApplicationQueryEnd application $
    GTK.applicationRemoveWindow application window

--------------------------------------------------------------------------------

--------------
-- App data --
--------------

newtype Unique = Unique { unique :: Word64 }
  deriving newtype ( Eq, Ord, Enum )
instance Show Unique where { show ( Unique i ) = "%" ++ show i }

-- | Application-side representation of a layer.
data LayerSpec
  = LayerSpec Unique
  | GroupSpec Unique [ LayerSpec ]

-- | Application-side representation of layers.
data Content =
  Content { contentLayers :: [ LayerSpec ] }

data Meta =
  Meta { names :: Map Unique Text
       , invisibles :: Set Unique
       , cursorPosition :: Position
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

type DragSourceData = Int64

{-
data DragSourceData = CursorDrag | LayerDrag !Unique
  deriving stock ( Eq, Ord, Show )

instance Storable DragSourceData where
  sizeOf _ = sizeOf @Int64 undefined
  alignment _ = alignment @Int64 undefined
  peek ptr = do
    v <- peek @Int64 ( castPtr ptr )
    if v == -1
    then return CursorDrag
    else return $ LayerDrag $ Unique $ fromIntegral v
  poke ptr dat = do
    let val :: Int64
        val = case dat of
          CursorDrag -> -1
          LayerDrag ( Unique u ) -> fromIntegral u
    poke @Int64 ( castPtr ptr ) val


dragSourceGTypeRef :: IORef GI.GType
dragSourceGTypeRef = unsafePerformIO $ newIORef undefined
{-# NOINLINE dragSourceGTypeRef #-}

type BoxedCopyFunc a = Ptr a -> IO ( Ptr a )
type BoxedFreeFunc a = Ptr a -> IO ()

foreign import ccall "g_boxed_type_register_static"
  g_boxed_type_register_static ::
    CString ->
    FunPtr ( BoxedCopyFunc a ) ->
    FunPtr ( BoxedFreeFunc a ) ->
    IO GI.CGType
foreign import ccall "wrapper" mk_BoxedCopyFunc :: BoxedCopyFunc a -> IO ( FunPtr ( BoxedCopyFunc a ) )
foreign import ccall "wrapper" mk_BoxedFreeFunc :: BoxedFreeFunc a -> IO ( FunPtr ( BoxedFreeFunc a ) )

foreign import ccall unsafe "memcpy"
   memcpy :: Ptr a -> Ptr a -> CSize -> IO ()

boxedTypeRegisterStatic :: Text -- ^ /@name@/: Name of the new boxed type.
                        -> BoxedCopyFunc a -- ^ /@boxedCopy@/: Boxed structure copy function.
                        -> BoxedFreeFunc a -- ^ /@boxedFree@/: Boxed structure free function.
                        -> IO GI.GType
boxedTypeRegisterStatic name boxedCopy boxedFree = do
    name' <- GTK.textToCString name
    boxedCopy' <- mk_BoxedCopyFunc boxedCopy
    boxedFree' <- mk_BoxedFreeFunc boxedFree
    result <- g_boxed_type_register_static name' boxedCopy' boxedFree'
    let result' = GTK.GType result
    GI.freeMem name'
    return result'

newDragSourceDataGType :: IO ()
newDragSourceDataGType = do
  dragSourceDataGType <- boxedTypeRegisterStatic "DragSourceData" copyFn freeFn
  writeIORef dragSourceGTypeRef dragSourceDataGType
  where
    copyFn :: BoxedCopyFunc DragSourceData
    copyFn ptr = do
      ptr' <- malloc
      memcpy ptr' ptr ( fromIntegral $ sizeOf ( undefined :: DragSourceData ) )
      return ptr'
    freeFn :: BoxedFreeFunc DragSourceData
    freeFn = free


instance GTK.IsGValue DragSourceData where
  gvalueGType_ = readIORef dragSourceGTypeRef
  gvalueSet_ gvalue = poke ( castPtr gvalue )
  gvalueGet_ gvalue = peek ( castPtr gvalue )

-}

type Layers = [ Layer ]

data Layer
  = Group { groupUnique :: !Unique, groupName :: !Text, groupVisible :: Bool, groupChildren :: !Layers }
  | Layer { layerUnique :: !Unique, layerName :: !Text, layerVisible :: Bool }
  | Cursor

layerDragSource :: Layer -> DragSourceData
layerDragSource ( Group { groupUnique } ) = fromIntegral $ unique groupUnique -- LayerDrag groupUnique
layerDragSource ( Layer { layerUnique } ) = fromIntegral $ unique layerUnique -- LayerDrag layerUnique
layerDragSource Cursor                    = -1 -- CursorDrag

prettyLayers :: Layers -> [ Text ]
prettyLayers = concatMap prettyLayer

prettyLayer :: Layer -> [ Text ]
prettyLayer Cursor = [ "- (cursor)" ]
prettyLayer ( Layer { layerName, layerVisible }) = [ "- Layer { layerName = \"" <> layerName <> "\", layerVisible = " <> Text.pack ( show layerVisible ) <> " } " ]
prettyLayer ( Group { groupName, groupVisible, groupChildren }) =
   [ "- Group { groupName = \"" <> groupName <> "\", groupVisible = " <> Text.pack ( show groupVisible ) <> " }" ]
     ++ map ( "  " <> ) ( prettyLayers groupChildren )

newtype LayerItem = LayerItem ( GTK.ManagedPtr LayerItem )
instance GI.TypedObject LayerItem  where
  glibType = GI.registerGType LayerItem

instance GI.GObject LayerItem

instance GI.HasParentTypes LayerItem
type instance GI.ParentTypes LayerItem = '[ GObject.Object ]

instance GI.DerivedGObject LayerItem where
  type GObjectParentType  LayerItem = GObject.Object
  type GObjectPrivateData LayerItem = Maybe ( Layer )
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

layerDataForUI :: Meta -> [ LayerSpec ] -> [ Layer ]
layerDataForUI ( Meta { names, invisibles, cursorPosition } ) specs =
  case cursorPosition of
    Top -> Cursor : go 0 Nothing specs
    _   -> go 0 ( Just cursorPosition ) specs
    -- TODO: we might have created a group, moved the cursor into it,
    -- then done undo. In this case we need to reset the cursor to the closest
    -- position.
  where
    go :: Int -> Maybe Position -> [ LayerSpec ] -> [ Layer ]
    go depth mbCursor = \case
      [] -> []
      l : ls ->
        case l of
          LayerSpec u ->
            let l' = Layer u ( names Map.! u ) ( not $ u `Set.member` invisibles )
                ls' = case mbCursor of
                  Just ( Below u' ) | u == u' -> Cursor : go depth Nothing ls
                  _                           -> go depth mbCursor ls
            in l' : ls'
          GroupSpec u cs ->
            let (topG, belowG) = case mbCursor of
                  Just ( TopOfGroup u' ) | u == u' -> ( True, False )
                  Just ( Below u' ) | u == u' -> ( False, True )
                  _ -> ( False, False )

                l' = Group u ( names Map.! u ) ( not $ u `Set.member` invisibles )
                       ( if topG
                         then Cursor : go ( depth + 1 ) Nothing cs
                         else go ( depth + 1 ) ( if belowG then Nothing else mbCursor ) cs
                       )
                ls' =
                  if belowG
                  then Cursor : go depth Nothing ls
                  else go depth ( if topG then Nothing else mbCursor ) ls
            in l' : ls'

layerSpecUniques :: LayerSpec -> Set Unique
layerSpecUniques = \case
  LayerSpec u -> Set.singleton u
  GroupSpec u us -> Set.insert u $ foldMap layerSpecUniques us

--------------------------------------------------------------------------------
-- Drag and drop update.

-- TODO

dragAndDropLayerUpdate :: DragSourceData -> DragSourceData -> Bool -> Bool -> [ LayerSpec ] -> [ LayerSpec ]
dragAndDropLayerUpdate _dragSrc _dropTgt _dropAbove _expanded layers = layers

dragAndDropListModelUpdate :: DragSourceData -> DragSourceData -> Bool -> Bool -> GTK.ListView -> IO ()
dragAndDropListModelUpdate dragSrc dropTgt dropAbove expanded listView = do
  mbLayersListModel <- GTK.listViewGetModel listView
  case mbLayersListModel of
    Nothing -> error "dragAndDropListModelUpdate: no underlying model"
    Just layersListModel -> do
      nbItems <- GIO.listModelGetNItems layersListModel
      putStrLn $ unlines
        [ "dragAndDropListModelUpdate"
        , "dragSrc: " ++ show dragSrc
        , "dropTgt: " ++ show dropTgt
        , "dropAbove: " ++ show dropAbove
        , "expanded: " ++ show expanded
        , "nbItems: " ++ show nbItems
        ]

  return ()

--------------------------------------------------------------------------------
-- Sample data for illustration.

testLayers :: [ Layer ]
testLayers = layerDataForUI testMeta testLayerSpecs

testLayerSpecs :: [ LayerSpec ]
testLayerSpecs =
  [ LayerSpec (Unique 1)
  , GroupSpec (Unique 2)
      [ LayerSpec (Unique 3)
      , LayerSpec (Unique 4)
      , GroupSpec (Unique 5)
         [ LayerSpec (Unique 6)
         , LayerSpec (Unique 7)
         , GroupSpec (Unique 8)
            [ ]
         , LayerSpec (Unique 9)
         ]
      , LayerSpec (Unique 10)
      ]
  , LayerSpec (Unique 11)
  ]
testMeta :: Meta
testMeta = Meta
        { names = Map.fromList
                    [ ( Unique 1, "layer 1" )
                    , ( Unique 2, "group 1" )
                    , ( Unique 3, "layer 1.1" )
                    , ( Unique 4, "layer 1.2" )
                    , ( Unique 5, "group 1.3" )
                    , ( Unique 6, "layer 1.3.1" )
                    , ( Unique 7, "layer 1.3.2" )
                    , ( Unique 8, "group 1.3.3" )
                    , ( Unique 9, "layer 1.3.4" )
                    , ( Unique 10, "layer 1.4" )
                    , ( Unique 11, "layer 2" )
                    ]
        , invisibles = Set.fromList [ Unique 4, Unique 7 ]
        , cursorPosition = TopOfGroup ( Unique 5 )
        }
testInitialHistory :: History
testInitialHistory =
  History
    { past = mempty
    , present = ( Content { contentLayers = testLayerSpecs }, testMeta )
    , future = mempty
    }

--------------------------------------------------------------------------------
