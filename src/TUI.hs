{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module TUI
    ( MyEvent (PSUpdate)
    , mkApp
    , mkInitialState
    , initialDelay
    ) where

import Brick
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Table (table, renderTable, rowBorders, surroundingBorder, columnBorders)
import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Lens ((.=), (^.), over, use)
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (find, Foldable (toList))

import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Graphics.Vty as V
import qualified Data.Set as S

import CircularBuffer
import Commands
import ProcessInformation
import Types
import Data.List (sortBy)

--------------- STATE ---------------

data Name = WidgetEdit
          | Button1
          | Button2
    deriving (Eq, Ord, Show)

data Display = DisplayOverview
             | DisplayGraph
    deriving (Eq)

data State = PsList { _processes :: [ProcessInfo]
                    , _editor :: E.Editor T.Text Name
                    , _orderType :: ProcessOrder
                    , _orderDir :: OrderDirection
                    , _pinned :: S.Set PID
                    , _delay :: MVar Int
                    , _history :: CircularBuffer (Maybe ProcessInfo)
                    , _display :: Display
                    , _selected :: Maybe PID
                    }
makeLenses ''State

---------- ATTRIBUTES ----------

headerAttr :: AttrName
headerAttr = attrName "header"

pinnedAttr :: AttrName
pinnedAttr = attrName "pinned"

myAppAttrMap :: AttrMap
myAppAttrMap = attrMap V.defAttr
    [ (headerAttr, fg V.blue `V.withStyle` V.bold)
    , (pinnedAttr, fg V.yellow)
    ]

---------- APP ----------

newtype MyEvent = PSUpdate [ProcessInfo]
    deriving (Eq)

mkApp :: App State MyEvent Name
mkApp = App
    { appDraw = mkDraw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const myAppAttrMap
    }

mkInitialState :: MVar Int -> State
mkInitialState delayVar = PsList
    { _processes = []
    , _editor = mkEditor
    , _orderType = OrderCPU
    , _orderDir = OrderDec
    , _pinned = S.empty
    , _delay = delayVar
    , _history = newCBuffer $ replicate 30 Nothing
    , _display = DisplayOverview
    , _selected = Nothing
    }

mkEditor :: E.Editor T.Text Name
mkEditor = E.editor WidgetEdit (Just 1) ""

initialDelay :: Int
initialDelay = 1000000

--------------- DRAW ---------------

drawEditor :: E.Editor T.Text Name -> Widget Name
drawEditor = E.renderEditor (txt . T.unlines) True

mkDraw :: State -> [Widget Name]
mkDraw st = case st ^. display of
    DisplayOverview -> drawDisplayOverview (st ^. editor) (st ^. pinned) (st ^. processes)
    DisplayGraph -> drawDisplayGraph (st ^. editor) (toList $ st ^. history) (st ^. orderType)

drawDisplayOverview :: E.Editor T.Text Name -> S.Set PID -> [ProcessInfo] -> [Widget Name]
drawDisplayOverview edit pins ps =
    [ drawEditor edit
    <=> hBorder
    <=> drawProcesses pins ps
    ]

drawProcesses :: S.Set PID -> [ProcessInfo] -> Widget Name
drawProcesses _ [] = str ""
drawProcesses pins ps = renderTable
    $ rowBorders False
    $ columnBorders False
    $ surroundingBorder False
    $ table $ drawProcessHeaders : ((\p -> drawProcess (piPID p `elem` pins) p) <$> ps)

drawProcessHeaders :: [Widget Name]
drawProcessHeaders =
    [ withAttr headerAttr $ str "pid "
    , withAttr headerAttr $ str "cpu (%) "
    , withAttr headerAttr $ str "mem (%) "
    , withAttr headerAttr $ str "time (h:m:s) "
    , withAttr headerAttr $ str "comm "
    ]

drawProcess :: Bool -> ProcessInfo -> [Widget Name]
drawProcess pin (ProcessInfo pid cpu mem time comm) =
    [ applyIf (withAttr pinnedAttr) pin $ str $ show pid <> " "
    , applyIf (withAttr pinnedAttr) pin $ str $ show cpu <> " "
    , applyIf (withAttr pinnedAttr) pin $ str $ show mem <> " "
    , applyIf (withAttr pinnedAttr) pin $ str $ show time <> " "
    , applyIf (withAttr pinnedAttr) pin $ str $ show comm <> " "
    ]
    where
        applyIf :: (a -> a) -> Bool -> a -> a
        applyIf f True a = f a
        applyIf _ False a = a

drawDisplayGraph :: E.Editor T.Text Name -> [Maybe ProcessInfo] -> ProcessOrder -> [Widget Name]
drawDisplayGraph edit hist ord =
    let pts = (maybe 0 (fromOrder ord) <$> hist)
        graphWidget = drawUnscaledGraph pts
    in [ drawEditor edit
        <=> hBorder
        <=> hBox
            [ vBox
                [ str (show $ maximum pts)
                , hLimit 1 (fill ' ')
                , str "0.0"
                ]
            , vBorder
            , vBox [graphWidget]
            ]
        ]
    where
        fromOrder :: ProcessOrder -> ProcessInfo -> Percent
        fromOrder OrderMemory = piMemoryPercent
        fromOrder _ = piCPUPercent

drawUnscaledGraph :: RealFrac a => [a] -> Widget Name
drawUnscaledGraph pts = Widget Greedy Greedy $ do
    ctx <- getContext
    let width = availWidth ctx
        height = availHeight ctx
        scaled = scaleValues height $ scaleElements width pts
    render $ if maximum pts > 0
        then drawGraph scaled
        else str ""

scaleElements :: (Fractional a) => Int -> [a] -> [a]
scaleElements lim xs = interpolate <$> [0 .. lim - 1]
    where
        scale = fromIntegral (length xs - 1) / fromIntegral (lim - 1)
        interpolate i =
            let idx = fromIntegral i * scale
                lowerIdx = floor idx
                upperIdx = ceiling idx
                ratio = idx - fromIntegral lowerIdx
            in if upperIdx < length xs
                then (xs !! lowerIdx) * (1 - fromRational ratio) + (xs !! upperIdx) * fromRational ratio
                else xs !! lowerIdx

scaleValues :: RealFrac a => Int -> [a] -> [Int]
scaleValues lim xs = scale lim (maximum xs) <$> xs
    where
        scale :: RealFrac a => Int -> a -> a -> Int
        scale lim' maxVal' v = round $ (v / maxVal') * fromIntegral lim'

drawGraph :: [Int] -> Widget Name
drawGraph ps = vBox $ drawRow ps <$> reverse [0 .. maximum ps]
    where
        drawRow :: [Int] -> Int -> Widget Name
        drawRow l req = hBox $ drawCell req <$> l
        drawCell :: Int -> Int -> Widget Name
        drawCell req v = if v >= req then str "#" else str " "

--------------- EVENT ---------------

handleEvent :: BrickEvent Name MyEvent -> EventM Name State ()
handleEvent (AppEvent e) = handleMyEvent e
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    contents <- T.unlines . E.getEditContents <$> use editor
    let cmds = M.parse pCommands "Main" contents
    case cmds of
        (Right cmds') -> do
            mapM_ handleCommand cmds'
            editor .= mkEditor
            pure ()
        (Left _) -> pure ()
handleEvent e@(VtyEvent _) = zoom editor $ E.handleEditorEvent e
handleEvent _ = pure ()

handleCommand :: Command -> EventM Name State ()
handleCommand CommandQuit = halt
handleCommand (CommandDelay time) = do
    _ <- liftIO . takeMVar =<< use delay
    liftIO =<< putMVar <$> use delay <*> pure time
handleCommand CommandReset = do
    _ <- liftIO . takeMVar =<< use delay
    liftIO =<< putMVar <$> use delay <*> pure initialDelay
    initialState <- mkInitialState <$> use delay
    modify $ const initialState
handleCommand (CommandOrderType typ) = do
    resetHistory
    orderType .= typ
    editor .= mkEditor
handleCommand (CommandOrderDirection dir) = do
    orderDir .= dir
    editor .= mkEditor
handleCommand (CommandPin pid) = do
    modify $ over pinned $ \s -> if pid `elem` s
        then S.delete pid s
        else S.insert pid s
    editor .= mkEditor
handleCommand (CommandShow pid) = do
    resetHistory
    sel <- use selected
    if sel == Just pid
        then do
            selected .= Nothing
            display .= DisplayOverview
        else do
            selected .= Just pid
            display .= DisplayGraph
    editor .= mkEditor

resetHistory :: EventM Name State ()
resetHistory = (history .=) . (Nothing <$) =<< use history

handleMyEvent :: MyEvent -> EventM Name State ()
handleMyEvent (PSUpdate ps) = do
    updateProcesses ps
    sel <- use selected
    case sel of
        (Just pid) -> do
            pinfo <- if pid `elem` (piPID <$> ps)
                then pure $ find ((==) pid . piPID) ps
                else pure Nothing
            cbuf <- pushCBuffer pinfo <$> use history
            history .= cbuf
        Nothing -> do
            hist <- (Nothing <$) <$> use history
            history .= hist

updateProcesses :: [ProcessInfo] -> EventM Name State ()
updateProcesses ps = do
    ordered <- orderProcesses <$> use orderType <*> use orderDir <*> pure ps
    cps <- pinnedFirst <$> use pinned <*> pure ordered
    processes .= cps
    where
        isPinned :: S.Set PID -> ProcessInfo -> Bool
        isPinned pins p = piPID p `elem` pins
        orderPinned :: S.Set PID -> ProcessInfo -> ProcessInfo -> Ordering
        orderPinned pins a b = compare (isPinned pins b) (isPinned pins a)
        pinnedFirst :: S.Set PID -> [ProcessInfo] -> [ProcessInfo]
        pinnedFirst pins = sortBy (orderPinned pins)

