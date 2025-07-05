{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ys.Yampa.Simple where

import Control.Concurrent (threadDelay)

import Control.Monad.Free (Free, foldFree)
import Data.Time (defaultTimeLocale, getCurrentTime, parseTimeM)
import Data.Time.Clock (UTCTime, diffUTCTime)
import FRP.Yampa (
    Arrow (arr),
    Event (..),
    SF,
    accumHoldBy,
    after,
    catEvents,
    dSwitch,
    lMerge,
    maybeToEvent,
    mergeEvents,
    never,
    parB,
    reactimate,
    repeatedly,
    returnA,
    rpSwitchB,
    switch,
    tagWith,
 )
import System.IO (hReady)

-- | 每隔三秒输出一次字符串事件
per3SecEvent :: SF a (Event Text)
per3SecEvent = repeatedly 3.0 () >>> arr (tagWith "Hello from Yampa! (3s)")

-- | 每隔五秒输出一次字符串事件
per5SecEvent :: SF a (Event Text)
per5SecEvent = repeatedly 6.0 () >>> arr (tagWith "Hello from Yampa! (6s)")

-- | 每隔十秒输出一次字符串事件
per10SecEvent :: SF a (Event Text)
per10SecEvent = repeatedly 7.0 () >>> arr (tagWith "Hello from Yampa! (7s)")

-- | 在特定绝对时间点触发事件
atSpecificTimeEvent :: UTCTime -> Text -> SF a (Event Text)
atSpecificTimeEvent appStartTime targetTimeString =
    let
        -- 解析目标时间字符串
        mTargetTime :: Maybe UTCTime
        mTargetTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" (toString targetTimeString)
     in
        case mTargetTime of
            Just targetTime ->
                let
                    -- 计算从应用启动到目标时间的相对时间差
                    timeDiff = realToFrac (diffUTCTime targetTime appStartTime)
                 in
                    trace ("left time: " ++ (show timeDiff)) $
                        if timeDiff > 0
                            then after timeDiff (toText $ "Scheduled message at " <> targetTimeString)
                            else never -- 如果目标时间已过或无效，则永不触发
            Nothing ->
                trace "parseTimeM failed" never -- 解析失败，永不触发

-- 根据用户输入，输出消息的信号函数
inputMessageSF :: SF (Maybe Text) (Event Text)
inputMessageSF = arr (maybeToEvent . (fmap ("User input: " <>)))

-- 新实现：每次输入都能独立触发一个定时事件，所有定时事件并行叠加
-- 2025-07-05 20:37:35 +0800
userScheduledEventSF :: UTCTime -> SF (Maybe Text) (Event Text)
userScheduledEventSF appStartTime =
    let
        -- 这个 SF 负责产生 Event (timers -> timers ++ [新定时器])
        updateTimersSF :: SF (Maybe Text, Event ([SF (Maybe Text) (Event Text)] -> [SF (Maybe Text) (Event Text)])) [Event Text]
        updateTimersSF = rpSwitchB []
     in
        -- 构造 (input, Event ...) 作为 rpSwitchB 的输入
        arr
            ( \input ->
                ( input
                , case input of
                    Just t -> Event (\timers -> timers ++ [atSpecificTimeEvent appStartTime t])
                    Nothing -> NoEvent
                )
            )
            >>> updateTimersSF
            >>> arr (foldr lMerge NoEvent)

-- | 组合所有周期性消息，输出 Event String
periodicMessages :: UTCTime -> SF (Maybe Text) (Event [Text])
periodicMessages appStartTime = proc inputEvent -> do
    -- inputEvent 现在是命令行输入事件
    e3 <- per3SecEvent -< ()
    e5 <- per5SecEvent -< ()
    e10 <- per10SecEvent -< ()
    inputE <- inputMessageSF -< inputEvent
    scheduledEvent <- userScheduledEventSF appStartTime -< inputEvent

    -- 将命令行输入事件与周期性事件合并
    returnA -< catEvents [e3, e5, e10, inputE, scheduledEvent]

runYampaApp :: IO ()
runYampaApp = do
    lastInteraction <- newMVar =<< getCurrentTime
    appStartTime <- getCurrentTime
    putStrLn $ "App started at: " ++ show appStartTime
    let senseInput _canBlock = do
            currentTime <- getCurrentTime
            dt <- (realToFrac . diffUTCTime currentTime) <$> swapMVar lastInteraction currentTime
            threadDelay 10000 -- 10 milliseconds

            -- 检查标准输入是否有数据可读，非阻塞
            inputEvent <- do
                ready <- hReady stdin
                if ready
                    then Just . toText <$> getLine
                    else return Nothing
            return (dt, Just inputEvent)
        actuateOutput _ b = do
            case b of
                Event ss -> mapM_ (putStrLn . toString) ss -- 遍历列表并打印每个字符串
                NoEvent -> void pass
            return False
    reactimate
        (return Nothing)
        senseInput
        actuateOutput
        (periodicMessages appStartTime)

------------
------------
------------
------------ DoDSL
newtype Timestamp = Timestamp UTCTime
    deriving stock (Show)

data DoMethod next
    = LockScreen (() -> next)
    | Ring (() -> next)
    | Print Text (() -> next)

instance Functor DoMethod where
    fmap f (LockScreen cb) = LockScreen (f . cb)
    fmap f (Ring cb) = Ring (f . cb)
    fmap f (Print txt cb) = Print txt (f . cb)

newtype DoL a = DoL (Free DoMethod a)
    deriving newtype (Functor, Applicative, Monad)

interpretDoMethod :: DoMethod a -> IO a
interpretDoMethod (LockScreen cb) = do
    putStrLn "Lock Screen ..."
    return $ cb ()
interpretDoMethod (Ring cb) = do
    putStrLn "Ring Ring Ring ..."
    return $ cb ()
interpretDoMethod (Print txt cb) = do
    putStrLn $ toString txt
    return $ cb ()

runDoDsl :: DoL a -> IO a
runDoDsl (DoL dsl) = foldFree interpretDoMethod dsl

sdf :: SF a b
sdf = switch sf1 cb1
  where
    sf1 :: SF a (b, Event c)
    sf1 = undefined

    cb1 :: (c -> SF a b)
    cb1 c = switch sf11 cb11
      where
        sf11 :: SF a (b, Event c)
        sf11 = undefined
        cb11 :: (c -> SF a b)
        cb11 = undefined