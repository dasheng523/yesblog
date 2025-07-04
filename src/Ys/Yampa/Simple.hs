{-# LANGUAGE Arrows #-}

module Ys.Yampa.Simple where

import Control.Concurrent (threadDelay)
import Data.Text (strip)
import Data.Time
import FRP.Yampa
import System.Process (readProcess)

-- | 解析时间字符串为UTCTime
parseTime :: String -> UTCTime
parseTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- | 在特定时间打印字符串
printAtSpecificTime :: UTCTime -> String -> SF () (Event (IO ()))
printAtSpecificTime targetTime msg = proc () -> do
    currentTime <- time -< ()
    let sfStartTime = parseTime "2025-07-03 17:37:17" -- 假设 SF 启动时间
    let timeReached = currentTime >= realToFrac (diffUTCTime targetTime sfStartTime)
    eventAtTime <- edge -< timeReached
    returnA -< tagWith (putStrLn msg >> hFlush stdout) eventAtTime

-- | 执行命令行并根据结果打印字符串
printOnCommandResult :: String -> String -> SF () (Event (IO ()))
printOnCommandResult cmd expectedOutput = proc () -> do
    -- 每秒检查一次命令行
    cmdTrigger <- repeatedly 1.0 () -< ()
    let ioAction = do
            output <- readProcess cmd [] ""
            when
                (strip (toText output) == strip (toText expectedOutput))
                $ putStrLn
                    ("Command '" ++ cmd ++ "' result is '" ++ expectedOutput ++ "'!")
                    >> hFlush stdout
    returnA -< tagWith ioAction cmdTrigger

-- | 组合所有功能并运行
runYampaApp :: IO ()
runYampaApp =
    reactimate
        pass
        (\_ -> threadDelay (1 * 1000000) >> return (1.0, Just ())) -- 每秒更新一次
        ( \_ event' -> do
            case event' of
                Event ioAction -> ioAction >> return True -- 执行 IO 动作，并继续运行
                NoEvent -> return True -- 如果没有事件，继续运行
        )
        ( proc () -> do
            -- Changed _ to ()
            timeEvent <- printAtSpecificTime (parseTime "2025-07-03 17:53:00") "It's 18:00:00 on 2025-07-03!" -< ()
            cmdEvent <- printOnCommandResult "echo true" "true" -< ()
            helloEvent <- repeatedly 3.0 () >>> arr (tagWith (putStrLn "Hello, Yampa!" >> hFlush stdout)) -< ()

            let mergedEvents = timeEvent `lMerge` cmdEvent `lMerge` helloEvent
            returnA -< mergedEvents
        )

runYampaApp2 :: IO ()
runYampaApp2 = do
    reactimate
        pass
        (\_ -> threadDelay (1 * 1000000) >> return (1.0, Just ())) -- 每秒更新一次
        (\_ b -> putStrLn b >> return False)
        (arr (const "Hello"))

-- | 每隔三秒输出一次字符串
per3Sec :: SF (Event ()) String
per3Sec = undefined

-- 看看是不是返回false就会停下运行

-- 分开多个event吧？