module Ys.Banana where

import Control.Concurrent (forkIO, threadDelay)
import Reactive.Banana
import Reactive.Banana.Frameworks

-- 定时器事件源
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

-- 启动定时器，每隔3秒触发一次
startTimer :: EventSource () -> IO ()
startTimer src = do
    let loop = do
            threadDelay (3 * 1000000) -- 3秒
            fire src ()
            loop
    _ <- forkIO loop
    pass

main :: IO ()
main = do
    src <- newAddHandler
    startTimer src
    network <- compile $ do
        etick <- fromAddHandler (addHandler src)
        reactimate $ fmap (\_ -> putStrLn "每隔3秒打印一次") etick
    actuate network
    -- 保持主线程运行
    getLine >> pass
