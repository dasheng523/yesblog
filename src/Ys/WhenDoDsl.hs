{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ys.WhenDoDsl where

import Control.Monad.Free
import Data.Time.Clock.POSIX (getPOSIXTime)

newtype Timestamp = Timestamp Int
    deriving stock (Show)

data DoMethod next
    = GetCurrentTime (Timestamp -> next)
    | LockScreen (() -> next)
    | Ring (() -> next)

instance Functor DoMethod where
    fmap f (GetCurrentTime cb) = GetCurrentTime (f . cb)
    fmap f (LockScreen cb) = LockScreen (f . cb)
    fmap f (Ring cb) = Ring (f . cb)

newtype DoDsl a = DoDsl (Free DoMethod a)
    deriving newtype (Functor, Applicative, Monad)

getCurrentTime :: DoDsl ()
getCurrentTime = DoDsl $ liftF $ GetCurrentTime (const ())

lockScreen :: DoDsl ()
lockScreen = DoDsl $ liftF $ LockScreen (const ())

interpretDoMethod :: DoMethod a -> IO a
interpretDoMethod (GetCurrentTime cb) = do
    t <- getPOSIXTime
    let ts = Timestamp (round t)
    putStrLn ("time: " <> show ts)
    return $ cb ts
interpretDoMethod (LockScreen cb) = do
    putStrLn "Lock Screen ..."
    return $ cb ()
interpretDoMethod (Ring cb) = do
    putStrLn "Ring Ring Ring ..."
    return $ cb ()

runDoDsl :: DoDsl a -> IO a
runDoDsl (DoDsl dsl) = foldFree interpretDoMethod dsl

data LogLevel = Debug | Info | Warning | Error
    deriving stock (Show)

type Message = Text

data LoggerF next where
    LogMessage ::
        LogLevel ->
        Message ->
        (() -> next) ->
        LoggerF next

instance Functor LoggerF where
    fmap f (LogMessage level msg cb) = LogMessage level msg (f . cb)

type LoggerL = Free LoggerF

data LangF next where
    EvalLogger :: LoggerL () -> (() -> next) -> LangF next
    EvalDoDsl :: DoDsl a -> (a -> next) -> LangF next

instance Functor LangF where
    fmap f (EvalLogger loggerL cb) = EvalLogger loggerL (f . cb)
    fmap f (EvalDoDsl doDsl cb) = EvalDoDsl doDsl (f . cb)

type LangL = Free LangF

interpretLoggerMethod :: LoggerF a -> IO a
interpretLoggerMethod (LogMessage lvl msg next) = do
    putStrLn ("[" <> show lvl <> "] " <> toString msg)
    pure (next ())

runLogger :: LoggerL a -> IO a
runLogger = foldFree interpretLoggerMethod

interpretLangFMethod :: LangF a -> IO a
interpretLangFMethod (EvalLogger loggerL cb) = do
    s <- runLogger loggerL
    pure (cb s)
interpretLangFMethod (EvalDoDsl doDsl cb) = do
    s <- runDoDsl doDsl
    pure (cb s)

runLang :: LangL a -> IO a
runLang = foldFree interpretLangFMethod

logDebug :: Message -> LangL ()
logDebug msg = liftF (EvalLogger logDebug' id)
  where
    logDebug' :: LoggerL ()
    logDebug' = liftF (LogMessage Debug msg id)

evalDoDsl :: DoDsl a -> LangL a
evalDoDsl doDsl = liftF (EvalDoDsl doDsl id)