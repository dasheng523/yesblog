{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ys.WhenDoDsl where

import Control.Monad.Free
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

newtype Timestamp = Timestamp POSIXTime
    deriving stock (Show)

type Second = Int

------------- WhenL
data WhenMethod next where
    AtTimestamp :: Timestamp -> (() -> next) -> WhenMethod next
    AfterSeconds :: Second -> (() -> next) -> WhenMethod next

instance Functor WhenMethod where
    fmap f (AtTimestamp timestamp cb) = AtTimestamp timestamp (f . cb)
    fmap f (AfterSeconds timestamp cb) = AfterSeconds timestamp (f . cb)

newtype WhenL a = WhenL (Free WhenMethod a)
    deriving newtype (Functor, Applicative, Monad)

------------ DoDSL
data DoMethod next
    = GetCurrentTime (Timestamp -> next)
    | LockScreen (() -> next)
    | Ring (() -> next)

instance Functor DoMethod where
    fmap f (GetCurrentTime cb) = GetCurrentTime (f . cb)
    fmap f (LockScreen cb) = LockScreen (f . cb)
    fmap f (Ring cb) = Ring (f . cb)

newtype DoL a = DoL (Free DoMethod a)
    deriving newtype (Functor, Applicative, Monad)

getCurrentTime' :: DoL ()
getCurrentTime' = DoL $ liftF $ GetCurrentTime (const ())

lockScreen :: DoL ()
lockScreen = DoL $ liftF $ LockScreen (const ())

interpretDoMethod :: DoMethod a -> IO a
interpretDoMethod (GetCurrentTime cb) = do
    t <- getPOSIXTime
    let ts = Timestamp t
    putStrLn ("time: " <> show ts)
    return $ cb ts
interpretDoMethod (LockScreen cb) = do
    putStrLn "Lock Screen ..."
    return $ cb ()
interpretDoMethod (Ring cb) = do
    putStrLn "Ring Ring Ring ..."
    return $ cb ()

runDoDsl :: DoL a -> IO a
runDoDsl (DoL dsl) = foldFree interpretDoMethod dsl

---------------------- WhenToDoL
data WhenToDoF next where
    WhenToDo :: WhenL tl -> DoL md -> (() -> next) -> WhenToDoF next

instance Functor WhenToDoF where
    fmap f (WhenToDo whenL doL cb) = WhenToDo whenL doL (f . cb)

newtype WhenToDoL a = WhenToDoL (Free WhenToDoF a)
    deriving newtype (Functor, Applicative, Monad)

interpretWhenDoF :: WhenToDoF a -> IO a
interpretWhenDoF (WhenToDo _ _ cb) = do
    putStrLn "WhenToDo executed (placeholder)"
    pure (cb ())

--------------------- Log
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
    EvalDoDsl :: DoL a -> (a -> next) -> LangF next

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

evalDoDsl :: DoL a -> LangL a
evalDoDsl doDsl = liftF (EvalDoDsl doDsl id)
