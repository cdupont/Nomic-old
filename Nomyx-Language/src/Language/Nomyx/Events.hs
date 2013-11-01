{-# LANGUAGE ScopedTypeVariables #-}

-- | All the building blocks to allow rules to build events.
module Language.Nomyx.Events where

import Language.Nomyx.Expression
import Data.Typeable
import Control.Monad.State
import Data.List
import Data.Time hiding (getCurrentTime)
import Data.Time.Recurrence hiding (filter)
import Safe


-- * Events

-- | register a callback on an event
onEvent :: (Typeable e, Show e, Eq e) => Event e -> ((EventNumber, EventData e) -> Nomex ()) -> Nomex EventNumber
onEvent = OnEvent

-- | register a callback on an event, disregard the event number
onEvent_ :: forall e. (Typeable e, Show e, Eq e) => Event e -> (EventData e -> Nomex ()) -> Nomex ()
onEvent_ e h = do
    OnEvent e (\(_, d) -> h d)
    return ()

-- | set an handler for an event that will be triggered only once
onEventOnce :: (Typeable e, Show e, Eq e) => Event e -> (EventData e -> Nomex ()) -> Nomex EventNumber
onEventOnce e h = do
    let handler (en, ed) = delEvent_ en >> h ed
    n <- OnEvent e handler
    return n

-- | set an handler for an event that will be triggered only once
onEventOnce_ :: (Typeable e, Show e, Eq e) => Event e -> (EventData e -> Nomex ()) -> Nomex ()
onEventOnce_ e h = do
    let handler (en, ed) = delEvent_ en >> h ed
    OnEvent e handler
    return ()

delEvent :: EventNumber -> Nomex Bool
delEvent = DelEvent

delEvent_ :: EventNumber -> Nomex ()
delEvent_ e = delEvent e >> return ()

delAllEvents :: (Typeable e, Show e, Eq e) => Event e -> Nomex ()
delAllEvents = DelAllEvents

-- | broadcast a message that can be catched by another rule
sendMessage :: (Typeable a, Show a, Eq a) => Msg a -> a -> Nomex ()
sendMessage = SendMessage

sendMessage_ :: Msg () -> Nomex ()
sendMessage_ m = SendMessage m ()

-- | subscribe on a message 
onMessage :: (Typeable m, Show m) => Msg m -> (MsgData m -> Nomex ()) -> Nomex ()
onMessage m f = onEvent_ m f

onMessageOnce :: (Typeable m, Show m) => Msg m -> (MsgData m -> Nomex ()) -> Nomex ()
onMessageOnce m f = onEventOnce_ m f

-- | on the provided schedule, the supplied function will be called
schedule :: (Schedule Freq) -> (UTCTime -> Nomex ()) -> Nomex ()
schedule sched f = do
    now <- getCurrentTime
    let next = head $ starting now $ sched
    if (next == now) then executeAndScheduleNext (f . timeData) sched (TimeData now)
                     else onEventOnce_ (Time next) $ executeAndScheduleNext (f . timeData) sched where

executeAndScheduleNext :: (EventData Time -> Nomex ()) -> (Schedule Freq) -> (EventData Time) -> Nomex ()
executeAndScheduleNext f sched now = do
   f now
   let rest = drop 1 $ starting (timeData now) $ sched
   when (rest /= []) $ onEventOnce_ (Time $ head rest) $ executeAndScheduleNext f sched


schedule_ :: (Schedule Freq) -> Nomex () -> Nomex ()
schedule_ ts f = schedule ts (\_-> f)

--at each time provided, the supplied function will be called
schedule' :: [UTCTime] -> (UTCTime -> Nomex ()) -> Nomex ()
schedule' sched f = do
    let sched' = sort sched
    now <- getCurrentTime
    let nextMay = headMay $ filter (>=now) $ sched'
    case nextMay of
        Just next -> do
           if (next == now) then executeAndScheduleNext' (f . timeData) sched' (TimeData now)
                     else onEventOnce_ (Time next) $ executeAndScheduleNext' (f . timeData) sched'
        Nothing -> return ()
            

executeAndScheduleNext' :: (EventData Time -> Nomex ()) -> [UTCTime] -> (EventData Time) -> Nomex ()
executeAndScheduleNext' f sched now = do
   f now
   let rest = drop 1 $ sched
   when (rest /= []) $ onEventOnce_ (Time $ head rest) $ executeAndScheduleNext' f sched
   

schedule'_ :: [UTCTime] -> Nomex () -> Nomex ()
schedule'_ ts f = schedule' ts (\_-> f)

getCurrentTime :: Nomex UTCTime
getCurrentTime = CurrentTime

-- | durations
oneWeek, oneDay, oneHour, oneMinute :: NominalDiffTime
oneWeek = 7 * oneDay
oneDay = 24 * oneHour
oneHour = 60 * oneMinute
oneMinute = 60
