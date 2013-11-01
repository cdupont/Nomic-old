{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables #-}

-- | All the building blocks to allow rules to build variables.
module Language.Nomyx.Variables where

import Language.Nomyx.Expression
import Language.Nomyx.Events
import Data.Typeable
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Data.Map hiding (map, filter, insert, mapMaybe, null)


-- * Variables
-- | variable creation
newVar :: (Typeable a, Show a, Eq a) => VarName -> a -> Nomex (Maybe (V a))
newVar = NewVar

newVar_ :: (Typeable a, Show a, Eq a) => VarName -> a -> Nomex (V a)
newVar_ s a = partial "newVar_: Variable existing" (newVar s a)

-- | variable reading
readVar :: (Typeable a, Show a, Eq a) => (V a) -> Nomex (Maybe a)
readVar = ReadVar

readVar_ :: forall a. (Typeable a, Show a, Eq a) => (V a) -> Nomex a
readVar_ v@(V a) = partial ("readVar_: Variable \"" ++ a ++ "\" with type \"" ++ (show $ typeOf v) ++ "\" not existing") (readVar v)

-- | variable writing
writeVar :: (Typeable a, Show a, Eq a) => (V a) -> a -> Nomex Bool
writeVar = WriteVar

writeVar_ :: (Typeable a, Show a, Eq a) => (V a) -> a -> Nomex ()
writeVar_ var val = void $ writeVar var val

-- | modify a variable using the provided function
modifyVar :: (Typeable a, Show a, Eq a) => (V a) -> (a -> a) -> Nomex ()
modifyVar v f = writeVar_ v . f =<< readVar_ v

-- | delete variable
delVar :: (V a) -> Nomex Bool
delVar = DelVar

delVar_ :: (V a) -> Nomex ()
delVar_ v = void $ delVar v

-- * Message Variable
-- | a MsgVar is a variable with a message attached, allowing to trigger registered functions anytime the var if modified
data VEvent a = VUpdated a | VDeleted deriving (Typeable, Show, Eq)
data MsgVar a = MsgVar {message :: (Msg (VEvent a)), variable :: (V a) }

msgVar :: String -> MsgVar a
msgVar a = MsgVar (Message a) (V a)

newMsgVar :: (Typeable a, Show a, Eq a) => VarName -> a -> Nomex (Maybe (MsgVar a))
newMsgVar name a = do
    mv <- newVar name a
    return $ mv >>= Just . MsgVar (Message name)

newMsgVar_ :: (Typeable a, Show a, Eq a) => VarName -> a -> Nomex (MsgVar a)
newMsgVar_ name a = partial "newMsgVar_: Variable existing" (newMsgVar name a)


newMsgVar' :: (Typeable a, Show a, Eq a) => VarName -> a -> (VEvent a -> Nomex()) -> Nomex (Maybe (MsgVar a))
newMsgVar' name a f = do
    mv <- newMsgVar name a
    case mv of
       Just (MsgVar m _) -> do
          onMessage m $ f . messageData
          return mv
       Nothing -> return Nothing

writeMsgVar :: (Typeable a, Show a, Eq a) => MsgVar a -> a -> Nomex Bool
writeMsgVar (MsgVar m v) a = do
   r <- writeVar v a
   sendMessage m (VUpdated a)
   return r

writeMsgVar_ :: (Typeable a, Show a, Eq a) => MsgVar a -> a -> Nomex ()
writeMsgVar_ mv a = void $ writeMsgVar mv a

readMsgVar :: (Typeable a, Show a, Eq a) => MsgVar a -> Nomex (Maybe a)
readMsgVar (MsgVar _ v) = readVar v

readMsgVar_ :: (Typeable a, Show a, Eq a) => MsgVar a -> Nomex a
readMsgVar_ mv = partial "readMsgVar_: variable not existing" (readMsgVar mv)

modifyMsgVar :: (Typeable a, Show a, Eq a) => MsgVar a -> (a -> a) -> Nomex ()
modifyMsgVar mv f = writeMsgVar_ mv . f =<< readMsgVar_ mv

delMsgVar :: (Typeable a, Show a, Eq a) => MsgVar a -> Nomex Bool
delMsgVar (MsgVar m v) = do
   sendMessage m VDeleted
   delAllEvents m
   delVar v

onMsgVarChange :: (Typeable a, Show a, Eq a) => MsgVar a -> (VEvent a -> Nomex()) -> Nomex ()
onMsgVarChange mv f = do
   m <- getMsgVarMessage mv
   onMessage m $ \(MessageData v) -> f v

onMsgVarEvent :: (Typeable a, Show a, Eq a)
               => MsgVar a
               -> (a -> Nomex b)
               -> (a -> b -> Nomex())
               -> (b -> Nomex())
               -> Nomex ()
onMsgVarEvent mv create update delete = do
   val <- readMsgVar_ mv
   c <- create val
   onMsgVarChange mv $ f c where
      f c' (VUpdated v) = update v c'
      f c' VDeleted     = delete c'

-- | get the messsage triggered when the array is filled
getMsgVarMessage :: (Typeable a, Show a, Eq a) => (MsgVar a) -> Nomex (Msg (VEvent a))
getMsgVarMessage (MsgVar m _) = return m

-- | get the association array
getMsgVarData :: (Typeable a, Show a, Eq a) => (MsgVar a) -> Nomex (Maybe a)
getMsgVarData (MsgVar _ v) = readVar v

getMsgVarData_ :: (Typeable a, Show a, Eq a) => (MsgVar a) -> Nomex a
getMsgVarData_ (MsgVar _ v) = readVar_ v

getMsgVarName :: (Typeable a, Show a, Eq a) => (MsgVar a) -> String
getMsgVarName (MsgVar _ (V varName)) = varName

    
-- * Variable arrays
-- | ArrayVar is an indexed array with a signal attached triggered at every change.
-- | each indexed elements starts empty (value=Nothing).
type ArrayVar i a = MsgVar [(i, Maybe a)]

-- | initialize an empty ArrayVar
newArrayVar :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i) => VarName -> [i] -> Nomex (Maybe (ArrayVar i a))
newArrayVar name l = do
    let list = map (\i -> (i, Nothing)) l
    newMsgVar name list

newArrayVar_ :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i) => VarName -> [i] -> Nomex (ArrayVar i a)
newArrayVar_ name l = partial "newArrayVar_: Variable existing" (newArrayVar name l)

-- | initialize an empty ArrayVar, registering a callback that will be triggered at every change
newArrayVar' :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i) => VarName -> [i] -> (VEvent [(i,Maybe a)] -> Nomex ()) -> Nomex (Maybe (ArrayVar i a))
newArrayVar' name l f = do
    let list = map (\i -> (i, Nothing)) l
    newMsgVar' name list f

-- | initialize an empty ArrayVar, registering a callback.
-- the ArrayVar will be deleted when full
newArrayVarOnce :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => VarName -> [i] -> (VEvent [(i, Maybe a)] -> Nomex ()) -> Nomex (Maybe (ArrayVar i a))
newArrayVarOnce name l f = do
   mv <- newArrayVar' name l f
   when (isJust mv) $ cleanOnFull $ fromJust mv
   return mv

cleanOnFull :: (Typeable a, Show a, Eq a, Ord i, Typeable i, Show i) => (ArrayVar i a) -> Nomex ()
cleanOnFull ar = do
   m <- getMsgVarMessage ar 
   onMessage m $ \_ -> do
      full <- (isFullArrayVar_ ar)
      when full $ void $ delMsgVar ar
      return ()
   return ()

isFullArrayVar_ :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Nomex Bool
isFullArrayVar_ av = do
   md <- getMsgVarData av
   return $ and $ map isJust $ map snd $ fromJust md
       
   
-- | store one value and the given index. If this is the last filled element, the registered callbacks are triggered.
putArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> i -> a -> Nomex Bool
putArrayVar mv i a = do
    ma <- readMsgVar mv
    case ma of
       Just ar -> do
          let ar2 = M.insert i (Just a) (fromList ar)
          writeMsgVar mv (toList ar2)
       Nothing -> return False

putArrayVar_ :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> i -> a -> Nomex ()
putArrayVar_ mv i a = void $ putArrayVar mv i a       

