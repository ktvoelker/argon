
module Queue where

import Types

import Graphics.X11

data QueueMode = QMTile | QMFloat deriving (Eq, Ord, Show)

newtype WindowQueue = WindowQueue (IORef WindowQueueData)

data WindowQueueData =
  WindowQueueData
  { mode  :: QueueMode
  , front :: IORef WQ
  , back  :: IORef WQ
  }

data WQ =
    Null
  | Node
    { window :: Window
    , prev   :: (IORef WQ)
    , next   :: (IORef WQ)
    }

get :: (MonadIO m) => WindowQueue -> m WindowQueueData
get (WindowQueue wqdRef) = readRef wqdRef

put :: (MonadIO m) => WindowQueue -> WindowQueueData -> m ()
put (WindowQueue wqdRef) = writeRef wqdRef

modifyM :: (MonadIO m)
       => WindowQueue -> (WindowQueueData -> m (WindowQueueData, a)) -> m a
modifyM (WindowQueue wqdRef) = modifyRefM wqdRef

newRef :: (MonadIO m) => a -> m (IORef a)
newRef = liftIO . newIORef

readRef :: (MonadIO m) => IORef a -> m a
readRef = liftIO . readIORef

writeRef :: (MonadIO m) => IORef a -> a -> m ()
writeRef r = liftIO . writeIORef r

modifyRef :: (MonadIO m) => IORef a -> (a -> a) -> m ()
modifyRef r = liftIO . modifyIORef r

modifyRefM :: (MonadIO m) => IORef a -> (a -> m (a, b)) -> m b
modifyRefM r f = do
  x       <- readRef r
  (x', v) <- f x
  writeRef r x'
  return v

newWindowQueue :: (MonadIO m) => QueueMode -> m WindowQueue
newWindowQueue m = do
  null <- newRef Null
  wqd  <- newRef WindowQueueData
    { mode  = m
    , front = null
    , back  = null
    }
  return $ WindowQueue wqd

extractFront, extractBack :: (MonadIO m) => WindowQueue -> m (Maybe Window)

insertFront, insertBack :: (MonadIO m) => WindowQueue -> Window -> m ()

insertFront wq win = modifyM wq $ \wqd -> do
  null   <- newRef Null
  front' <- newRef Node
    { window = win
    , prev   = null
    , next   = front wqd
    }
  modifyRef (front wqd) $ \front -> front { prev = front' }
  return wqd { front = front' }

extractFront wq = modifyM wq $ \wqd -> do
  frontNode <- readRef $ front wqd
  case frontNode of
    Null   -> return Nothing
    n@Node -> do
      let wqd' = wqd { front
