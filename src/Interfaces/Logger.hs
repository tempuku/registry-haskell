module Interfaces.Logger where

import RIO


class Monad m => Logger m where
  logDebug :: Show a => a -> m ()
