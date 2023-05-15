{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Adapter.Http.Servant.Router where

import Adapter.Http.Servant.Schemas
import RIO
import Servant
import Interfaces.Usecases (Usecases)
import Adapter.Http.Servant.Handlers
import qualified Interfaces.Usecases as UC
import qualified Interfaces.Logger as IN
import qualified Network.Wai as Wai

-- start :: (MonadIO m, IN.Logger m, MonadThrow m) => Usecases m -> m Application
-- start usecases =
--     pure $ serve (Proxy :: Proxy API) (server usecases)

type API = 
    Get '[JSON] NoContent -- health check
    :<|> "orders" :> ReqBody '[JSON] CreateOrderRequest :> Post '[JSON] CreateOrderResponse

server :: (MonadThrow m, IN.Logger m) => Usecases m -> ServerT API m
server usecases = healthH :<|> newOrder
    where
        healthH = pure NoContent
        newOrder = makeOrderHandler (UC._makeOrderUsecase usecases)
