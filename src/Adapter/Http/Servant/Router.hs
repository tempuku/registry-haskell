{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Adapter.Http.Servant.Router where
    
import RIO hiding (Handler)
import Servant
import qualified Network.Wai as Wai
import qualified Control.Monad.Except as Except

import Interfaces.Usecases (Usecases)
import Adapter.Http.Servant.Handlers
import qualified Interfaces.Usecases as UC
import qualified Interfaces.Logger as IN
import Adapter.Http.Servant.Schemas

-- Define your application
app :: (MonadThrow m, IN.Logger m) => Usecases m -> (forall a. m a -> IO a) -> IO Wai.Application
app usecases runner =  pure $ serve api $
  hoistServer api (ioToHandler . runner) (server usecases)
  where
    api :: Proxy API
    api = Proxy
    ioToHandler = Handler . Except.ExceptT . try

type API = 
    Get '[JSON] NoContent -- health check
    :<|> "orders" :> ReqBody '[JSON] CreateOrderRequest :> Post '[JSON] CreateOrderResponse

server :: (MonadThrow m, IN.Logger m) => Usecases m -> ServerT API m
server usecases = healthH :<|> newOrder
    where
        healthH = pure NoContent
        newOrder = makeOrderHandler (UC._makeOrderUsecase usecases)
