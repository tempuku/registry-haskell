{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Adapter.Http.Servant.Router where

import RIO hiding (Handler, (.~))
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Data.Swagger
import Control.Lens
import qualified Network.Wai as Wai
import qualified Control.Monad.Except as Except
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Domain.Models as D
import Interfaces.Usecases (Usecases)
import Adapter.Http.Servant.Handlers
import qualified Interfaces.Usecases as UC
import qualified Interfaces.Logger as IN
import Adapter.Http.Servant.Schemas
import Data.Aeson
import qualified RIO.ByteString as BL8
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.ByteString as BS

appAPI :: Proxy API
appAPI = Proxy

-- Define your application
app :: (MonadThrow m, IN.Logger m) => Usecases m -> (forall a. m a -> IO a) -> IO Wai.Application
app usecases runner =  pure $ serve appAPI $
  hoistServer appAPI (ioToHandler . runner) (server usecases)
  where
    ioToHandler = Handler . Except.ExceptT . try

type AppAPI =
    Get '[JSON] NoContent -- health check
    :<|> "orders" :> ReqBody '[JSON] CreateOrderRequest :> Post '[JSON] CreateOrderResponse

type API = 
      SwaggerSchemaUI "swagger-ui" "swagger.json"
      :<|> AppAPI

server :: (MonadThrow m, IN.Logger m) => Usecases m -> ServerT API m
server usecases = 
                  swaggerServer
                  :<|> healthH :<|> newOrder
    where
        healthH = pure NoContent
        newOrder = makeOrderHandler (UC._makeOrderUsecase usecases)
        swaggerServer = swaggerSchemaUIServerT todoSwagger

instance ToSchema CreateOrderRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "This is some real Order right here"
    & mapped.schema.example ?~ toJSON (CreateOrderRequest (D.UserId 1) [CreateOrderRequestItem (D.ProductId 1) 1])

instance ToSchema CreateOrderRequestItem
instance ToSchema D.ProductId
instance ToSchema D.UserId
instance ToParamSchema CreateOrderResponse
instance ToSchema CreateOrderResponse

-- | Swagger spec for Todo API.
todoSwagger :: Swagger
todoSwagger = toSwagger (Proxy :: Proxy AppAPI)
  & info.title   .~ "Registry API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (BS.concat . LBS.toChunks $ encodePretty todoSwagger)