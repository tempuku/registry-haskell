module Main where

import RIO
import qualified RIO.Map as Map
import System.IO
import qualified Network.Wai.Handler.Warp as Warp
import qualified Config.Config as Config

import Usecases
import qualified Adapter.Storage.InMemory.ProductPrices as HasqlUserRepo
import qualified Domain.Models as D
import qualified Interfaces.DAO as IN
import qualified Interfaces.Logger as IN
import qualified Interfaces.Usecases as IN
import qualified Helpers as HP
import qualified Services.Order as UC
import qualified Services.EventPipe as UC
import Adapter.Http.Servant.Router


main :: IO ()
main = do
    writeSwaggerJSON
    port <- Config.getIntFromEnv "PORT" 3000
    storageBackend <- Config.getStringFromEnv "STORAGE" "inMem"
    newOrderQueue <- newTQueueIO
    router <- case storageBackend of
            "inMem" -> app (usecasesBuilder newOrderQueue inMemProductPricesDAO) $ runRIO ()
            _ -> error $ "Incorrect storage:" <> storageBackend
    eventPipeProcessorStart newOrderQueue
    Warp.run port router

usecasesBuilder :: (IN.Logger m, MonadUnliftIO m) =>UC.NewOrdersPipe -> IN.ProductPricesDAO m -> IN.Usecases m
usecasesBuilder orderQueue productPricesDAO = IN.Usecases (
                makeOrderUsecase ordersService
            )
            where
                ordersService = UC.OrdersService (
                        UC.makeOrder orderQueue productPricesDAO HP.enrichOrderItemsDataWithPrices
                    ) 

inMemProductPricesDAO :: (MonadUnliftIO m) =>IN.ProductPricesDAO m 
inMemProductPricesDAO = IN.ProductPricesDAO (
                HasqlUserRepo.getMap inMemoryPrices
            )
            where
                inMemoryPrices = Map.fromList [
                        (D.ProductId 1, 1.0) 
                        , (D.ProductId 2, 2.0)
                        , (D.ProductId 3, 3.0)
                        , (D.ProductId 4, 4.0)
                    ]

eventPipeProcessorStart :: UC.NewOrdersPipe -> IO ()
eventPipeProcessorStart orderQueue = UC.eventPipeProcessorRunner eventPipes eventPipeProcessorService
    where
        eventPipes = UC.EventPipes (
            orderQueue
            )
        eventPipeProcessorService = UC.EventPipeProcessor (
            UC.processNewOrder
            )

instance IN.Logger (RIO a) where
  logDebug msg = liftIO $ print msg

instance IN.Logger IO where
  logDebug msg = print msg
