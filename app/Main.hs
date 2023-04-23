module Main where

import RIO
import RIO.Time
import qualified RIO.Map as Map
import System.IO
import Control.Concurrent (forkIO)
import qualified Adapter.Storage.InMemory.ProductPrices as HasqlUserRepo
import qualified Domain.Models as D
import qualified Interfaces.DAO as IN
import qualified Interfaces.DTO as IN
import qualified Usecases.Helpers as UC
import qualified Usecases.Services as UC
import qualified Usecases.Interactors as UC
import Control.Exception.Base (absentError)

main :: IO ()
main = do
    let inMemoryPrices = Map.fromList [(D.ProductId 1, 1.0), (D.ProductId 2, 2.0), (D.ProductId 3, 3.0), (D.ProductId 4, 4.0)]
    let productPricesDAO = IN.ProductPricesDAO (
            HasqlUserRepo.getMap inMemoryPrices
            )

    newOrderQueue <- newTQueueIO
    let ordersService = UC.OrdersService (
            UC.makeOrder newOrderQueue productPricesDAO UC.enrichOrderItemsDataWithPrices
            ) 
    let eventPipes = UC.EventPipes (
                newOrderQueue
            )
    let eventPipeProcessorService = UC.EventPipeProcessor (
            UC.processNewOrder
            )
    let makeOrderItemDTO1 = IN.MakeOrderItemDTO (D.ProductId 1)  1
    let makeOrderItemDTO2 = IN.MakeOrderItemDTO (D.ProductId 2) 2
    let makeOrderDTO = IN.MakeOrderDTO (D.UserId 1) [makeOrderItemDTO1, makeOrderItemDTO2]
    a <- UC._makeOrder ordersService makeOrderDTO
    UC.logDebug a
    UC.eventPipeProcessor eventPipes eventPipeProcessorService
    UC.logDebug "krinz"


instance UC.Logger IO where
  logDebug msg = print msg
