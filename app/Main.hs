module Main where

import RIO
import RIO.Time
import System.IO
import qualified Domain.Models as M
import qualified Interfaces.DAO as IN
import qualified Interfaces.DTO as IN
import qualified Usecases.Services as UC

main :: IO ()
main = do
--     productPricesDAO <- IN.ProductPricesDAO (
--         UC.getMap
--     )

--     ordersService <- UC.OrdersService (
--         (UC.makeOrder productPricesDAO)
--     )
    putStrLn "krinz"
