module Adapter.Storage.Hasql.Statement where

import RIO
import Data.Functor.Contravariant (contramap)
import Hasql.Statement
import Hasql.TH as TH
import qualified Hasql.Decoders as DEC
import qualified Hasql.Encoders as ENC
import qualified Interfaces.DTO as IN
import qualified Domain.Models as D


queryStatement :: Text -> Statement (Vector Int64) (Vector Int64)
queryStatement tableName =
    [TH.vectorStatement|SELECT foo :: int8 FROM "orders"|]

-- -- Function to insert a new order into the "orders" table
-- insertNewOrder :: Statement D.UserId D.OrderId
-- insertNewOrder = 
--       dimap
--         (\ (D.UserId a) -> a)
--         (\ case
--         Just (a) -> Just . D.OrderId . fromIntegral $ a
--         Nothing -> Nothing)
--         [TH.singletonStatement|
--         insert into orders 
--         values $1 :: int8
--         returning (id :: int8)|]
    -- Statement sql encoder decoder True
    -- where
    --     sql = "INSERT INTO orders (user_id) VALUES ($1) RETURNING id"
    --     encoder = contramap D.UserId $ ENC.param $ ENC.nonNullable $ D.UserId . fromIntegral <$> ENC.int8
    --     decoder = DEC.singleRow . DEC.column . DEC.nonNullable $ D.OrderId . fromIntegral <$> DEC.int8

-- -- Function to insert the order items into the "order_items" table
-- insertOrderItems :: Connection -> D.OrderId -> [IN.NewOrderItemDTO] -> IO (Either Session.QueryError ())
-- insertOrderItems conn orderId orderItems = run session conn
--   where
--     session =
--       statement (orderId, orderItems) $ Statement sql encoder decoder True
--     sql = "INSERT INTO order_items (order_id, product_id, count, product_price) VALUES ($1, $2, $3, $4)"
--     encoder =
--       contramap (unOrderId . fst) (param (ENC.nonNullable ENC.int8))
--         <> contramap (IN.nOrItProductId . snd) (param (ENC.nonNullable ENC.int8))
--         <> contramap (fromIntegral . IN.nOrItCount . snd) (param (ENC.nonNullable ENC.int8))
--         <> contramap (IN.nOrItProductPrice . snd) (param (ENC.nonNullable ENC.float4))
--     decoder = DEC.noResult