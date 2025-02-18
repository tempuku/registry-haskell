module Adapter.Storage.Hasql.Statement where

import RIO
import Data.Functor.Contravariant (contramap)
import Hasql.Statement
import Hasql.TH
import qualified Hasql.Decoders as DEC
import qualified Hasql.Encoders as ENC
import qualified Interfaces.DTO as IN
import qualified Domain.Models as D
import Control.Lens (dimap)
import RIO.Time (UTCTime)
import RIO.Partial (toEnum)


data Order = Order {
    orderId :: D.OrderId,
    orderUserId :: D.UserId,
    orderCreatedAt :: UTCTime,
    orderStatus :: D.OrderStatus
}

-- Encode/Decode functions for PostgreSQL queries
orderItemDecoder :: DEC.Row D.OrderItem
orderItemDecoder =
  D.OrderItem
    <$> DEC.column (DEC.nonNullable (D.OrderItemId . fromIntegral <$> DEC.int8))
    <*> DEC.column (DEC.nonNullable (D.OrderId . fromIntegral <$> DEC.int8))
    <*> DEC.column (DEC.nonNullable (D.ProductId . fromIntegral <$> DEC.int8))
    <*> DEC.column (DEC.nonNullable (fromIntegral <$> DEC.int8))
    <*> DEC.column (DEC.nonNullable DEC.float4)

orderDecoder :: DEC.Row Order
orderDecoder =
  Order
    <$> DEC.column (DEC.nonNullable (fmap (D.OrderId . fromIntegral) DEC.int8))
    <*> DEC.column (DEC.nonNullable (fmap (D.UserId . fromIntegral) DEC.int8))
    <*> DEC.column (DEC.nonNullable DEC.timestamptz)
    <*> DEC.column (DEC.nonNullable $ toEnum . fromIntegral <$> DEC.int2)

-- -- Function to insert a new order into the "orders" table
-- insertNewOrder :: Statement D.UserId (Maybe D.OrderId)
-- insertNewOrder =
--       dimap
--         (\ (D.UserId a) -> fromIntegral a)
--         (\ case
--         Just a -> Just . D.OrderId . fromIntegral $ a
--         Nothing -> Nothing)
--         [maybeStatement|
--         insert into orders 
--         values ($1 :: int8)
--         returning (id :: int8)|]

insertNewOrder :: Statement D.UserId Order
insertNewOrder = Statement sql encoder decoder True
    where
        sql = "INSERT INTO orders (user_id) VALUES ($1) RETURNING id, user_id, created_at, status"
        encoder = contramap (\ (D.UserId a) -> fromIntegral a) $ ENC.param $ ENC.nonNullable ENC.int8
        decoder = DEC.singleRow orderDecoder

insertOrderItems :: Statement (D.OrderId, IN.NewOrderItemDTO) D.OrderItem
insertOrderItems = Statement sql encoder decoder True
    where
      sql = "INSERT INTO order_items (order_id, product_id, count, product_price) VALUES ($1, $2, $3, $4)" 
            <> "RETURNING id, order_id, product_id, count, product_price"
      encoder =
        contramap ((\ (D.OrderId a) -> fromIntegral a) . fst) (ENC.param (ENC.nonNullable ENC.int8))
          <> contramap ((\ (D.ProductId a) -> fromIntegral a) . IN.nOrItProductId . snd) (ENC.param (ENC.nonNullable ENC.int8))
          <> contramap (fromIntegral . IN.nOrItCount . snd) (ENC.param (ENC.nonNullable ENC.int8))
          <> contramap (IN.nOrItProductPrice . snd) (ENC.param (ENC.nonNullable ENC.float4))
      decoder = DEC.singleRow orderItemDecoder

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
