{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Adapter.Storage.Hasql.Order where

import qualified Interfaces.DAO as IN
import qualified Interfaces.DTO as IN
import qualified Domain.Models as D
import qualified Adapter.Storage.Hasql.Statement as ST
import qualified Hasql.Session as Session
-- import qualified PostgreSQL.ErrorCodes as PgErr
import RIO hiding (trace)
import qualified Interfaces.Usecases as UC
import Data.Functor.Contravariant (contramap)
import Hasql.Connection
import qualified Hasql.Decoders as DEC
import qualified Hasql.Encoders as ENC
import Hasql.Transaction
import Hasql.Transaction.Sessions
import qualified Hasql.Session as Session
import RIO.Partial (toEnum)
import RIO.Time (UTCTime)
import Prelude (print)
import RIO.List.Partial (head)


-- execTransaction :: MonadIO m => HConn.Connection -> Transaction.Transaction -> m (Either Session.QueryError b)
-- execQuery conn transaction = liftIO $ Session.run (Transaction.run transaction) conn

-- data Order = Order {
--     orderId :: D.OrderId,
--     orderUserId :: D.UserId,
--     orderCreatedAt :: UTCTime,
--     orderStatus :: D.OrderStatus
-- }


-- -- Encode/Decode functions for PostgreSQL queries
-- orderItemDecoder :: DEC.Row D.OrderItem
-- orderItemDecoder =
--   D.OrderItem
--     <$> DEC.column (DEC.nonNullable (D.OrderItemId . fromIntegral <$> DEC.int8))
--     <*> DEC.column (DEC.nonNullable (D.OrderId . fromIntegral <$> DEC.int8))
--     <*> DEC.column (DEC.nonNullable (D.ProductId . fromIntegral <$> DEC.int8))
--     <*> DEC.column (DEC.nonNullable (fromIntegral <$> DEC.int8))
--     <*> DEC.column (DEC.nonNullable DEC.float4)


-- orderDecoder :: DEC.Row Order
-- orderDecoder =
--   Order
--     <$> DEC.column (DEC.nonNullable (fmap (D.OrderId . fromIntegral) DEC.int8))
--     <*> DEC.column (DEC.nonNullable (fmap (D.UserId . fromIntegral) DEC.int8))
--     <*> DEC.column (DEC.nonNullable DEC.timestamptz)
--     <*> DEC.column (DEC.nonNullable $ toEnum . fromIntegral <$> DEC.int2)


-- orderEncoder :: Order -> ENC.Params Order
-- orderEncoder order =
--   contramap orderId (ENC.param (ENC.nonNullable $ D.OrderId . fromIntegral <$> ENC.int8))
--     <> contramap orderUserId (ENC.param (ENC.nonNullable $ D.UserId . fromIntegral <$> ENC.int8))
--     <> contramap orderCreatedAt (ENC.param (ENC.nonNullable ENC.timestamptz))
--     <> contramap orderStatus (ENC.param (ENC.nonNullable $ toEnum . fromIntegral <$> ENC.int2))

-- orderItemEncoder :: D.OrderItem -> ENC.Params D.OrderItem
-- orderItemEncoder orderItem =
--   contramap D.orderItemId (ENC.param (ENC.nonNullable $ D.OrderItemId . fromIntegral <$> ENC.int8))
--     <> contramap D.orderItemProductId (ENC.param (ENC.nonNullable $ D.orderItemProductId . fromIntegral <$> ENC.int8))
--     <> contramap D.orderItemCount (ENC.param (ENC.nonNullable $ fromIntegral <$> ENC.int8))
--     <> contramap D.orderItemProductPrice (ENC.param (ENC.nonNullable ENC.float4))

-- Encode/Decode functions for PostgreSQL queries
-- newOrderItemEncoder :: IN.NewOrderItemDTO -> ENC.Params IN.NewOrderItemDTO
-- newOrderItemEncoder newOrderItem =
--   contramap IN.nOrItProductId (ENC.param $ ENC.nonNullable $ D.ProductId . fromIntegral <$> ENC.int8)
--     <> contramap IN.nOrItCount $ ENC.param . ENC.nonNullable $ ENC.int8
--     <> contramap IN.nOrItProductPrice (param (ENC.nonNullable ENC.float4))

-- newOrderEncoder :: IN.NewOrderDTO -> ENC.Params IN.NewOrderDTO 
-- newOrderEncoder newOrder =
--   contramap IN.nOrUserId (param (ENC.nonNullable ENC.int8))
--     <> contramap IN.nOrOrderItems (param (ENC.nonNullable (ENC.array newOrderItemEncoder)))

-- Function to insert a new order and its order items within a transaction
insertNewOrderWithTransaction :: Connection -> IN.NewOrderDTO -> IO (Either Session.QueryError [D.OrderItemId])
insertNewOrderWithTransaction conn IN.NewOrderDTO {..} = Session.run mySession conn
  where
    mySession =
      transaction ReadCommitted Write $ do
        -- Insert the new order
        orderId <- statement nOrUserId ST.insertNewOrder
        -- Insert the order items if the order insertion succeeded
        traverse (\ a ->statement (orderId, a) ST.insertOrderItems) nOrOrderItems
        -- statement (orderId, head nOrOrderItems) ST.insertOrderItems

-- Function to insert a new order into the "orders" table
-- insertNewOrder :: Connection -> IN.NewOrderDTO -> IO (Either Session.QueryError D.OrderId)
-- insertNewOrder conn IN.NewOrderDTO {..} = session conn do $
--   orderId <- Session.statement IN.nOrUserId ST.insertNewOrder 
--   where
--     session =
--       statement (IN.nOrUserId newOrder) $ Statement sql encoder decoder True
--     sql = "INSERT INTO orders (user_id) VALUES ($1) RETURNING id"
--     encoder = contramap IN.nOrUserId (param (ENC.nonNullable ENC.int8))
--     decoder = DEC.singleRow (DEC.column (DEC.nonNullable (fmap (OrderId . fromIntegral) DEC.noResult)))

-- Function to insert the order items into the "order_items" table
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


-- -- Function to insert a new order into the database
-- insertNewOrder :: Connection -> IN.NewOrderDTO -> IO (Either Session.QueryError ())
-- insertNewOrder conn orderDto =
--   transaction conn $ do
--     -- Insert the order and retrieve its ID
--     orderInsertResult <- run (statement (IN.nOrUserId orderDto) (Statement "INSERT INTO \"order\" (user_id) VALUES ($1) RETURNING id" orderEncoder (DEC.rowMaybe orderDecoder) True))
--     case orderInsertResult of
--       Left err -> condemn >= return (Left err)
--       Right (Just order) -> do
--         -- Insert the order items
--         let orderItems = IN.nOrOrderItems orderDto
--         itemsInsertResult <- run (statement orderItems (Statement "INSERT INTO order_item (order_id, product_id, count, product_price) VALUES ($1, $2, $3, $4)" (DEC.rowList (DEC.nonNullable $ DEC.composite orderItemEncoder)) DEC.noResult True))
--         case itemsInsertResult of
--           Left err -> return (Left err)
--           Right _ -> return (Right ())
