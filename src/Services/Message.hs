{-# LANGUAGE ScopedTypeVariables #-}
module Services.Message where

import RIO

import qualified Interfaces.DTO as IN
import qualified Interfaces.DAO as IN


data MessageService m a = MessageService
    {
        _sendNewOrderMsg :: IN.TargetTopic a -> IN.NewOrderMessageDTO -> m (Either IN.ErrDAO ())
    }

sendNewOrderMsg :: IN.MessagesDAO m a -> IN.TargetTopic a -> IN.NewOrderMessageDTO -> m (Either IN.ErrDAO ())
sendNewOrderMsg IN.MessagesDAO {..} targetTopic newOrderDTO = _sendNewOrderMsg targetTopic newOrderDTO
