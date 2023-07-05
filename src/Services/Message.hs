{-# LANGUAGE ScopedTypeVariables #-}
module Services.Message where

import RIO

import qualified Interfaces.DTO as IN
import qualified Interfaces.DAO as IN


data MessageService m = MessageService
    {
        _sendNewOrderMsg :: IN.NewOrderMessageDTO -> m (Either IN.ErrDAO ())
    }

sendNewOrderMsg :: IN.MessagesDAO m -> IN.NewOrderMessageDTO -> m (Either IN.ErrDAO ())
sendNewOrderMsg IN.MessagesDAO {..} newOrderDTO = _sendNewOrderMsg newOrderDTO
