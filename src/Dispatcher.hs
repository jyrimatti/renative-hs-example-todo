{-# LANGUAGE TypeApplications #-}
module Dispatcher (dispatchTodo, handleTodo) where

import           Prelude    ((.))
import           React.Flux
import           Store

dispatchTodo :: TodoAction -> [SomeStoreAction]
dispatchTodo a = [action @TodoState a]

handleTodo :: TodoAction -> ([SomeStoreAction], [EventModification])
handleTodo = simpleHandler . dispatchTodo
