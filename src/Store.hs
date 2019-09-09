{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Store where

import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.DeepSeq
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           Prelude
import           React.Flux

import qualified React.Flux.Rn.APIs            as RnA

data Todo = Todo {
    todoText      :: String
  , todoComplete  :: Bool
  , todoIsEditing :: Bool
} deriving (Show, Typeable, Eq)

data Filter = AcceptAll | AcceptActive | AcceptCompleted
  deriving (Show, Eq, Typeable, NFData, Generic)

data TodoState = TodoState {
    todoList   :: [(Int, Todo)]
  , todoFilter :: Filter
} deriving (Show, Typeable, Eq)

data TodoAction = TodoCreate String
                | TodoCreateAction
                | TodoDelete Int
                | TodoEdit Int
                | UpdateText Int String
                | CancelUpdate Int
                | CancelUpdateWithDelay Int
                | ToggleAllComplete
                | TodoSetComplete Int Bool
                | ClearCompletedTodos
                | SetFilter Filter
                | Alert String
                | OpenLink String
  deriving (Show, Typeable, Generic, NFData, Eq)

instance StoreData TodoState where
  type StoreAction TodoState = TodoAction
  transform action (TodoState todos filt) = do
    putStrLn $ "Action: " ++ show action
    putStrLn $ "Initial todos: " ++ show todos

    -- Care is taken here to leave the Haskell object for the pair (Int, Todo) unchanged if the todo
    -- itself is unchanged.  This allows React to avoid re-rendering the todo when it does not change.
    -- For more, see the "Performance" section of the React.Flux haddocks.
    (newTodos, newFilter) <- return $ case action of
      (TodoCreate txt) ->
        let index = if null todos then 0 else maximum (map fst todos)
        in  ((index + 1, Todo txt False False) : todos, filt)
      (TodoDelete i) -> (filter ((/= i) . fst) todos, filt)
      (TodoEdit i) ->
        let f (idx, todo) | idx == i = (idx, todo { todoIsEditing = True })
            f p                      = p
        in  (map f todos, filt)
      (UpdateText newIdx newTxt) ->
        let f (idx, todo) | idx == newIdx =
                (idx, todo { todoText = newTxt, todoIsEditing = False })
            f p = p
        in  (map f todos, filt)
      (CancelUpdate newIdx) ->
        let f (idx, todo) | idx == newIdx =
                (idx, todo { todoIsEditing = False })
            f p = p
        in  (map f todos, filt)
      ToggleAllComplete -> if all (todoComplete . snd) todos
        then
          ([ (idx, Todo txt False False) | (idx, Todo txt _ _) <- todos ], filt)
        else
          ([ (idx, Todo txt True False) | (idx, Todo txt _ _) <- todos ], filt)
      TodoSetComplete newIdx newComplete ->
        let f (idx, todo) | idx == newIdx =
                (idx, todo { todoComplete = newComplete })
            f p = p
        in  (map f todos, filt)
      ClearCompletedTodos -> (filter (not . todoComplete . snd) todos, filt)
      SetFilter             newFilt -> (todos, newFilt)
      Alert                 _       -> (todos, filt)
      OpenLink              _       -> (todos, filt)
      CancelUpdateWithDelay _       -> (todos, filt)
    case action of
      Alert                 msg -> RnA.alert msg Nothing
      OpenLink              url -> RnA.openURL url
      CancelUpdateWithDelay i   -> do
        forkIO $ do
          threadDelay $ 1000 * 1000
          executeAction (todoAction $ CancelUpdate i)
        return ()
      _ -> return ()
    putStrLn $ "New todos: " ++ show newTodos
    putStrLn $ "New filter: " ++ show newFilter
    return $ TodoState newTodos newFilter

todoAction :: TodoAction -> SomeStoreAction
todoAction = action @TodoState

appStore = TodoState
  [(0, Todo "Learn react" True False), (1, Todo "Learn react-flux" False False)]
  AcceptAll
