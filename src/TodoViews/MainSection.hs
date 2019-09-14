{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TodoViews.MainSection where

import           Control.Monad                                     (forM_,
                                                                    unless,
                                                                    when)
import           Prelude                                           (Maybe (..),
                                                                    all, filter,
                                                                    id, length,
                                                                    not, snd,
                                                                    ($), (++),
                                                                    (-), (.),
                                                                    (==))

import           React.Flux.Rn.Views

import           React.Flux                                        (elemShow,
                                                                    elemString)
import           React.Flux.Rn.Components.Text
import           React.Flux.Rn.Components.TouchableOpacity
import           React.Flux.Rn.Components.TouchableWithoutFeedback
import           React.Flux.Rn.Components.View

import           Components
import           Dispatcher
import           Store

mainSection todoState@(TodoState todos filt) =
    let doFilter AcceptAll       = id
        doFilter AcceptActive    = filter (not . todoComplete . snd)
        doFilter AcceptCompleted = filter (todoComplete . snd)
        allCompleted = all (todoComplete . snd) todos
    in
        view [ style [ backgroundColor "#fff"
                     , marginHorizontal 30
                     , shadowOffset $ ContentSize 0 2
                     , shadowRadius 4
                     , shadowColor $ Rgba 0 0 0 0.2
                     , shadowOpacity 1
                     ]] $ do
            view [ style [ flexDirection Row ]] $ do
                touchableOpacity [ onPress $ dispatchTodo ToggleAllComplete ] $
                    view [ style [ alignItems Center______
                                 , justifyContent Center_____
                                 , flexDirection Column
                                 , width 50
                                 , height 65
                                 ]] $
                        text [ style [ transform [RotateZ (Deg 90.0)]
                                     , color $ if allCompleted then "#4d4d4d" else "#d9d9d9"
                                     , fontSize 20
                                     , fontFamily "HelveticaNeue"
                                     ]]
                            ">"
                todoTextInput [ fontStyle Italic
                              , fontSize 16
                              ] TextInputArgs { tiaPlaceholder = "What needs to be done?"
                                              , tiaSaveAction = SACreate
                                              , tiaOnCancel = []
                                              , tiaValue = Nothing
                                              }

            view [ style [ borderTopWidth 1
                         , borderTopColor "#e6e6e6"
                         ]] $
                forM_ (doFilter filt todos) todoItem

            mainSectionFooter todoState

todoItem = mkView "todo item" $ \(todoIdx, todo) ->
    let isComplete = todoComplete todo
    in
        view [ style [ flexDirection Row ] ] $ do
            unless (todoIsEditing todo) $ do
                touchableWithoutFeedback [ onPress $ dispatchTodo $ TodoSetComplete todoIdx (not isComplete) ] $
                    -- I guess IOS does not support rendering inline SVG, so let's use border and unicode instead of a check-mark image.
                    view [ style [ width 30
                                 , height 30
                                 , alignSelf Center____
                                 , marginLeft 8
                                 , paddingTop 4
                                 , borderWidth 1
                                 , borderRadius 30
                                 , borderColor "#bddad5"
                                 , alignItems Center______
                                 ]] $
                        text [ style [ fontSize 20, color "#5dc2af", fontFamily "HelveticaNeue" ]] $
                            if isComplete then "\x2713" else ""
                touchableOpacity [ onLongPress $ dispatchTodo $ TodoEdit todoIdx ] $
                    view [ style [ padding 15
                                 , flex 1
                                 ]] $
                        text [ style [ fontSize 22
                                     , marginVertical 3
                                     , fontWeight W300
                                     , color $ Color $ if isComplete then "#d9d9d9" else "#4d4d4d"
                                     , fontFamily "HelveticaNeue"
                                     , textDecorationLine $ if isComplete then LineThrough else None__________
                                     ]] $
                            elemString $ todoText todo

            when (todoIsEditing todo) $ do
                touchableWithoutFeedback [ onPress $ dispatchTodo $ TodoDelete todoIdx ] $
                    view [ style [ width 30
                                 , height 30
                                 , alignSelf Center____
                                 , marginLeft 8
                                 , paddingTop 4
                                 , alignItems Center______
                                 ]] $
                        text [ style [ fontSize 20
                                     , fontFamily "HelveticaNeue"
                                     , color "#cc9a9a"
                                     ]]
                            "x"

                view [ style [ flex 1
                             , marginLeft 15
                             , marginTop 1
                             ]] $
                    todoTextInput [] TextInputArgs { tiaPlaceholder = ""
                                                   , tiaSaveAction = SAUpdate todoIdx
                                                   , tiaOnCancel = [CancelUpdateWithDelay todoIdx]
                                                   , tiaValue = Just $ todoText todo
                                                   }

footerStyles = [ color "#777"
               , fontFamily "HelveticaNeue"
               , fontWeight W300
               ]

filterStyle = [ paddingHorizontal 7
              , marginHorizontal 3
              ]
activeFilterStyle = [ borderWidth 1
                    , borderColor $ Rgba 175 47 47 0.2
                    , borderRadius 3
                    ]

mainSectionFooter :: TodoState -> ReactElementM eventHandler ()
mainSectionFooter = mkView "msfooter" $ \(TodoState todos filtering) ->
    let completed = length (filter (todoComplete . snd) todos)
        itemsLeft = length todos - completed
        styling f = style $ filterStyle ++ (if f == filtering then activeFilterStyle else [])
     in
        view [ style [ flexDirection Row
                     , borderTopWidth 1
                     , borderTopColor "#e6e6e6"
                     , paddingVertical 10
                     , paddingHorizontal 15
                     , justifyContent SpaceBetween_
                     ]] $ do
            view [ style [ flexDirection Row ] ] $ do
                text [ style  $ fontWeight Bold : footerStyles ] $
                    elemShow itemsLeft
                text [ style  footerStyles ] $
                    if itemsLeft == 1 then " item left" else " items left"

            view [ style [ flexDirection Row
                         , justifyContent Center_____
                         , flexWrap Wrap
                         , flex 1
                         ]] $ do
                touchableOpacity [ onPress $ dispatchTodo (SetFilter AcceptAll) ] $
                    view [styling AcceptAll] $
                        text [ style  [fontFamily "HelveticaNeue"] ] "All"
                touchableOpacity [ onPress $ dispatchTodo (SetFilter AcceptActive) ] $
                    view [styling AcceptActive] $
                        text [ style  [fontFamily "HelveticaNeue"] ] "Active"
                touchableOpacity [ onPress $ dispatchTodo (SetFilter AcceptCompleted) ] $
                    view [styling AcceptCompleted] $
                        text [ style  [fontFamily "HelveticaNeue"] ] "Complete"

            touchableOpacity [ onPress $ dispatchTodo ClearCompletedTodos ] $
                view [ style (if completed == 0 then [opacity 0] else []) ] $
                    text [ style (flexWrap Wrap : footerStyles) ]
                        "Clear"
