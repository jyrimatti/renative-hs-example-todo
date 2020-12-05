{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TodoViews.MainSection where

import Components
import Control.Monad                                     (forM_, unless, when)

import Dispatcher

import Prelude                                           (Maybe (..),
                                                                    all, filter,
                                                                    id, length,
                                                                    not, snd,
                                                                    ($), (++),
                                                                    (-), (.),
                                                                    (==))
import React.Flux                                        (elemShow, elemString)
import React.Flux.Rn.Components.Text as T
import React.Flux.Rn.Components.TouchableOpacity as TO
import React.Flux.Rn.Components.TouchableWithoutFeedback as TWF
import React.Flux.Rn.Components.View as V
import React.Flux.Rn.Types.AlignSelf as AS
import React.Flux.Rn.Types.FontStyle as FS
import React.Flux.Rn.Types.JustifyContent as JC
import React.Flux.Rn.Views
import Store

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
                touchableOpacity [ TO.onPress $ dispatchTodo ToggleAllComplete ] $
                    view [ style [ alignItems V.Center
                                 , justifyContent JC.Center
                                 , flexDirection Column
                                 , V.width 50
                                 , V.height 65
                                 ]] $
                        text [ style [ transform [RotateZ (Deg 90.0)]
                                     , color $ if allCompleted then "#4d4d4d" else "#d9d9d9"
                                     , fontSize 20
                                     , fontFamily "HelveticaNeue"
                                     ]]
                            ">"
                todoTextInput [ fontStyle FS.Italic
                              , fontSize 16
                              ] TextInputArgs { tiaPlaceholder = "What needs to be done?"
                                              , tiaSaveAction = SACreate
                                              , tiaOnCancel = []
                                              , tiaValue = Nothing
                                              }

            view [ style [ V.borderTopWidth 1
                         , borderTopColor "#e6e6e6"
                         ]] $
                forM_ (doFilter filt todos) todoItem

            mainSectionFooter todoState

todoItem = mkView "todo item" $ \(todoIdx, todo) ->
    let isComplete = todoComplete todo
    in
        view [ style [ flexDirection Row ] ] $ do
            unless (todoIsEditing todo) $ do
                touchableWithoutFeedback [ TWF.onPress $ dispatchTodo $ TodoSetComplete todoIdx (not isComplete) ] $
                    -- I guess IOS does not support rendering inline SVG, so let's use border and unicode instead of a check-mark image.
                    view [ style [ V.width 30
                                 , V.height 30
                                 , alignSelf AS.Center
                                 , marginLeft 8
                                 , paddingTop 4
                                 , V.borderWidth 1
                                 , borderRadius 30
                                 , borderColor "#bddad5"
                                 , alignItems V.Center
                                 ]] $
                        text [ style [ fontSize 20, color "#5dc2af", fontFamily "HelveticaNeue" ]] $
                            if isComplete then "\x2713" else ""
                touchableOpacity [ TO.onLongPress $ dispatchTodo $ TodoEdit todoIdx ] $
                    view [ style [ padding 15
                                 , flex 1
                                 ]] $
                        text [ style [ fontSize 22
                                     , marginVertical 3
                                     , fontWeight W300
                                     , color $ Color $ if isComplete then "#d9d9d9" else "#4d4d4d"
                                     , fontFamily "HelveticaNeue"
                                     , textDecorationLine $ if isComplete then LineThrough else T.None
                                     ]] $
                            elemString $ todoText todo

            when (todoIsEditing todo) $ do
                touchableWithoutFeedback [ TWF.onPress $ dispatchTodo $ TodoDelete todoIdx ] $
                    view [ style [ V.width 30
                                 , V.height 30
                                 , alignSelf AS.Center
                                 , marginLeft 8
                                 , paddingTop 4
                                 , alignItems V.Center
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
activeFilterStyle = [ V.borderWidth 1
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
                     , V.borderTopWidth 1
                     , borderTopColor "#e6e6e6"
                     , paddingVertical 10
                     , paddingHorizontal 15
                     , justifyContent SpaceBetween
                     ]] $ do
            view [ style [ flexDirection Row ] ] $ do
                text [ style  $ fontWeight Bold : footerStyles ] $
                    elemShow itemsLeft
                text [ style  footerStyles ] $
                    if itemsLeft == 1 then " item left" else " items left"

            view [ style [ flexDirection Row
                         , justifyContent JC.Center
                         , flexWrap Wrap
                         , flex 1
                         ]] $ do
                touchableOpacity [ TO.onPress $ dispatchTodo (SetFilter AcceptAll) ] $
                    view [styling AcceptAll] $
                        text [ style  [fontFamily "HelveticaNeue"] ] "All"
                touchableOpacity [ TO.onPress $ dispatchTodo (SetFilter AcceptActive) ] $
                    view [styling AcceptActive] $
                        text [ style  [fontFamily "HelveticaNeue"] ] "Active"
                touchableOpacity [ TO.onPress $ dispatchTodo (SetFilter AcceptCompleted) ] $
                    view [styling AcceptCompleted] $
                        text [ style  [fontFamily "HelveticaNeue"] ] "Complete"

            touchableOpacity [ TO.onPress $ dispatchTodo ClearCompletedTodos ] $
                view [ style (if completed == 0 then [opacity 0] else []) ] $
                    text [ style (flexWrap Wrap : footerStyles) ]
                        "Clear"
