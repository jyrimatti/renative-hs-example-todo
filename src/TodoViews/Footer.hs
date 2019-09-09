{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TodoViews.Footer where

import           Prelude                                   (($), (++))
import qualified Prelude                                   as P

import           React.Flux.Rn.Views

import           React.Flux                                (elemString)
import qualified React.Flux.Rn.APIs                        as RnA
import           React.Flux.Rn.Components.ScrollView
import           React.Flux.Rn.Components.Text
import           React.Flux.Rn.Components.TouchableOpacity
import           React.Flux.Rn.Components.View

import           Dispatcher
import           Store

infoStyles = [ fontFamily "HelveticaNeue"
             , fontSize 10
             , color "#bfbfbf"
             , textShadowOffset $ ContentSize 0 1
             , textShadowColor $ Rgba 255 255 255 0.5
             , alignSelf Center____
             ]

todoFooter = mkView "todoFooter" $ \() ->
    view [ style [ marginTop 20
                 , marginBottom 10
                 , marginHorizontal 30
                 ]] $ do
        text [ style (marginBottom 10 : infoStyles) ]
            "Long-press to edit, double-click x to delete"
        text [ style infoStyles ] $
            elemString ("You are running on: " ++ P.show RnA.platform)
        view [ style [ flexWrap Wrap
                     , alignSelf Center____
                     ]] $
            credit "Credits: Jyri-Matti Lähteenmäki" "https://twitter.com/jyrimatti"
        text [ style infoStyles ]
            "standing on the shoulders of"
        view [ style [ flexDirection Row
                     , flexWrap Wrap
                     , alignSelf Center____
                     ]] $ do
            credit "GHCJS, "                "https://github.com/ghcjs/ghcjs"
            credit "React, "                "https://facebook.github.io/react/"
            credit "React-flux, "           "https://hackage.haskell.org/package/react-flux"
            credit "React-native, "         "https://facebook.github.io/react-native/"
            credit "React-native-desktop, " "https://www.npmjs.com/package/react-native-desktop"
            credit "React-native-web"       "https://github.com/necolas/react-native-web"
  where credit txt link = touchableOpacity [ onPress $ dispatchTodo $ OpenLink link ] $
                              text [style  infoStyles] txt
