{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TodoViews.Footer where

import           Dispatcher
import qualified Prelude                                   as P

import           Prelude                                   (($), (++))

import           React.Flux                                (elemString)
import qualified React.Flux.Rn.APIs                        as RnA
import           React.Flux.Rn.Components.ScrollView
import           React.Flux.Rn.Components.Text as T
import           React.Flux.Rn.Components.TouchableOpacity as TO
import           React.Flux.Rn.Components.View
import           React.Flux.Rn.Types.AlignSelf as AS

import           React.Flux.Rn.Views
import           Store

infoStyles = [ fontFamily "HelveticaNeue"
             , fontSize 10
             , color "#bfbfbf"
             , textShadowOffset $ ContentSize 0 1
             , textShadowColor $ Rgba 255 255 255 0.5
             , alignSelf AS.Center
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
                     , alignSelf AS.Center
                     ]] $
            credit "Credits: Jyri-Matti Lähteenmäki" "https://twitter.com/jyrimatti"
        text [ style infoStyles ]
            "standing on the shoulders of"
        view [ style [ flexDirection Row
                     , flexWrap Wrap
                     , alignSelf AS.Center
                     ]] $ do
            credit "GHCJS, "                "https://github.com/ghcjs/ghcjs"
            credit "React, "                "https://facebook.github.io/react/"
            credit "React-flux, "           "https://hackage.haskell.org/package/react-flux"
            credit "React-native, "         "https://facebook.github.io/react-native/"
            credit "Renative "              "https://renative.org"
  where credit txt link = touchableOpacity [ TO.onPress $ dispatchTodo $ OpenLink link ] $
                              text [style  infoStyles] txt
