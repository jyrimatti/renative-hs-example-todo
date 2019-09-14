{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TodoViews.Header where

import           Prelude                       (($))
import           React.Flux.Rn.Views

import           React.Flux.Rn.Components.Text
import           React.Flux.Rn.Components.View

todoHeader = mkView "header" $ \() ->
    view [ style [flex 1, justifyContent Center_____, alignItems Center______] ] $ do
        text [ style [ fontSize 100
                     , fontFamily "HelveticaNeue"
                     , fontWeight W100
                     , color $ Rgba 175 47 47 0.15
                    ]]
            "todos!"
