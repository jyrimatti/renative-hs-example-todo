{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TodoViews.Header where

import           Prelude                       (($))
import           React.Flux.Rn.Components.Text
import           React.Flux.Rn.Components.View as V
import qualified React.Flux.Rn.Types.JustifyContent as JC
import           React.Flux.Rn.Views

todoHeader = mkView "header" $ \() ->
    view [ style [flex 1, justifyContent JC.Center, alignItems V.Center] ] $
        text [ style [ fontSize 100
                     , fontFamily "HelveticaNeue"
                     , fontWeight W100
                     , color $ Rgba 175 47 47 0.15
                    ]]
            "todos!"
