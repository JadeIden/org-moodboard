{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Components.Grid (renderGridGaps, renderGridGapless) where

import Text.Blaze.Html5 as H hiding (main, style)
import Text.Blaze.Html5.Attributes

_renderGrid :: String -> [Html] -> Html
_renderGrid dclass els = H.div ! class_ (stringValue dclass) $ do
  mconcat $ H.div ! class_ "column col-4" <$> els

renderGridGaps :: [Html] -> Html
renderGridGaps = _renderGrid "columns"

renderGridGapless :: [Html] -> Html
renderGridGapless = _renderGrid "columns col-gapless"
