{-# LANGUAGE OverloadedStrings #-}
module Web.Markury where

import Web.Markury.View

import Web.Spock.Safe
import Text.Blaze.Html.Renderer.Utf8 ( renderHtml )

runMarkury :: IO ()
runMarkury =
    runSpock 8080 $ spockT id $ do
        get "bookmarks" $
            lazyBytes $ renderHtml bookmarksView
