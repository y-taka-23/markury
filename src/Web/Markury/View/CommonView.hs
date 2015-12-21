{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View.CommonView where

import Data.Time ( UTCTime )
import Data.Time.Format ( FormatTime, formatTime, defaultTimeLocale )
import Prelude hiding ( head )
import Text.Blaze.XHtml5 ( (!), Html, docType, html, head, meta, title, link, body )
import Text.Blaze.XHtml5.Attributes ( lang, charset, httpEquiv, content, name, rel, href )

mkSite :: Html -> Html
mkSite mainContent = do
    docType
    html ! lang "ja" $ do
        head $ do
            meta ! charset "UTF-8"
            meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
            meta ! name "description" ! content "A simple bookmark application, written in Haskell."
            meta ! name "viewport" ! content "width=device-width, initial-scale=1"
            title "Markury"
            link ! rel "stylesheet" ! href "https://storage.googleapis.com/code.getmdl.io/1.0.6/material.indigo-pink.min.css"
            link ! rel "stylesheet" ! href "https://fonts.googleapis.com/icon?family=Material+Icons"

showTime :: FormatTime t => t -> String
showTime = formatTime defaultTimeLocale "%D, %R"
