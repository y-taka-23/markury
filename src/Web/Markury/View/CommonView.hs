{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View.CommonView where

import Data.Time ( UTCTime )
import Data.Time.Format ( FormatTime, formatTime, defaultTimeLocale )
import Prelude hiding ( head, div, span )
import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes hiding ( span, title )

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
        body $ do
            div ! class_ "mdl-layout mdl-js-layout" $ do
                header ! class_ "mdl-layout__header" $ do
                    div ! class_ "mdl-layout-icon" $ ""
                    div ! class_ "mdl-layout__header-row" $ do
                        span ! class_ "mdl-layout__title" $ "Markury"
                        div ! class_ "mdl-layout-spacer" $ ""
                div ! class_ "mdl-layout__drawer" $ do
                    span ! class_ "mdl-layout__title" $ "Markury"
                    nav ! class_ "mdl-navigation" $ do
                        a ! class_ "mdl-navigation__link" ! href "#" $ "Bookmarks"
                        a ! class_ "mdl-navigation__link" ! href "#" $ "Users"
                        a ! class_ "mdl-navigation__link" ! href "#" $ "Tags"
                main ! class_ "mdl-layout__content" $ do
                    mainContent
            script ! src "https://storage.googleapis.com/code.getmdl.io/1.0.6/material.min.js" $ ""

showTime :: FormatTime t => t -> String
showTime = formatTime defaultTimeLocale "%D, %R"
