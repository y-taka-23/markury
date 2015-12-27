{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View.LoginView where

import Web.Markury.Model.DB
import Web.Markury.Model.Input
import Web.Markury.View.CommonView

import Control.Monad ( forM_ )
import Data.Text hiding ( head )
import Prelude hiding ( head, div )
import Text.Blaze.XHtml5 hiding ( Tag, form, label, text )
import Text.Blaze.XHtml5.Attributes hiding ( form, label )
import Text.Digestive
import Text.Digestive.Blaze.Html5

loginForm :: Monad m => Form Html m LoginInput
loginForm = LoginInput
    <$> "email" .: text Nothing
    <*> "password" .: text Nothing

loginView :: View Html -> Text -> Html
loginView view path = mkSite $
    div $ do
        form view path $ do
            label "email" view "Email: "
            inputText "email" view
            label "password" view "Password: "
            inputText "password" view
            inputSubmit "Login"
