{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View.UserView where

import Web.Markury.Model.DB
import Web.Markury.Model.Input
import Web.Markury.View.CommonView

import Prelude hiding ( head, div )
import Text.Blaze.XHtml5 hiding ( Tag, text, label, form )
import Text.Blaze.XHtml5.Attributes ( href )
import Control.Monad
import Data.Text hiding ( head )
import Text.Digestive
import Text.Digestive.Blaze.Html5

userListView :: [User] -> Html
userListView users = mkSite $
    div $ do
        h3 "Users"
        table $ do
            thead $ do
                th "Email"
                th "Password"
                th "Created"
                th "Modified"
                th "Actions"
            tbody $ forM_ users $ \user -> do
                tr $ do
                    td $ toHtml $ userEmail user
                    td $ toHtml $ userPassword user
                    td $ toHtml $ showTime $ userCreated user
                    td $ toHtml $ showTime $ userModified user

userView :: Show i => i -> User -> Html
userView id user = mkSite $
    div $ do
        h3 $ toHtml $ show id
        table $ do
            tr $ do
                th "Email"
                td $ toHtml $ userEmail user
            tr $ do
                th "Password"
                td $ toHtml $ userPassword user
            tr $ do
                th "Id"
                td $ toHtml $ show id
            tr $ do
                th "Created"
                td $ toHtml $ showTime $ userCreated user
            tr $ do
                th "Modified"
                td $ toHtml $ showTime $ userModified user

userAddForm :: Monad m => Form Html m UserInput
userAddForm = UserInput
    <$> "email" .: text Nothing
    <*> "password" .: text Nothing

userAddView :: View Html -> Text -> Html
userAddView view path = mkSite $
    div $ do
        form view path $ do
            label "email" view "Email: "
            inputText "email" view
            label "password" view "Password: "
            inputText "password" view
            inputSubmit "Add"
