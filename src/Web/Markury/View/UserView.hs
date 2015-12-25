{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View.UserView where

import Web.Markury.Model.DB
import Web.Markury.Model.Input
import Web.Markury.View.CommonView

import Control.Monad ( forM_ )
import Data.Text hiding ( head )
import Prelude hiding ( head, div )
import Text.Blaze.XHtml5 hiding ( form, label, text )
import Text.Blaze.XHtml5.Attributes hiding ( form, label )
import Text.Digestive
import Text.Digestive.Blaze.Html5

userListView :: [User] -> Html
userListView users = mkSite $
    table ! class_ "mdl-data-table mdl-js-data-table" $ do
        thead $ do
            tr $ do
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Email"
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Password"
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Created"
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Modified"
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Actions"
        tbody $ forM_ users $ \user -> do
            tr $ do
                td ! class_ "mdl-data-table__cell--non-numeric" $ toHtml $ userEmail user
                td ! class_ "mdl-data-table__cell--non-numeric" $ toHtml $ userPassword user
                td ! class_ "mdl-data-table__cell--non-numeric" $ toHtml $ showTime $ userCreated user
                td ! class_ "mdl-data-table__cell--non-numeric" $ toHtml $ showTime $ userModified user
                td ! class_ "mdl-data-table__cell--non-numeric" $ ""

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
