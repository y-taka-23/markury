{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View.BookmarkView where

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

bookmarkListView :: [Bookmark] -> Html
bookmarkListView bookmarks = mkSite $
    table ! class_ "mdl-data-table mdl-js-data-table" $ do
        thead $ do
            tr $ do
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Title"
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Create"
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Modified"
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Actions"
        tbody $ forM_ bookmarks $ \bookmark -> do
            tr $ do
                td ! class_ "mdl-data-table__cell--non-numeric" $ toHtml $ bookmarkTitle bookmark
                td ! class_ "mdl-data-table__cell--non-numeric" $ toHtml $ showTime $ bookmarkCreated bookmark
                td ! class_ "mdl-data-table__cell--non-numeric" $ toHtml $ showTime $ bookmarkModified bookmark
                td ! class_ "mdl-data-table__cell--non-numeric" $ ""

bookmarkView :: Show i => i -> Bookmark -> [Tag] -> Html
bookmarkView id bookmark tags = mkSite $
    div $ do
        h3 $ toHtml $ bookmarkTitle bookmark
        table $ do
            tr $ do
                th "Title"
                td $ toHtml $ bookmarkTitle bookmark
            tr $ do
                th "Id"
                td $ toHtml $ show id
            tr $ do
                th "Created"
                td $ toHtml $ showTime $ bookmarkCreated bookmark
            tr $ do
                th "Modified"
                td $ toHtml $ showTime $ bookmarkModified bookmark
        div $ do
            h4 "Description"
            p $ toHtml $ bookmarkDescription bookmark
        div $ do
            h4 "Url"
            p $ toHtml $ bookmarkUrl bookmark
        div $ do
            h4 "Related Tags"
            table $ do
                thead $ do
                    th "Title"
                    th "Created"
                    th "Modified"
                tbody $ forM_ tags $ \tag -> do
                    tr $ do
                        td $ toHtml $ tagTitle tag
                        td $ toHtml $ showTime $ tagCreated tag
                        td $ toHtml $ showTime $ tagModified tag

bookmarkAddForm :: Monad m => Form Html m BookmarkInput
bookmarkAddForm = BookmarkInput
    <$> "title" .: text Nothing
    <*> "description" .: text Nothing
    <*> "url" .: text Nothing
    <*> "tags" .: text Nothing

bookmarkAddView :: View Html -> Text -> Html
bookmarkAddView view path = mkSite $
    div $ do
        form view path $ do
            label "title" view "Title: "
            inputText "title" view
            label "description" view "Description: "
            inputText "description" view
            label "url" view "URL: "
            inputText "url" view
            label "tags" view "Tags: "
            inputText "tags" view
            inputSubmit "Add"
