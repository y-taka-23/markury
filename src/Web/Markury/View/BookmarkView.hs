{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View.BookmarkView where

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

bookmarkListView :: [Bookmark] -> Html
bookmarkListView bookmarks =
    div $ do
        h3 "Bookmarks"
        table $ do
            thead $ do
                th "Title"
                th "Created"
                th "Modified"
                th "Actions"
            tbody $ forM_ bookmarks $ \bookmark -> do
                tr $ do
                    td $ toHtml $ bookmarkTitle bookmark
                    td $ toHtml $ showTime $ bookmarkCreated bookmark
                    td $ toHtml $ showTime $ bookmarkModified bookmark

bookmarkView :: Show i => i -> Bookmark -> [Tag] -> Html
bookmarkView id bookmark tags =
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
bookmarkAddView view path =
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
