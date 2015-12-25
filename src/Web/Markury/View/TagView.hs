{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View.TagView where

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

tagListView :: [Tag] -> Html
tagListView tags = mkSite $
    table ! class_ "mdl-data-table mdl-js-data-table" $ do
        thead $ do
            tr $ do
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Title"
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Create"
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Modified"
                th ! class_ "mdl-data-table__cell--non-numeric" $ "Actions"
        tbody $ forM_ tags $ \tag -> do
            tr $ do
                td ! class_ "mdl-data-table__cell--non-numeric" $ toHtml $ tagTitle tag
                td ! class_ "mdl-data-table__cell--non-numeric" $ toHtml $ showTime $ tagCreated tag
                td ! class_ "mdl-data-table__cell--non-numeric" $ toHtml $ showTime $ tagModified tag
                td ! class_ "mdl-data-table__cell--non-numeric" $ ""

tagView :: Show i => i -> Tag -> [Bookmark] -> Html
tagView id tag bookmarks = mkSite $
    div $ do
        h3 $ toHtml $ tagTitle tag
        table $ do
            tr $ do
                th "Title"
                td $ toHtml $ tagTitle tag
            tr $ do
                th "Id"
                td $ toHtml $ show id
            tr $ do
                th "Created"
                td $ toHtml $ showTime $ tagCreated tag
            tr $ do
                th "Modified"
                td $ toHtml $ showTime $ tagModified tag
        div $ do
            h4 "Related Bookmarks"
            table $ do
                thead $ do
                    th "Title"
                    th "Created"
                    th "Modified"
                tbody $ forM_ bookmarks $ \bookmark -> do
                    tr $ do
                        td $ toHtml $ bookmarkTitle bookmark
                        td $ toHtml $ showTime $ bookmarkCreated bookmark
                        td $ toHtml $ showTime $ bookmarkModified bookmark

tagAddForm :: Monad m => Form Html m TagInput
tagAddForm = TagInput
    <$> "title" .: text Nothing

tagAddView :: View Html -> Text -> Html
tagAddView view path = mkSite $
    div $ do
        form view path $ do
            label "title" view "Title: "
            inputText "title" view
            inputSubmit "Add"
