{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View where

import Prelude hiding ( head, div )
import Text.Blaze.XHtml5

bookmarksView :: Html
bookmarksView = docTypeHtml $ do
    head $ do
        title "Markury - Simple Bookmarker"
    body $ do
        nav $ do
            ul $ do
                li $ h1 "Bookmarks"
            section $ do
                ul $ do
                    li "Documentation"
                    li "API"
        section $ do
            nav $ do
                ul $ do
                    li "Actions"
                    li "New Bookmark"
                    li "List Users"
                    li "New User"
                    li "List Tags"
                    li "New Tag"
            div $ do
                h3 "Bookmarks"
                table $ do
                    thead $ do
                        th "Id"
                        th "User"
                        th "Title"
                        th "Created"
                        th "Modified"
                        th "Actions"
                    tbody ""
                div $ do
                    ul $ do
                        li "< previous"
                        li "next >"
                    p "1 of 1"
        footer $ ""

usersView :: Html
usersView = docTypeHtml $ do
    head $ do
        title "Markury - Simple Bookmarker"
    body $ do
        nav $ do
            ul $ do
                li $ h1 "Users"
            section $ do
                ul $ do
                    li "Documentation"
                    li "API"
        section $ do
            nav $ do
                ul $ do
                    li "Actions"
                    li "New User"
                    li "List Bookmarks"
                    li "New Bookmark"
            div $ do
                h3 "Users"
                table $ do
                    thead $ do
                        th "Id"
                        th "Email"
                        th "Password"
                        th "Created"
                        th "Modified"
                        th "Actions"
                    tbody ""
                div $ do
                    ul $ do
                        li "< previous"
                        li "next >"
                    p "1 of 1"
        footer $ ""

tagsView :: Html
tagsView = docTypeHtml $ do
    head $ do
        title "Markury - Simple Bookmarker"
    body $ do
        nav $ do
            ul $ do
                li $ h1 "Tags"
            section $ do
                ul $ do
                    li "Documentation"
                    li "API"
        section $ do
            nav $ do
                ul $ do
                    li "Actions"
                    li "New Tag"
                    li "List Bookmarks"
                    li "New Bookmark"
            div $ do
                h3 "Tags"
                table $ do
                    thead $ do
                        th "Id"
                        th "Title"
                        th "Created"
                        th "Modified"
                        th "Actions"
                    tbody ""
                div $ do
                    ul $ do
                        li "< previous"
                        li "next >"
                    p "1 of 1"
        footer $ ""
