module Web.Markury.Model.Input where

import Data.Text ( Text )

data BookmarkInput = BookmarkInput
    { bookmarkInputTitle :: Text
    , bookmarkInputDescription :: Text
    , bookmarkInputUrl :: Text
    , bookmarkInputTags :: Text
    }

data UserInput = UserInput
    { userInputEmail :: Text
    , userInputPassword :: Text
    }

data TagInput = TagInput
    { tagInputTitle :: Text
    }
