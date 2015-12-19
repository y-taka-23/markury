{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View.CommonView where

import Data.Time ( UTCTime )
import Data.Time.Format ( FormatTime, formatTime, defaultTimeLocale )

showTime :: FormatTime t => t -> String
showTime = formatTime defaultTimeLocale "%D, %R"
