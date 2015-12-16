{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury.Action.Util where

import Web.Markury.View
import Web.Markury.Model.DB
import Web.Markury.Model.Input

import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Logger ( NoLoggingT, runNoLoggingT )
import Control.Monad.Trans.Resource ( ResourceT, runResourceT )
import Database.Persist.Sql ( SqlBackend, SqlPersistT, runSqlConn )
import Text.Blaze.Html ( Html )
import Text.Blaze.Html.Renderer.Utf8 ( renderHtml )
import Web.Spock.Safe

runSql :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSql action = runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn

renderSite :: MonadIO m => Html -> ActionT m a
renderSite = lazyBytes . renderHtml . siteView
