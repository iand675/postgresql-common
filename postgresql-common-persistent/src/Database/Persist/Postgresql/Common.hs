{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Postgresql.Common
    ( LSN(..)
    ) where

import Database.PostgreSQL.Types.LSN
import Database.Persist.Sql
import qualified Data.Text as T

instance PersistField LSN where
  toPersistValue = PersistDbSpecific . encodeLSN
  fromPersistValue (PersistDbSpecific t) = case decodeLSN t of
    Left err -> Left $ T.pack err
    Right ok -> Right ok
  fromPersistValue _ = Left "Could not decode LSN"

instance PersistFieldSql LSN where
  sqlType _ = SqlOther "pg_lsn"
