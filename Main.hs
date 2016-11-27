{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Application
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

connStr :: ConnectionString
connStr = "dbname=dfe27dgrck3kt0 host=ec2-54-235-65-221.compute-1.amazonaws.com user=ibneirltsjezhx password=hYNq-AgbiH6E6BW9fJgnT1BjcB port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       warp 8080 (Sitio pool)
