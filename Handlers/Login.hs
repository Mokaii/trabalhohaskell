{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Login where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) <$>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Password" Nothing

getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout [whamlet|
             <form method=post action=@{LoginR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Login">
           |]
           
postLoginR :: Handler Html
postLoginR = do
            ((result, _), _) <- runFormPost formLogin
            case result of
                FormSuccess (email, password) -> do
                   cara <- runDB $ selectFirst [FuncionarioEmail_funcionario ==. email,
                                                FuncionarioSenha_funcionario ==. password] []
                   case cara of
                       Just (Entity pid funcionario) -> do
                           setSession "_ID" (pack $ show $ fromSqlKey pid)
                           redirect (HomeR)
                       Nothing -> redirect LoginR
                _ -> redirect CadastroFuncionarioR

postLogoutR:: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect LoginR