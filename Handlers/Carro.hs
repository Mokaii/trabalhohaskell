{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Carro where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist


formCarro :: Form Carro
formCarro = renderDivs $ Carro <$>
            areq textField "Modelo" Nothing <*>
            areq textField "Ano" Nothing <*>
            areq textField "Placa" Nothing <*>
            areq doubleField "Valor Diária" Nothing

postCadastroCarroR :: Handler Html
postCadastroCarroR = do
            ((result, _), _) <- runFormPost formCarro
            case result of
                FormSuccess carro -> do
                    pid <- runDB $ insert carro
                    defaultLayout [whamlet|
                        Carro cadastrado com sucesso #{fromSqlKey pid}!
                    |]
                    redirect HomeR
                _ -> redirect HomeR
                
getCadastroCarroR :: Handler Html
getCadastroCarroR = do
           (widget, enctype) <- generateFormPost formCarro
           defaultLayout [whamlet|
             <form method=post action=@{CadastroCarroR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
           |]
        
postCarroR :: CarroId -> Handler Html
postCarroR pid = do
     runDB $ delete pid
     redirect ListCarroR