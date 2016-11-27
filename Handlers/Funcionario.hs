{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Funcionario where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist

formFuncionario :: Form Funcionario
formFuncionario = renderDivs $ Funcionario <$>
            areq textField "Nome" Nothing <*>
            areq textField "Senha" Nothing <*>
            areq textField "Cargo" Nothing <*>
            areq textField "CPF" Nothing <*>
            areq textField "Email" Nothing <*>
            areq textField "Data Nascimento" Nothing 


getCadastroFuncionarioR :: Handler Html
getCadastroFuncionarioR = do
           (widget, enctype) <- generateFormPost formFuncionario
           defaultLayout [whamlet|
             <form method=post action=@{CadastroFuncionarioR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
           |]