{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Locacao where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist

formLocacao :: Form Locacao
formLocacao = renderDivs $ Locacao <$>
             areq (selectField funcs) "Funcionario" Nothing <*>
             areq (selectField clis) "Cliente" Nothing <*>
             areq (selectField cars) "Carro" Nothing <*>
             areq textField "Data" Nothing <*>
             areq intField  "Dia de locação" Nothing 

funcs = do
       entidades <- runDB $ selectList [] [Asc FuncionarioNm_funcionario] 
       optionsPairs $ fmap (\ent -> (funcionarioNm_funcionario $ entityVal ent, entityKey ent)) entidades

clis = do
       entidades <- runDB $ selectList [] [Asc ClienteNm_cliente] 
       optionsPairs $ fmap (\ent -> (clienteNm_cliente $ entityVal ent, entityKey ent)) entidades

cars = do
       entidades <- runDB $ selectList [] [Asc CarroModelo_carro] 
       optionsPairs $ fmap (\ent -> (carroModelo_carro $ entityVal ent, entityKey ent)) entidades

getCadastroLocacaoR :: Handler Html
getCadastroLocacaoR = do
           (widget, enctype) <- generateFormPost formLocacao
           defaultLayout [whamlet|
             <form method=post action=@{CadastroLocacaoR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
           |]

postCadastroLocacaoR :: Handler Html
postCadastroLocacaoR = do
            ((result, _), _) <- runFormPost formLocacao
            case result of
                FormSuccess locacao -> do
                    pid <- runDB $ insert locacao
                    defaultLayout [whamlet|
                        Locação cadastrada com sucesso #{fromSqlKey pid}!
                    |]
                _ -> redirect HomeR