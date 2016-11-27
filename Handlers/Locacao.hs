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
                    redirect ListLocacaoR
                _ -> redirect HomeR
                
getListLocacaoR :: Handler Html
getListLocacaoR = do
            locacoes <- runDB $ selectList [] [Asc LocacaoDt_locacao]
            defaultLayout $ do
                [whamlet|
                    <head>
                        <meta charset="utf-8">
                        <meta name="viewport" content="width=device-width, initial-scale=1">
                        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
                        <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
                        <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
                        <title>Consulta de locacao</title>

                    <body>
                        <div class="container">
                            <div class="jumbotron">
                                <h3 style="text-align:center; align:center;">LOCACOES

                        <table class="table table-striped">
                            <thead>
                                <tr>
                                    <th>ID
                                    <th>Data da locacao
                                    <th>Duracao da locacao
                                    <th>Deseja Voltar?

                         $forall Entity pid locacao <- locacoes
                             <tbody>
                                 <tr>
                                     <td><a href=@{LocacaoR pid}> #{fromSqlKey pid}
                                     <td>#{locacaoDt_locacao locacao}
                                     <td>#{locacaoQtd_dias_locacao locacao}
                                     <td><a href=@{HomeR}><button type="button" class="btn-warning">Voltar

                |]
                                
getLocacaoR :: LocacaoId -> Handler Html
getLocacaoR pid = do
             locacao <- runDB $ get404 pid 
             funcionario <- runDB $ get404 (locacaoId_func locacao)
             cliente <- runDB $ get404 (locacaoId_cliente locacao)
             carro <- runDB $ get404 (locacaoId_carro locacao)
             defaultLayout [whamlet| 
                <head>
                  <title>LOCACOES</title>
                  <meta charset="utf-8">
                  <meta name="viewport" content="width=device-width, initial-scale=1">
                  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
                  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
                  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
                <body>
                    <div class="container">
                        <div class="jumbotron">
                            <h3 style="text-align:center; align:center;"><center>Voce esta consultando a locacao: <b>#{fromSqlKey pid}
                    <div class="container" style="background-color:#ad4f4 text-align:center;" >
                      <table class="table table-bordered">
                        <thead>
                          <tr>
                            <th>ID
                            <th>Funcionario
                            <th>Cliente
                            <th>Carro
                            <th>Data da locacao
                            <th>Duracao da locacao
                            <th>Deseja Voltar?
                          <tr>
                            <td><a href=@{LocacaoR pid}>#{fromSqlKey pid}
                            <td>#{funcionarioNm_funcionario funcionario}
                            <td>#{clienteNm_cliente cliente}
                            <td>#{carroModelo_carro carro}
                            <td>#{locacaoDt_locacao locacao}
                            <td>#{locacaoQtd_dias_locacao locacao}
                            <td><a href=@{ListLocacaoR}><button type="button" class="btn-warning">Voltar
             |]