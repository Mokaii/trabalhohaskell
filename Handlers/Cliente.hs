{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Cliente where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist

formCliente :: Form Cliente
formCliente = renderDivs $ Cliente <$>
            areq textField "Nome" Nothing <*>
            areq textField "CPF" Nothing <*>
            areq textField "Data Nascimento" Nothing <*>
            areq textField "Email" Nothing

getCadastroClienteR :: Handler Html
getCadastroClienteR = do
           (widget, enctype) <- generateFormPost formCliente
           defaultLayout [whamlet|
             <form method=post action=@{CadastroClienteR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
           |]


getClienteR :: ClienteId -> Handler Html
getClienteR pid = do
             cliente <- runDB $ get404 pid 
             defaultLayout [whamlet| 
                <head>
                  <title>Epic</title>
                  <meta charset="utf-8">
                  <meta name="viewport" content="width=device-width, initial-scale=1">
                  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
                  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
                  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
                <body>
                    <div class="container">
                        <div class="jumbotron">
                            <h3 style="text-align:center; align:center;"><center>Voce esta consultando o cliente: <b>#{clienteNm_cliente cliente}
                    <div class="container" style="background-color:#ad4f4 text-align:center;" >
                      <table class="table table-bordered">
                        <thead>
                          <tr>
                            <th>ID
                            <th>Nome
                            <th>CPF
                            <th>Data de Nascimento
                            <th>Email
                            <th>Deseja Excluir?
                            <th>Deseja Voltar?
                          <tr>
                            <td><a href=@{ClienteR pid}>#{fromSqlKey pid}
                            <td>#{clienteNm_cliente cliente}
                            <td>#{clienteCpf_cliente cliente}
                            <td>#{clienteDt_nascimento cliente}
                            <td>#{clienteEmail_cliente cliente}
                            <td><form method=post action=@{ClienteR pid}><input type="submit" value="Deletar">
                            <td><a href=@{ListClienteR}><button type="button" class="btn-warning">Voltar
             |]

getListClienteR :: Handler Html
getListClienteR =  do
            clientes <- runDB $ selectList [] [Asc ClienteNm_cliente]
            defaultLayout $ do
                [whamlet|
                    <head>
                        <meta charset="utf-8">
                        <meta name="viewport" content="width=device-width, initial-scale=1">
                        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
                        <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
                        <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
                        <title>Consulta de clientes</title>

                    <body>
                        <div class="container">
                            <div class="jumbotron">
                                <h3 style="text-align:center; align:center;">CARROS CADASTRADOS

                        <table class="table table-striped">
                            <thead>
                                <tr>
                                    <th>ID
                                    <th>Nome
                                    <th>CPF
                                    <th>Data de nascimento
                                    <th>Email

                         $forall Entity pid cliente <- clientes
                             <tbody>
                                 <tr>
                                     <td><a href=@{ClienteR pid}>#{fromSqlKey pid}
                                     <td>#{clienteNm_cliente cliente}
                                     <td>#{clienteCpf_cliente cliente}
                                     <td>#{clienteDt_nascimento cliente}
                                     <td>#{clienteEmail_cliente cliente}
                    <a href=@{HomeR}><button type="button" class="btn-warning">Voltar
                |]

postCadastroClienteR :: Handler Html
postCadastroClienteR = do
            ((result, _), _) <- runFormPost formCliente
            case result of
                FormSuccess cliente -> do
                    pid <- runDB $ insert cliente
                    redirect ListClienteR 
                _ -> redirect HomeR

postClienteR :: ClienteId -> Handler Html
postClienteR pid = do
     runDB $ delete pid
     redirect ListClienteR