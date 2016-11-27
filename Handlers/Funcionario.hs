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
           
getFuncionarioR :: FuncionarioId -> Handler Html
getFuncionarioR pid = do
             funcionario <- runDB $ get404 pid 
             defaultLayout [whamlet| 
                <head>
                  <title>Bootstrap Example</title>
                  <meta charset="utf-8">
                  <meta name="viewport" content="width=device-width, initial-scale=1">
                  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
                  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
                  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
                <body>
                    <div class="container">
                        <div class="jumbotron">
                            <h3 style="text-align:center; align:center;"><center>Voce esta consultando o funcionario: <b>#{funcionarioNm_funcionario funcionario}
                    <div class="container" style="background-color:#ad4f4 text-align:center;" >
                      <table class="table table-bordered">
                        <thead>
                          <tr>
                            <th>ID
                            <th>Nome
                            <th>Cargo
                            <th>CPF
                            <th>Email
                            <th>Data Nascimento
                            <th>Deseja Excluir?
                            <th>Deseja Voltar?
                          <tr>
                            <td><a href=@{FuncionarioR pid}>#{fromSqlKey pid}
                            <td>#{funcionarioNm_funcionario funcionario}
                            <td>#{funcionarioCargo_funcionario funcionario}
                            <td>#{funcionarioCpf_funcionario funcionario}
                            <td>#{funcionarioEmail_funcionario funcionario}
                            <td>#{funcionarioDt_nascimento funcionario}
                            <td><form method=post action=@{FuncionarioR pid}><input type="submit" value="Deletar">
                            <td><a href=@{ListFuncionarioR}><button type="button" class="btn-warning">Voltar
             |]

getListFuncionarioR :: Handler Html
getListFuncionarioR = do
            funcionarios <- runDB $ selectList [] [Asc FuncionarioNm_funcionario]
            defaultLayout $ do
                [whamlet|
                    <head>
                        <meta charset="utf-8">
                        <meta name="viewport" content="width=device-width, initial-scale=1">
                        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
                        <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
                        <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
                        <title>Consulta de funcionarios</title>

                    <body>
                        <div class="container">
                            <div class="jumbotron">
                                <h3 style="text-align:center; align:center;">FUNCIONARIOS CADASTRADOS

                        <table class="table table-striped">
                            <thead>
                                <tr>
                                    <th>ID
                                    <th>Nome
                                    <th>Cargo
                                    <th>CPF
                                    <th>Email
                                    <th>Data Nascimento
                                    <th>Deseja Voltar?

                         $forall Entity pid funcionario <- funcionarios
                             <tbody>
                                 <tr>
                                     <td><a href=@{FuncionarioR pid}>#{fromSqlKey pid}
                                     <td>#{funcionarioNm_funcionario funcionario}
                                     <td>#{funcionarioCargo_funcionario funcionario}
                                     <td>#{funcionarioCpf_funcionario funcionario}
                                     <td>#{funcionarioEmail_funcionario funcionario}
                                     <td>#{funcionarioDt_nascimento funcionario}
                                     <td><a href=@{HomeR}><button type="button" class="btn-warning">Voltar

                |]


postCadastroFuncionarioR :: Handler Html
postCadastroFuncionarioR = do
            ((result, _), _) <- runFormPost formFuncionario
            case result of
                FormSuccess funcionario -> do
                    pid <- runDB $ insert funcionario
                    redirect ListFuncionarioR
                _ -> redirect HomeR


postFuncionarioR :: FuncionarioId -> Handler Html
postFuncionarioR pid = do
     runDB $ delete pid
     redirect ListFuncionarioR