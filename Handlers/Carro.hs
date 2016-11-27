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
            
getCadastroCarroR :: Handler Html
getCadastroCarroR = do
           (widget, enctype) <- generateFormPost formCarro
           defaultLayout [whamlet|
             <form method=post action=@{CadastroCarroR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
           |]
           
getCarroR :: CarroId -> Handler Html
getCarroR pid = do
             carro <- runDB $ get404 pid 
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
                            <h3 style="text-align:center; align:center;"><center>Voce esta consultando o carro: <b>#{carroModelo_carro carro}
                    <div class="container" style="background-color:#ad4f4 text-align:center;" >
                      <table class="table table-bordered">
                        <thead>
                          <tr>
                            <th>ID
                            <th>Modelo
                            <th>Ano
                            <th>Placa
                            <th>Valor Diária
                            <th>Deseja Excluir?
                            <th>Deseja Voltar?
                          <tr>
                            <td><a href=@{CarroR pid}>#{fromSqlKey pid}
                            <td>#{carroModelo_carro carro}
                            <td>#{carroAno_carro carro}
                            <td>#{carroPlaca_carro carro}
                            <td>#{carroValor_diaria carro}
                            <td><form method=post action=@{CarroR pid}><input type="submit" value="Deletar">
                            <td><a href=@{ListCarroR}><button type="button" class="btn-warning">Voltar
             |]
             
getListCarroR :: Handler Html
getListCarroR = do
            carros <- runDB $ selectList [] [Asc CarroModelo_carro]
            defaultLayout $ do
                [whamlet|
                    <head>
                        <meta charset="utf-8">
                        <meta name="viewport" content="width=device-width, initial-scale=1">
                        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
                        <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
                        <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
                        <title>Consulta de carros</title>

                    <body>
                        <div class="container">
                            <div class="jumbotron">
                                <h3 style="text-align:center; align:center;">CARROS CADASTRADOS

                        <table class="table table-striped">
                            <thead>
                                <tr>
                                    <th>ID
                                    <th>Modelo
                                    <th>Ano
                                    <th>Placa
                                    <th>Valor Diária
                                    <th>Deseja Voltar?

                         $forall Entity pid carro <- carros
                             <tbody>
                                 <tr>
                                     <td><a href=@{CarroR pid}>#{fromSqlKey pid}
                                     <td>#{carroModelo_carro carro}
                                     <td>#{carroAno_carro carro}
                                     <td>#{carroPlaca_carro carro}
                                     <td>#{carroValor_diaria carro}
                                     <td><a href=@{HomeR}><button type="button" class="btn-warning">Voltar

                |]

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
                
        
postCarroR :: CarroId -> Handler Html
postCarroR pid = do
     runDB $ delete pid
     redirect ListCarroR