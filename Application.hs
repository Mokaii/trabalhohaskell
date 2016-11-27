{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}

module Application where

import Foundation
import Yesod
import Handlers.Carro
import Handlers.Cliente
import Handlers.Funcionario
import Handlers.Locacao
import Handlers.Login

mkYesodDispatch "Sitio" resourcesSitio

getHomeR :: Handler Html
getHomeR  = defaultLayout $ do
    sess <- lookupSession "_ID"
    toWidget [lucius|
        h1{
            color:red;
        }
        ul li{
           display:inline; 
        }
        
        #menususpenso ul{
           text-align:center;
           padding:10px;
           margin:0px;
           background-color:black;
           list-style:none;
           color:white;
           width:730px;
           height:24px;
        }
        
        .divider {
          height: 1px;
          width:100%;
          display:block; /* for use on default inline elements like span */
          margin: 9px 0;
          overflow: hidden;
          background-color: #e5e5e5;
        }
    |]
    [whamlet|
        <head>
            <title>Gerenciamento de Aluguel Simples</title>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
            <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
            <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
            <div class="container">
                    <img src="http://4.bp.blogspot.com/-BehfOLqhAXc/VL-6xhFf3DI/AAAAAAAAAhY/VUiOOdDUUbs/s1600/Loca%C3%A7%C3%A3o%2Bde%2Bcarro.jpg" style="max-width:1138px;">
       
        <body>
            <nav class="navbar navbar-inverse" data-spy="affix" data-offset-top="300">
                <div class="container-fluid">
                    <div class="navbar-header">
                        <ul class="nav navbar-nav">
                            <li class="active"><a href=@{LogoutR}>Logout
                            <li><a href=@{CadastroCarroR}>Cadastrar Carro
                            <li><a href=@{CadastroFuncionarioR}>Cadastrar Funcionario
                            <li><a href=@{CadastroClienteR}>Cadastrar Cliente
                            <li><a href=@{CadastroLocacaoR}>Registrar Aluguel
        
       <div class="panel-content">
           <table class="table table-striped">
                   <thead>
                       <tr>
                           <th><a href=@{ListClienteR}><button type="button" class="btn-mini">Consultar Clientes
                           <th><a href=@{ListFuncionarioR}><button type="button" class="btn-default">Consultar Funcionarios
                           <th><a href=@{ListCarroR}><button type="button" class="btn-info">Consultar Carros
                           <th><a href=@{ListLocacaoR}><button type="button" class="btn-warning">Consultar Alugueis
                
                
    |]