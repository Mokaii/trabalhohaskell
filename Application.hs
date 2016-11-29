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
        
        img, button, input{
            margin:auto;
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
            <div class="container-fluid">
                        <img class="page-header center-block" src="https://i.sli.mg/8xsU0p.png" style="max-width:1138px;">
       
        <body>
            <nav class="navbar navbar-inverse">
                <div class="container-fluid">
                    <div class="navbar-header">
                        $maybe _ <- sess
                            <li>
                                <form action=@{LogoutR} method=post>
                                    <input class="btn btn-default" type="submit" value="Logout">
                    <ul class="nav navbar-nav">
                        <li>
                            <a href=@{CadastroCarroR}>Cadastrar Carro
                        <li>
                            <a href=@{CadastroFuncionarioR}>Cadastrar Funcionario
                        <li>
                            <a href=@{CadastroClienteR}>Cadastrar Cliente
                        <li>
                            <a href=@{CadastroLocacaoR}>Registrar Aluguel
        
       <div class="panel-content">
            <div class="col-md-3">
                <a href=@{ListClienteR}><button type="button" class="center-block btn-mini">Consultar Clientes
            <div class="col-md-3">
                <a href=@{ListFuncionarioR}><button type="button" class="center-block btn-mini">Consultar Funcionarios
            <div class="col-md-3">
                <a href=@{ListCarroR}><button type="button" class="center-block btn-mini">Consultar Carros
            <div class="col-md-3">
                <a href=@{ListLocacaoR}><button type="button" class="center-block btn-mini">Consultar Alugueis

    |]