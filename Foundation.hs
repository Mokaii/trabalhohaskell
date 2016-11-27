{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where

import Yesod
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool)

data Sitio = Sitio {connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Carro
    modelo_carro  Text
    ano_carro Text
    placa_carro Text
    valor_diaria Double
    UniquePlaca placa_carro
    
Cliente
    nm_cliente Text
    cpf_cliente Text
    dt_nascimento Text
    email_cliente Text
    UniqueCpfCliente cpf_cliente
    UniqueEmailCliente email_cliente

Funcionario
    nm_funcionario Text
    senha_funcionario Text
    cargo_funcionario Text
    cpf_funcionario Text
    email_funcionario Text
    dt_nascimento Text
    UniqueCpfFuncionario cpf_funcionario
    UniqueEmailFuncionario email_funcionario
    
Locacao
    id_func FuncionarioId
    id_cliente ClienteId
    id_carro CarroId
    dt_locacao Text
    qtd_dias_locacao Int
|]

mkYesodData "Sitio" $(parseRoutesFile "routes")

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool