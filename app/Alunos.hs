{-# LANGUAGE OverloadedStrings #-}

-- | Módulo para transformar arquivos CSV do MinhaUFOP.
module Alunos
    ( transformaAlunos
    ) where

import Control.Applicative
import Prelude hiding (readFile, writeFile)
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.Vector as V
import Data.Csv
import qualified Data.Text.Lazy as T
import qualified Data.Char as C


{-
ANO SEMESTRE    COD DISCIPLINA  MATRICULA   NOME    CURSO   TURMA   EMAIL
2013    2   CEA439  11.2.8065   ADRIANO ROMERO FRANCISCO    SJM 11  drico.rfc@gmail.com
-}
-- | Modelo para CSV de elemVetorada de alunos
data AlunosEntrada = AlunosEntrada
    { ano :: !Int
    , semestre :: !Int
    , disciplina :: !T.Text
    , matricula :: !T.Text
    , nome :: !T.Text
    , curso :: !T.Text
    , turma :: !Int
    , emailE :: !T.Text
    } deriving (Show)

{-
username    password    firstname   lastname    email   cohort1
sjm1128065  sjm1128065  Adriano     Romero Francisco    drico.rfc@gmail.com 2013-2_CEA439-11
-}
-- | Modelo para CSV de saída de alunos
data AlunosSaida = AlunosSaida
    { username :: !T.Text
    , password :: !T.Text
    , firstname :: !T.Text
    , lastname :: !T.Text
    , email :: !T.Text
    , cohort1 :: !T.Text
    } deriving (Show)

alunosSaidaHeader :: Header
alunosSaidaHeader = V.fromList [
                        "username"
                        , "password"
                        , "firstname"
                        , "lastname"
                        , "email"
                        , "cohort1"
                    ]

instance FromNamedRecord AlunosEntrada where
    parseNamedRecord r = AlunosEntrada
                            <$> r .: "ANO"
                            <*> r .: "SEMESTRE"
                            <*> r .: "COD DISCIPLINA"
                            <*> r .: "MATRICULA"
                            <*> r .: "NOME"
                            <*> r .: "CURSO"
                            <*> r .: "TURMA"
                            <*> r .: "EMAIL"

instance ToNamedRecord AlunosSaida where
    toNamedRecord (AlunosSaida u p f l e c) =
        namedRecord [
            "username" .= u
            , "password" .= p
            , "firstname" .= f
            , "lastname" .= l
            , "email" .= e
            , "cohort1" .= c
        ]

opcoesDec :: DecodeOptions
opcoesDec = defaultDecodeOptions {
                decDelimiter = fromIntegral (C.ord ';')
            }

opcoesEnc :: EncodeOptions
opcoesEnc = defaultEncodeOptions {
                encDelimiter = fromIntegral (C.ord ';')
            }

-- | Trata arquivo de alunos (diário de classe).
transformaAlunos :: FilePath -> IO ()
transformaAlunos alunosFile = do
    alunosCSVData <- readFile alunosFile
    case decodeByNameWith opcoesDec alunosCSVData of
        Left err -> putStrLn err
        Right (_, vector) -> do
            alunosSaidas <- V.forM vector $ \ elemVetor -> do
                let user = T.toLower (curso elemVetor `T.append` T.filter (/= '.') (matricula elemVetor))
                    pwd = user
                    nm = T.splitOn " " $ nome elemVetor
                    fname = T.toTitle $ T.concat $ take 1 nm
                    lname = T.toTitle $ T.unwords $ drop 1 nm
                    mail = emailE elemVetor
                    chrt1 = T.pack (show $ ano elemVetor) `T.append` "-" `T.append` T.pack (show $ semestre elemVetor) `T.append` "_" `T.append` disciplina elemVetor `T.append` "-" `T.append` T.pack (show $ turma elemVetor)
                    sd = AlunosSaida user pwd fname lname mail chrt1
                return sd
            --print (V.toList AlunosSaidas)
            let outputFile = (cohort1 ((V.toList alunosSaidas)!!0)) `T.append` ".csv"
            writeFile (T.unpack outputFile) $ encodeByNameWith opcoesEnc alunosSaidaHeader (V.toList alunosSaidas)

