{-# LANGUAGE OverloadedStrings #-}

-- | Módulo para transformar arquivos CSV do MinhaUFOP.
module Transformações
    ( transformaDisciplinas
    , transformaAlunos
    ) where

import Control.Applicative
import Prelude hiding (readFile, writeFile)
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.Vector as V
import Data.Csv
import qualified Data.Text.Lazy as T
import qualified Data.Char as C


-- | Modelo para CSV de elemVetorada de disciplinas
data DisciplinasEntrada = DisciplinasEntrada
    { anoD :: !Int
    , semestreD :: !Int
    , disciplinaD :: !T.Text
    , descricaoD :: !T.Text
    , departamentoD :: !T.Text
    , turmaD :: !Int
    , cargaHorariaD :: !Int
    , professoresD :: !T.Text
    } deriving (Show)

-- | Modelo para CSV de saída de disciplinas
-- Mais informações em https://docs.moodle.org/29/en/Upload_courses
data DisciplinasSaida = DisciplinasSaida
    { shortnameD :: !T.Text
    , fullnameD :: !T.Text
    , categoryIDNumberD :: !T.Text
    , categoryPathD :: !T.Text
    , summary :: !T.Text
    , visibleD :: !T.Text
    , formatD :: !T.Text
    --, enrolment_1D :: !T.Text
    --, enrolment_1_roleD :: !T.Text
    } deriving (Show)

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

disciplinasSaidaHeader :: Header
disciplinasSaidaHeader = V.fromList [
                            "shortname"
                            , "fullname"
                            , "category_idnumber"
                            , "category_path"
                            , "summary"
                            , "visible"
                            , "format"
                            --, "enrolment_1"
                            --, "enrolment_1_role"
                        ]

instance FromNamedRecord DisciplinasEntrada where
    parseNamedRecord r = DisciplinasEntrada
                            <$> r .: "ANO"
                            <*> r .: "SEMESTRE"
                            <*> r .: "DISCIPLINA"
                            <*> r .: "DESCRICAO"
                            <*> r .: "DEPARTAMENTO"
                            <*> r .: "TURMA"
                            <*> r .: "CARGA HORARIA"
                            <*> r .: "PROFESSORES"

instance ToNamedRecord DisciplinasSaida where
    toNamedRecord (DisciplinasSaida s f cidn cp summ v ft ) = -- e1 e1r) =
        namedRecord [
            "shortname" .= s
            , "fullname" .= f
            , "category_idnumber" .= cidn
            , "category_path" .= cp
            , "summary" .= summ
            , "visible" .= v
            , "format" .= ft
            --, "enrolment_1" .= e1
            --, "enrolment_1_role" .= e1r
        ]

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

-- | Trata arquivo de disciplinas por departamento.
transformaDisciplinas :: FilePath -> IO ()
transformaDisciplinas disciplinasFile = do
    disciplinasCSVData <- readFile disciplinasFile
    case decodeByNameWith opcoesDec disciplinasCSVData of
        Left err -> putStrLn err
        Right (_, vector) -> do
            disciplinasSaidas <- V.forM vector $ \ elemVetor -> do
                let
                    shortN = T.pack (show $ anoD elemVetor)
                                `T.append` "-"
                                `T.append` T.pack (show $ semestreD elemVetor)
                                `T.append` "_"
                                `T.append` (disciplinaD elemVetor)
                                `T.append` "-"
                                `T.append` T.pack (show $ turmaD elemVetor)
                    formataProfessores = T.replace "(P)" ""
                                . T.replace "(T)" ""
                                . T.replace "(T+P)" ""
                    fullN = disciplinaD elemVetor
                                `T.append` "-"
                                `T.append` T.pack (show $ turmaD elemVetor)
                                `T.append` " - "
                                `T.append` T.toTitle (descricaoD elemVetor)
                                `T.append` " - "
                                `T.append` T.toTitle ((formataProfessores . professoresD) elemVetor)
                    catIDNum = T.pack (show $ anoD elemVetor)
                                `T.append` "-"
                                `T.append` T.pack (show $ semestreD elemVetor)
                    catPath = departamentoD elemVetor
                                `T.append` " / "
                                `T.append` T.pack (show $ anoD elemVetor)
                                `T.append` "-"
                                `T.append` T.pack (show $ semestreD elemVetor)
                    summ = "Disciplina: " `T.append` (disciplinaD elemVetor) `T.append` " - " `T.append` T.toTitle (descricaoD elemVetor)
                            `T.append` "\nTurma: " `T.append` T.pack (show $ turmaD elemVetor)
                            `T.append` "\nProfessor(es): " `T.append` ((T.toTitle . professoresD) elemVetor)
                    vis = T.pack (show 1)
                    fmt = "topics"
                    --enrol_1 = "cohort"
                    --enrol_1_role = "student"
                return $ DisciplinasSaida shortN fullN catIDNum catPath summ vis fmt --enrol_1 enrol_1_role
            --print (V.toList AlunosSaidas)
            let outputFile = T.replace "/" "_" (T.filter (/= ' ') (categoryPathD ((V.toList disciplinasSaidas)!!0))) `T.append` ".csv"
            writeFile (T.unpack outputFile) $ encodeByNameWith opcoesEnc disciplinasSaidaHeader (V.toList disciplinasSaidas)

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

