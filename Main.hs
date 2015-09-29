{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to the application.
module Main where

import System.Environment
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import qualified Data.Text.Lazy as T
import qualified Data.Char as C

-- | Modelo para CSV de entrada de disciplinas
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
data DisciplinasSaida = DisciplinasSaida
    { shortnameD :: !T.Text
    , fullnameD :: !T.Text
    , categoryD :: !T.Text
    , visibleD :: !T.Text
    , formatD :: !T.Text
    , enrolment_1D :: !T.Text
    , enrolment_1_roleD :: !T.Text
    } deriving (Show)

{-
ANO SEMESTRE    COD DISCIPLINA  MATRICULA   NOME    CURSO   TURMA   EMAIL
2013    2   CEA439  11.2.8065   ADRIANO ROMERO FRANCISCO    SJM 11  drico.rfc@gmail.com
-}
-- | Modelo para CSV de entrada de alunos
data AlunosEntrada = AlunosEntrada
    { ano :: !Int
    , semestre :: !Int
    , codigo :: !T.Text
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
disciplinasSaidaHeader = V.fromList ["shortname", "fullname", "category", "visible", "format", "enrolment_1", "enrolment_1_role"]

instance FromNamedRecord DisciplinasEntrada where
    parseNamedRecord r = DisciplinasEntrada <$> r .: "ANO" <*> r .: "SEMESTRE" <*> r .: "DISCIPLINA" <*> r .: "DESCRICAO" <*> r .: "DEPARTAMENTO" <*> r .: "TURMA" <*> r .: "CARGA HORARIA" <*> r .: "PROFESSORES"

instance ToNamedRecord DisciplinasSaida where
    toNamedRecord (DisciplinasSaida s f c v ft e1 e1r) =
        namedRecord ["shortname" .= s, "fullname" .= f, "category" .= c, "visible" .= v, "format" .= ft, "enrolment_1" .= e1, "enrolment_1_role" .= e1r]

alunosSaidaHeader :: Header
alunosSaidaHeader = V.fromList ["username", "password", "firstname", "lastname", "email", "cohort1"]

instance FromNamedRecord AlunosEntrada where
    parseNamedRecord r = AlunosEntrada <$> r .: "ANO" <*> r .: "SEMESTRE" <*> r .: "COD DISCIPLINA" <*> r .: "MATRICULA" <*> r .: "NOME" <*> r .: "CURSO" <*> r .: "TURMA" <*> r .: "EMAIL"

instance ToNamedRecord AlunosSaida where
    toNamedRecord (AlunosSaida u p f l e c) =
        namedRecord ["username" .= u, "password" .= p, "firstname" .= f, "lastname" .= l, "email" .= e, "cohort1" .= c]

opcoesDec :: DecodeOptions
opcoesDec = defaultDecodeOptions {
                decDelimiter = fromIntegral (C.ord ';')
            }

opcoesEnc :: EncodeOptions
opcoesEnc = defaultEncodeOptions {
                encDelimiter = fromIntegral (C.ord ';')
            }

-- | The main entry point.
main :: IO ()
main = do
    (disciplinasFile:alunosFile:_) <- getArgs
    -- Trata arquivo de disciplinas
    disciplinasCSVData <- BL.readFile disciplinasFile
    case decodeByNameWith opcoesDec disciplinasCSVData of
        Left err -> putStrLn err
        Right (_, vector) -> do
            disciplinasSaidas <- V.forM vector $ \ entr -> do
                let
                    shortN = T.pack (show $ anoD entr)
                                `T.append` "-"
                                `T.append` T.pack (show $ semestreD entr)
                                `T.append` "_"
                                `T.append` (disciplinaD entr)
                                `T.append` "-"
                                `T.append` T.pack (show $ turmaD entr)
                    fullN = disciplinaD entr
                                `T.append` "-"
                                `T.append` T.pack (show $ turmaD entr)
                                `T.append` " - "
                                `T.append` T.toTitle (descricaoD entr)
                                `T.append` " - "
                                `T.append` T.toTitle (professoresD entr)
                    cat = departamentoD entr
                            `T.append` " / "
                            `T.append` T.pack (show $ anoD entr)
                            `T.append` "-"
                            `T.append` T.pack (show $ semestreD entr)
                    vis = T.pack (show 1)
                    fmt = "topics"
                    enrol_1 = "cohort"
                    enrol_1_role = "student"
                return $ DisciplinasSaida shortN fullN cat vis fmt enrol_1 enrol_1_role
            --print (V.toList AlunosSaidas)
            let outputFile = "DisciplinasSaida.csv"
            BL.writeFile outputFile $ encodeByNameWith opcoesEnc disciplinasSaidaHeader (V.toList disciplinasSaidas)
    -- Trata arquivo de alunos
    alunosCSVData <- BL.readFile alunosFile
    case decodeByNameWith opcoesDec alunosCSVData of
        Left err -> putStrLn err
        Right (_, vector) -> do
            alunosSaidas <- V.forM vector $ \ entr -> do
                let user = T.toLower (curso entr `T.append` T.filter (/= '.') (matricula entr))
                    pwd = user
                    nm = T.splitOn " " $ nome entr
                    fname = T.toTitle $ T.concat $ take 1 nm
                    lname = T.toTitle $ T.unwords $ drop 1 nm
                    mail = emailE entr
                    c1 = T.pack (show $ ano entr) `T.append` "-" `T.append` T.pack (show $ semestre entr) `T.append` "_" `T.append` codigo entr `T.append` "-" `T.append` T.pack (show $ turma entr)
                    sd = AlunosSaida user pwd fname lname mail c1
                return sd
            --print (V.toList AlunosSaidas)
            let outputFile = "AlunosSaida.csv" --T.unpack (cohort1 ((V.toList alunosSaidas)!!0)) ++ ".csv"
            BL.writeFile outputFile $ encodeByNameWith opcoesEnc alunosSaidaHeader (V.toList alunosSaidas)
