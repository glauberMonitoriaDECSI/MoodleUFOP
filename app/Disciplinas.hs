{-# LANGUAGE OverloadedStrings #-}

-- | Módulo para transformar arquivos CSV do MinhaUFOP.
module Disciplinas
    ( transformaDisciplinas
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
