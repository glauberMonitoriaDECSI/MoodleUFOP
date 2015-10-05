-- | Main entry point to the application.
module Main where

import System.Environment
import Options.Applicative
import Transformações

data Opções = Opções
  { disciplinas :: [String]
  , alunos :: [String] }

opções :: Parser Opções
opções = Opções
     <$> many (strOption
         ( short 'd'
        <> metavar "DISCIPLINA.csv"
        <> help "Arquivo CSV de Disciplinas de Departamento." ))
     <*> many (strOption
         ( short 'a'
        <> metavar "ALUNO.csv"
        <> help "Arquivo CSV de Alunos (diário de classe)." ))

transformar :: Opções -> IO ()
transformar (Opções [] []) = error "Utilize -h para informações."
transformar (Opções d a) = do
    mapM_ transformaDisciplinas d
    mapM_ transformaAlunos a

-- | The main function.
main :: IO ()
main = execParser opts >>= transformar
  where
    opts = info (helper <*> opções)
            ( fullDesc
            <> progDesc "Este programa prepara os arquivos CSV do MinhaUFOP para serem utilizados na carga de arquivos do Moodle para criação de cursos e alunos. O arquivo DISCIPLINA.csv é o CSV de disciplinas de um departamento. O arquivo ALUNO.csv é o arquivo do diário de classe para uma dada turma. É possível repetir as opções para diversos arquivos."
            <> header "moodleUFOP - Transforma arquivos CSV do MinhaUFOP para carga no Moodle."
            )

