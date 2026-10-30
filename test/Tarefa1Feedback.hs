{-|
Module      : Main
Description : Testes e feedback da Tarefa 1.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Os testes da Tarefa 1. Correr com @cabal run t1-feedback@: corre a 'validaJogo' sobre cada teste
e abre a página de feedback com o resultado obtido e o esperado.
-}
module Main (main) where

import Labs2026
import Magic
import Tarefa1

-- | Os testes da Tarefa 1, que são avaliados pelos docentes.
testesTarefa1 :: [Jogo]
testesTarefa1 = []

-- | Corre os testes e abre a página de feedback. Não alterar.
main :: IO ()
main = do
  resultados <- mapM (runTest . validaJogo) testesTarefa1
  runFeedback (T1 testesTarefa1 resultados)
