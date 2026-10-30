{-|
Module      : Main
Description : Testes e feedback da Tarefa 3.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Os testes da Tarefa 3. Correr com @cabal run t3-feedback@: corre a 'avancaTempo' sobre cada teste
e abre a página de feedback com o resultado obtido e o esperado.
-}
module Main (main) where

import Labs2026
import Magic
import Tarefa3

-- | Os testes da Tarefa 3, que são avaliados pelos docentes.
testesTarefa3 :: [Jogo]
testesTarefa3 = []

-- | Corre os testes e abre a página de feedback. Não alterar.
main :: IO ()
main = do
  resultados <- mapM (runTest . avancaTempo) testesTarefa3
  runFeedback (T3 testesTarefa3 resultados)
