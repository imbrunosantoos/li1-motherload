{-|
Module      : Main
Description : Testes e feedback da Tarefa 2.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Os testes da Tarefa 2. Correr com @cabal run t2-feedback@: corre a 'efetuaJogada' sobre cada teste
e abre a página de feedback com o resultado obtido e o esperado.
-}
module Main (main) where

import Labs2026
import Magic
import Tarefa2

-- | Os testes da Tarefa 2, que são avaliados pelos docentes.
testesTarefa2 :: [(Jogada, Jogo)]
testesTarefa2 = []

-- | Corre os testes e abre a página de feedback. Não alterar.
main :: IO ()
main = do
  resultados <- mapM (runTest . uncurry efetuaJogada) testesTarefa2
  runFeedback (T2 testesTarefa2 resultados)
