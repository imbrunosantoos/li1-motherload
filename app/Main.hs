{-|
Module      : Main
Description : O jogo.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

O jogo. Correr com @cabal run motherload-game@.
-}
module Main where

import Graphics.Gloss

import Desenhar
import Eventos
import Motherload
import Tempo

-- | A janela do jogo.
janela :: Display
janela = InWindow "Motherload" (1280, 720) (0, 0)

-- | A cor de fundo.
fundo :: Color
fundo = white

-- | Imagens por segundo.
fr :: Int
fr = 60

-- | Função principal que invoca o Gloss para correr o jogo.
main :: IO ()
main = play janela fundo fr estadoInicial desenha reageEventos reageTempo
