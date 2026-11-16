{-|
Module      : Desenhar
Description : Desenho do jogo.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Módulo para desenhar o estado do jogo no Gloss.
-}
module Desenhar where

import Graphics.Gloss

import Motherload

-- | Função que desenha o estado do jogo no Gloss.
desenha :: Motherload -> Picture
desenha _ = Translate (-250) 0 $ Scale 0.3 0.3 $ Text "Bem-vindo ao Motherload!"
