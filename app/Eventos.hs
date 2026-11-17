{-|
Module      : Eventos
Description : Reação ao teclado e ao rato.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Módulo para alterar o estado do jogo quando o jogador carrega numa tecla ou mexe o rato.
-}
module Eventos where

import Graphics.Gloss.Interface.Pure.Game

import Motherload

-- | Função que altera o estado do jogo no Gloss.
reageEventos :: Event -> Motherload -> Motherload
reageEventos _ e = e
