{-|
Module      : Tarefa1
Description : Validação de jogos.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2026\/27.
-}
module Tarefa1 where

import Labs2026
import Tarefa0_geral
import Tarefa0_2026

-- | Função principal da Tarefa 1. Recebe um jogo e verifica se é válido.
validaJogo :: Jogo -> Bool
validaJogo jogo = mapaValido (mapaJogo jogo) 
                            && escavadoraValida (mapaJogo jogo) (escavadoraJogo jogo)
                            && edificiosValidos (mapaJogo jogo) (edificiosJogo jogo)
                            && objetosValidos (mapaJogo jogo) (objetosJogo jogo)
                            && creditosValidos (creditosJogo jogo)
                            && lojaValida (lojaJogo jogo)

-- * Funções auxiliares

mapaValido :: Mapa -> Bool
mapaValido = undefined

escavadoraValida :: Mapa -> Escavadora -> Bool
escavadoraValida = undefined

edificiosValidos :: Mapa -> [Edificio] -> Bool
edificiosValidos = undefined

objetosValidos :: Mapa -> [Objeto] -> Bool
objetosValidos = undefined

-- | Regra 5: os créditos não podem ser negativos.
creditosValidos :: Creditos -> Bool
creditosValidos creditos = creditos >= 0

lojaValida :: Loja -> Bool
lojaValida = undefined