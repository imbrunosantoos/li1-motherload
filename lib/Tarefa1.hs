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

-- | Verifica se um edifício está na superfície (linha 0) e dentro do mapa
edificioNaSuperficie :: Mapa -> Edificio -> Bool
edificioNaSuperficie mapa edificio =  fst(posicaoEdificio edificio) == 0 
                                    && ePosicaoMatrizValida (posicaoEdificio edificio) mapa
               
-- | Verifica se uma lista não tem elementos repetidos 
semRepetidos :: [Posicao] -> Bool
semRepetidos [] = True
semRepetidos (x:xs) 
            | notElem x xs = semRepetidos xs
            | otherwise = False

-- | Verifica se há pelo menos um edifício de cada tipo 
temTodosTipos :: [TipoEdificio] -> Bool
temTodosTipos tipos = elem Posto tipos 
                && elem Refinaria tipos
                && elem Oficina tipos

edificiosValidos :: Mapa -> [Edificio] -> Bool
edificiosValidos = undefined

objetosValidos :: Mapa -> [Objeto] -> Bool
objetosValidos = undefined

-- | Regra 5: os créditos não podem ser negativos.
creditosValidos :: Creditos -> Bool
creditosValidos creditos = creditos >= 0

-- | Número associado a uma melhoria
valorMelhoria :: Melhoria -> Int
valorMelhoria (Deposito combustivel) = combustivel
valorMelhoria (Vida int) = int
valorMelhoria (Broca nivel) = nivel
valorMelhoria (Carga peso) = peso 
valorMelhoria (Dinamites int) = int

-- | Regra 6: nenhum preço da loja é negativo e todas as melhorias têm valor positivo
lojaValida :: Loja -> Bool
lojaValida [] = True
lojaValida ((preco, melhoria):resto) 
            | creditosValidos preco && valorMelhoria melhoria > 0 = lojaValida resto
            | otherwise = False 
