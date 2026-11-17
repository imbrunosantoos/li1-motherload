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

-- | Verifica se uma linha do mapa é toda Ar
linhaDeAr :: [Bloco] -> Bool
linhaDeAr [] = True
linhaDeAr (x:xs)
          | x == Ar = linhaDeAr xs
          | otherwise = False

-- | Regra 1: o mapa não é vazio, é uma grelha(todas as linhas do mesmo tamanho) e a superfície (linha 0) é toda Ar
mapaValido :: Mapa -> Bool
mapaValido mapa = mapa /= [] && eMatrizValida mapa && linhaDeAr(head mapa)

-- | Regra 2-a escavadora está num bloco Ar dentro do mapa, os valores respeitam os
-- limites (combustível, vida, capacidades, broca, carga, contadores) e, se está a voar,
-- a queda é 0.
escavadoraValida :: Mapa -> Escavadora -> Bool
escavadoraValida mapa escavadora = posicaoOk && combustivelEVidaOk && capacidadesOk
                                && brocaOk && cargaOk && contadoresOk && vooOk
    where 
        posicaoOk = blocoEm(posicaoEscavadora escavadora)mapa == Just Ar 
        combustivelEVidaOk = combustivelEscavadora escavadora <= depositoEscavadora escavadora
                            && combustivelEscavadora escavadora >= 0
                            && vidaEscavadora escavadora >= 0 
                            && vidaEscavadora escavadora <= vidaMaximaEscavadora escavadora
        capacidadesOk      = depositoEscavadora escavadora > 0
                            && vidaMaximaEscavadora escavadora > 0
                            && capacidadeEscavadora escavadora > 0
        brocaOk            = brocaEscavadora escavadora >= 1
        cargaOk            = capacidadeEscavadora escavadora >= (pesoCarga escavadora)
        contadoresOk       = quedaEscavadora escavadora >= 0 
                            && dinamitesEscavadora escavadora >= 0
        vooOk              = if (voandoEscavadora escavadora) then quedaEscavadora escavadora == 0 else True

-- | Verifica se um edifício está na superfície (linha 0) e dentro do mapa
edificioNaSuperficie :: Mapa -> Edificio -> Bool
edificioNaSuperficie mapa edificio = fst (posicaoEdificio edificio) == 0
                                  && ePosicaoMatrizValida (posicaoEdificio edificio) mapa

-- | Verifica se todos os edifícios estão na superfície e dentro do mapa 
todosNaSuperficie :: Mapa -> [Edificio] -> Bool
todosNaSuperficie _ [] = True
todosNaSuperficie mapa (e:es) = edificioNaSuperficie mapa e 
                              && todosNaSuperficie mapa es

               
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

-- | Regra 3-todos os edifícios estão na superfície, 
--  não há dois na mesma posição e há pelo menos um de cada tipo
edificiosValidos :: Mapa -> [Edificio] -> Bool
edificiosValidos mapa edificios  = temTodosTipos (map tipoEdificio edificios)
                                && semRepetidos (map posicaoEdificio edificios)
                                && todosNaSuperficie mapa edificios

-- | Verifica se uma dinamite está num bloco Ar dentro do mapa e tem o temporizador >= 0
dinamiteValida :: Mapa -> Objeto -> Bool
dinamiteValida mapa dinamite = blocoEm(posicaoDinamite dinamite) mapa == Just Ar 
                             && tempoDinamite dinamite >= 0

-- | Regra 4-todas as dinamites do jogo são válidas
objetosValidos :: Mapa -> [Objeto] -> Bool
objetosValidos _ [] = True
objetosValidos mapa (x:xs) 
                    | dinamiteValida mapa x = objetosValidos mapa xs
                    | otherwise = False

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
