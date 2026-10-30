{-|
Module      : Tarefa0_2026
Description : Funções auxiliares do jogo.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Módulo que define funções auxiliares específicas do modelo de jogo, úteis na realização do
projeto de LI1\/LP1 em 2026\/27.
-}
module Tarefa0_2026 where

import Labs2026

-- * Mapa

-- | Devolve o bloco numa dada posição do mapa.
--
-- __NB:__ Devolve @Nothing@ sempre que a posição não pertence ao mapa. Isso /inclui posições com
-- coordenadas negativas/ — que ocorrem naturalmente, por exemplo ao tentar subir a partir da
-- linha 0 ou ao calcular o raio de uma explosão à superfície.
--
-- >>> blocoEm (0,0) [[Ar],[Terra]]
-- Just Ar
-- >>> blocoEm (-1,0) [[Ar],[Terra]]
-- Nothing
blocoEm :: Posicao -> Mapa -> Maybe Bloco
blocoEm = undefined

-- | Substitui o bloco numa dada posição do mapa.
--
-- __NB:__ Devolve o próprio mapa se a posição não pertencer ao mapa.
atualizaBloco :: Posicao -> Bloco -> Mapa -> Mapa
atualizaBloco = undefined

-- | Verifica se um bloco é escavável, ou seja, se tem 'durezaBloco'.
--
-- __NB:__ 'Ar' e 'Rocha' não são escaváveis.
eBlocoEscavavel :: Bloco -> Bool
eBlocoEscavavel = undefined

-- * Escavadora

-- | Aplica uma transformação à escavadora do jogo.
atualizaEscavadora :: (Escavadora -> Escavadora) -> Jogo -> Jogo
atualizaEscavadora = undefined

-- | Verifica se a escavadora está apoiada, ou seja, se /não/ está suspensa no ar.
--
-- A escavadora está apoiada se o bloco imediatamente abaixo existir e não for 'Ar', ou se estiver
-- na última linha do mapa. Só se escava quando se está apoiado.
apoiado :: Jogo -> Bool
apoiado = undefined

-- | Peso total dos minérios transportados pela escavadora.
--
-- O peso limita a carga: ver 'capacidadeEscavadora'.
pesoCarga :: Escavadora -> Peso
pesoCarga = undefined

-- | Valor total dos minérios transportados pela escavadora, ou seja, quanto rende 'Vende'.
valorCarga :: Escavadora -> Creditos
valorCarga = undefined

-- | A escavadora perde vida, sem nunca ficar com vida negativa. É o que acontece com a 'Lava', com
-- as explosões e com as quedas.
sofreDano :: Int -> Escavadora -> Escavadora
sofreDano = undefined

-- * Edifícios

-- | Devolve o tipo de edifício numa dada posição, se lá existir algum.
--
-- __NB:__ Se houver mais do que um edifício na mesma posição — o que a Tarefa 1 não permite —
-- devolve o primeiro da lista.
edificioEm :: Posicao -> Jogo -> Maybe TipoEdificio
edificioEm = undefined
