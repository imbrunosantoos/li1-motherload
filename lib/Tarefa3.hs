{-|
Module      : Tarefa3
Description : Passagem do tempo.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2026\/27.
-}
module Tarefa3 where

import Labs2026
import Tarefa0_2026
import Tarefa0_geral

-- | Função principal da Tarefa 3. Avança o jogo um 'Ticks'.
avancaTempo :: Jogo -> Jogo
avancaTempo j
  | perdeuJogo j = j
  | otherwise = gravidade (avancaObjetos j)

-- * Fim do jogo

-- | Verifica se a escavadora ficou sem vida, ou seja, se a vida chegou a 0
--
-- Como a 'sofreDano' nunca deixa a vida ficar negativa, basta comparar com 0
semVida :: Escavadora -> Bool
semVida escavadora = vidaEscavadora escavadora == 0

-- | Verifica se o combustível da escavadora já não chega para um movimento,
-- ou seja, se é menor do que o 'custoMovimento'
faltaCombustivel :: Escavadora -> Bool
faltaCombustivel escavadora = combustivelEscavadora escavadora < custoMovimento

-- | Verifica se a escavadora pode abastecer onde está- tem de estar no 'Posto',
-- e os créditos tem de chegar para pagar o combustível que falta até ao
-- 'custoMovimento', ao preço de 'precoCombustivel' por unidade
--OBS: só faz sentido quando falta combustível, com o depósito cheio, a conta
-- do que falta dá negativa e a condição dos créditos é sempre verdadeira
podeAbastecer :: Jogo -> Bool
podeAbastecer jogo = edificioEm(posicaoEscavadora(escavadoraJogo jogo))jogo == Just Posto
                    && creditosJogo jogo >=((custoMovimento - combustivelEscavadora(escavadoraJogo jogo))* precoCombustivel) 

{- | Se o jogo deve terminar ou não.
-}
perdeuJogo :: Jogo -> Bool
perdeuJogo j = semVida (escavadoraJogo j)
              || faltaCombustivel (escavadoraJogo j)
              && not (podeAbastecer j)

            

-- * Objetos

descontaTempo :: [Objeto] -> [Objeto]
descontaTempo [] = []
descontaTempo (x:xs) = x {tempoDinamite = (tempoDinamite x)-1}:descontaTempo xs

explodem :: [Objeto] -> [Objeto]
explodem [] = []
explodem (x:xs) 
        | tempoDinamite x <= 0 = x:explodem xs
        | otherwise = explodem xs

ficam :: [Objeto] -> [Objeto]
ficam [] = []
ficam (x:xs)
     | tempoDinamite x > 0 = x:ficam xs
     | otherwise = ficam xs

{-| Avança todos os objetos.
-}
avancaObjetos :: Jogo -> Jogo
avancaObjetos j = undefined

-- * Gravidade

-- | Vida que a escavadora perde ao aterrar depois de cair 
danoDaQueda :: Int -> Int
danoDaQueda queda = max 0 ((queda - alturaQuedaSegura) * danoQueda)

-- | A escavadora cai uma linha-a posição desce um bloco e a queda aumenta 1
caiUmaLinha :: Escavadora -> Escavadora
caiUmaLinha escavadora = escavadora { quedaEscavadora =  (quedaEscavadora escavadora)+1
                                    , posicaoEscavadora = movePosicao Baixo(posicaoEscavadora escavadora)}

-- | A escavadora aterra- perde a vida dada pela 'danoDaQueda' da sua queda,
-- e a queda volta a 0
aterra :: Escavadora -> Escavadora 
aterra escavadora = (sofreDano (danoDaQueda(quedaEscavadora escavadora)) escavadora) {quedaEscavadora = 0}

-- | A gravidade aplicada a escavadora- se está a voar, nada muda. se está no ar,
-- cai uma linha. se está apoiada, aterra 
gravidadeEscavadora :: Jogo -> Jogo
gravidadeEscavadora jogo 
                  | voandoEscavadora (escavadoraJogo jogo) = jogo
                  | not (apoiado jogo) = atualizaEscavadora caiUmaLinha  jogo
                  | otherwise = atualizaEscavadora aterra jogo

-- | Uma dinamite com 'Ar' por baixo cai uma linha, se estiver apoiada, fica onde está
caiDinamite :: Mapa -> Objeto -> Objeto
caiDinamite mapa objeto 
          | apoiadoAux (posicaoDinamite objeto) mapa = objeto
          | otherwise = objeto {posicaoDinamite = movePosicao Baixo (posicaoDinamite objeto)}

-- | Aplica a 'caiDinamite' a todas as dinamites da lista
caiDinamites :: Mapa -> [Objeto] -> [Objeto]
caiDinamites _ [] = []
caiDinamites mapa (x:xs) = caiDinamite mapa x:caiDinamites mapa xs 

-- | A gravidade num 'Ticks'- primeiro caem as dinamites e depois a escavadora. 
-- As dinamites caem mesmo com a escavadora a voar
gravidade :: Jogo -> Jogo
gravidade j = gravidadeEscavadora j{objetosJogo = caiDinamites (mapaJogo j) (objetosJogo j)}

