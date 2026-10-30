{-|
Module      : Tarefa0_geral
Description : Funções auxiliares gerais.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Módulo que define funções genéricas sobre listas e matrizes, úteis na realização do projeto de
LI1\/LP1 em 2026\/27.

__NB:__ Nenhuma função deste módulo conhece as regras do jogo. As funções específicas do jogo vivem no módulo "Tarefa0_2026".
-}
module Tarefa0_geral where

-- * Tipos de dados

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://haslab.github.io/Teaching/LI1/2627/img/matriz.png>>
--
-- O @Mapa@ do jogo, em "Labs2026", é precisamente uma matriz de blocos.
type Matriz a = [[a]]

-- | Uma posição numa matriz é dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2627/img/posicaomatriz.png>>
type Posicao = (Int, Int)

-- | Dimensão de uma matriz: (/número de linhas/, /número de colunas/).
type Dimensao = (Int, Int)

-- | Direção de um movimento numa matriz: 'Cima' e 'Baixo' mudam a linha, 'Esquerda' e 'Direita'
-- mudam a coluna.
data Direcao = Cima | Baixo | Esquerda | Direita
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- * Funções não-recursivas

-- | Verifica se o índice pertence à lista.
--
-- >>> eIndiceListaValido 2 "abc"
-- True
-- >>> eIndiceListaValido (-1) "abc"
-- False
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido = undefined

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Não existem matrizes de dimensão /m * 0/ ou /0 * n/; qualquer matriz vazia tem
-- dimensão /0 * 0/.
--
-- >>> dimensaoMatriz [[1,2,3],[4,5,6]]
-- (2,3)
-- >>> dimensaoMatriz [[],[]]
-- (0,0)
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz = undefined

-- | Verifica se a posição pertence à matriz.
--
-- __NB:__ Posições com coordenadas negativas nunca pertencem a uma matriz.
--
-- Vamos buscar a linha pedida e vemos se a coluna existe /nessa/ linha, para que a função também
-- dê a resposta certa se as linhas tiverem tamanhos diferentes.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida = undefined

-- | Move uma posição uma unidade no sentido de uma direção.
--
-- Recorde que a linha cresce para baixo: 'Cima' /decrementa/ a linha.
--
-- >>> movePosicao Cima (3,4)
-- (2,4)
-- >>> movePosicao Direita (3,4)
-- (3,5)
movePosicao :: Direcao -> Posicao -> Posicao
movePosicao = undefined

-- | Distância de Chebyshev entre duas posições, ou seja, o número de passos necessários para ir
-- de uma à outra podendo mover-se também na diagonal.
--
-- >>> distanciaChebyshev (0,0) (1,3)
-- 3
distanciaChebyshev :: Posicao -> Posicao -> Int
distanciaChebyshev = undefined

-- | Todas as posições a uma distância de Chebyshev menor ou igual a um dado raio, incluindo a
-- própria posição central.
--
-- __NB:__ A lista pode conter posições fora de qualquer matriz, incluindo posições com
-- coordenadas negativas. Cabe a quem usa esta função filtrá-las.
--
-- >>> vizinhancaPosicao 1 (0,0)
-- [(-1,-1),(-1,0),(-1,1),(0,-1),(0,0),(0,1),(1,-1),(1,0),(1,1)]
vizinhancaPosicao :: Int -> Posicao -> [Posicao]
vizinhancaPosicao = undefined

-- * Funções recursivas

-- | Devolve o elemento num dado índice de uma lista.
--
-- __NB:__ Devolve @Nothing@ se o índice não existir, /incluindo quando é negativo/.
--
-- >>> encontraIndiceLista 1 "abc"
-- Just 'b'
-- >>> encontraIndiceLista 5 "abc"
-- Nothing
encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista = undefined

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o índice não existir.
--
-- >>> atualizaIndiceLista 0 'z' "abc"
-- "zbc"
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista = undefined

-- | Devolve o elemento numa dada posição de uma matriz.
--
-- __NB:__ Devolve @Nothing@ se a posição não existir.
encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz = undefined

-- | Modifica um elemento numa dada posição de uma matriz.
--
-- __NB:__ Devolve a própria matriz se a posição não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz = undefined

-- | Verifica se uma matriz é válida, no sentido em que modela um retângulo.
--
-- __NB:__ Todas as linhas devem ter o mesmo número de colunas, e esse número não pode ser 0.
--
-- >>> eMatrizValida [[1,2],[3,4]]
-- True
-- >>> eMatrizValida [[1,2],[3]]
-- False
eMatrizValida :: Matriz a -> Bool
eMatrizValida = undefined
