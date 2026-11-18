{-|
Module      : Main
Description : Testes e feedback da Tarefa 3.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Os testes da Tarefa 3. Correr com @cabal run t3-feedback@: corre a 'avancaTempo' sobre cada teste
e abre a página de feedback com o resultado obtido e o esperado.
-}
module Main (main) where

import Labs2026
import Magic
import Tarefa3

-- | Mapa exemplo do enunciado- 7 linhas e 13 colunas
mapaExemplo :: Mapa
mapaExemplo =
  [ replicate 13 Ar
  , [Terra, Terra, Rocha, Ar, Terra, Terra, Rocha, Terra, Terra, Terra, Rocha, Terra, Terra]
  , [Terra, Minerio Ferro, Terra, Ar, Terra, Minerio Prata, Terra, Terra, Lava, Terra, Terra, Minerio Bronze, Terra]
  , [Terra, Terra, Terra, Ar, Terra, Minerio Ouro, Rocha, Terra, Terra, Minerio Platina, Terra, Terra, Terra]
  , [Terra, Minerio Diamante, Terra, Ar, Terra, Lava, Terra, Terra, Rocha, Terra, Terra, Minerio Ferro, Terra]
  , [Rocha, Terra, Terra, Terra, Ar, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Rocha]
  , [Minerio Ouro, Terra, Terra, Rocha, Terra, Terra, Terra, Terra, Minerio Platina, Terra, Terra, Rocha, Terra]
  ]

-- | Escavadora do exemplo do enunciado, em (4,3), com o depósito e a vida cheios e a carga vazia
escavadoraExemplo :: Escavadora
escavadoraExemplo = Escavadora
  { posicaoEscavadora = (4,3), direcaoEscavadora = Baixo
  , combustivelEscavadora = 200, depositoEscavadora = 200
  , vidaEscavadora = 100, vidaMaximaEscavadora = 100
  , brocaEscavadora = 1, cargaEscavadora = [], capacidadeEscavadora = 20
  , quedaEscavadora = 0, voandoEscavadora = False, dinamitesEscavadora = 0
  }

-- | Jogo do exemplo do enunciado-É válido
jogoExemplo :: Jogo
jogoExemplo = Jogo
  { mapaJogo = mapaExemplo
  , escavadoraJogo = escavadoraExemplo
  , edificiosJogo = [Edificio Posto (0,2), Edificio Refinaria (0,6), Edificio Oficina (0,10)]
  , objetosJogo = []
  , creditosJogo = 0
  , lojaJogo = [ (250, Dinamites 3), (500, Carga 40), (800, Deposito 400), (1200, Vida 200)
               , (1500, Broca 2), (3000, Carga 80), (3500, Deposito 800), (4000, Vida 400)
               , (5000, Broca 3) ]
  }

-- | O jogo do exemplo, com outra escavadora
comEscavadora :: Escavadora -> Jogo
comEscavadora esc = jogoExemplo { escavadoraJogo = esc }

-- | Mapa pequeno (4x4), para testes focados em um detalhe
mapaPequeno :: Mapa
mapaPequeno =
  [ [Ar,    Ar,    Ar,           Ar   ]
  , [Terra, Ar,    Rocha,        Terra]
  , [Terra, Ar,    Minerio Ouro, Lava ]
  , [Rocha, Terra, Terra,        Terra]
  ]

-- | Jogo válido no mapa pequeno: a escavadora está em (2,1), pousada em Terra
jogoPequeno :: Jogo
jogoPequeno = jogoExemplo
  { mapaJogo = mapaPequeno
  , escavadoraJogo = escavadoraExemplo { posicaoEscavadora = (2,1) }
  , edificiosJogo = [Edificio Posto (0,0), Edificio Refinaria (0,1), Edificio Oficina (0,3)]
  }
  
-- * Testes da perdeuJogo

-- | Perde-a escavadora ficou sem vida
escavadoraSemVida :: Jogo
escavadoraSemVida = comEscavadora (escavadoraExemplo { vidaEscavadora = 0 })

-- | Perde- sem combustível em (4,3), longe do 'Posto'
semCombustivelLonge :: Jogo
semCombustivelLonge = comEscavadora (escavadoraExemplo { combustivelEscavadora = 0 })

-- | Perde- no 'Posto' sem combustível e sem créditos para abastecer
postoSemCreditos :: Jogo
postoSemCreditos = comEscavadora (escavadoraExemplo { posicaoEscavadora = (0,2), combustivelEscavadora = 0 })

-- | Não perde-no 'Posto' sem combustível, mas com 1 crédito, que chega para abastecer
postoComCredito :: Jogo
postoComCredito = postoSemCreditos { creditosJogo = 1 }

-- | Perde-na 'Refinaria' sem combustível, os créditos não servem, porque não é o 'Posto'
refinariaSemCombustivel :: Jogo
refinariaSemCombustivel = (comEscavadora (escavadoraExemplo { posicaoEscavadora = (0,6), combustivelEscavadora = 0 })) { creditosJogo = 1000 }

-- | Não perde-o combustível é igual ao 'custoMovimento', o que ainda chega para um movimento
combustivelNoLimite :: Jogo
combustivelNoLimite = comEscavadora (escavadoraExemplo { combustivelEscavadora = 1 })


-- | Os testes da Tarefa 3, que são avaliados pelos docentes.
testesTarefa3 :: [Jogo]
testesTarefa3 = [ escavadoraSemVida, semCombustivelLonge, postoSemCreditos,
                 postoComCredito, refinariaSemCombustivel , combustivelNoLimite]

-- | Corre os testes e abre a página de feedback. Não alterar.
main :: IO ()
main = do
  resultados <- mapM (runTest . avancaTempo) testesTarefa3
  runFeedback (T3 testesTarefa3 resultados)
