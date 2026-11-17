{-|
Module      : Main
Description : Testes e feedback da Tarefa 1.
Copyright   : Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
              Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

Os testes da Tarefa 1. Correr com @cabal run t1-feedback@: corre a 'validaJogo' sobre cada teste
e abre a página de feedback com o resultado obtido e o esperado.
-}
module Main (main) where

import Labs2026
import Magic
import Tarefa1

-- * Jogos base

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

-- | Mapa pequeno (4x4), para testes focados em um detalhe
mapaPequeno :: Mapa
mapaPequeno =
  [ [Ar,    Ar,    Ar,           Ar   ]
  , [Terra, Ar,    Rocha,        Terra]
  , [Terra, Ar,    Minerio Ouro, Lava ]
  , [Rocha, Terra, Terra,        Terra]
  ]

-- | O jogo do exemplo, com outra escavadora
comEscavadora :: Escavadora -> Jogo
comEscavadora esc = jogoExemplo { escavadoraJogo = esc }

-- * Testes

-- | Válido- combustível exatamente 0 
semCombustivel :: Jogo
semCombustivel = jogoExemplo { escavadoraJogo = escavadoraExemplo { combustivelEscavadora = 0 } }

-- | Inválido- a escavadora está num bloco de Terra
escavadoraEmTerra :: Jogo
escavadoraEmTerra = jogoExemplo { escavadoraJogo = escavadoraExemplo { posicaoEscavadora = (5,3) } }

-- | Inválido- créditos negativos 
creditosNegativos :: Jogo
creditosNegativos = jogoExemplo { creditosJogo = -1 }

-- | Inválido- há Terra na superfície, em (0,2) 
superficieComTerra :: Jogo
superficieComTerra = jogoPequeno
  { mapaJogo = [ [Ar,    Ar,    Terra,        Ar   ]    
               , [Terra, Ar,    Rocha,        Terra]
               , [Terra, Ar,    Minerio Ouro, Lava ]
               , [Rocha, Terra, Terra,        Terra]
               ]
  }

-- | Jogo válido no mapa pequeno-a escavadora está em (2,1), pousada em terra
jogoPequeno :: Jogo
jogoPequeno = jogoExemplo
  { mapaJogo = mapaPequeno
  , escavadoraJogo = escavadoraExemplo { posicaoEscavadora = (2,1) }
  , edificiosJogo = [Edificio Posto (0,0), Edificio Refinaria (0,1), Edificio Oficina (0,3)]
  }

-- | Válido-a loja pode estar vazia
lojaVazia :: Jogo
lojaVazia = jogoExemplo { lojaJogo = [] }

-- | Inválido-uma melhoria com preço negativo
precoNegativo :: Jogo
precoNegativo = jogoExemplo { lojaJogo = [(-1, Vida 200)] }

-- | Inválido- uma melhoria com valor 0(precisa ser positiva)
melhoriaSemValor :: Jogo
melhoriaSemValor = jogoExemplo { lojaJogo = [(100, Carga 0)] }

-- | Inválido-o Posto está na linha 1, e não na superfície 
edificioForaDaSuperficie :: Jogo
edificioForaDaSuperficie = jogoExemplo
  { edificiosJogo = [Edificio Posto (1,3), Edificio Refinaria (0,6), Edificio Oficina (0,10)] }

-- | Inválido-a Oficina está na linha 0, mas na coluna 13, fora do mapa
edificioForaDoMapa :: Jogo
edificioForaDoMapa = jogoExemplo
  { edificiosJogo = [Edificio Posto (0,2), Edificio Refinaria (0,6), Edificio Oficina (0,13)] }

-- | Inválido-o Posto e a Refinaria estão na mesma posição 
edificiosSobrepostos :: Jogo
edificiosSobrepostos = jogoExemplo
  { edificiosJogo = [Edificio Posto (0,2), Edificio Refinaria (0,2), Edificio Oficina (0,10)] }

-- | Inválido-falta a Oficina
semOficina :: Jogo
semOficina = jogoExemplo
  { edificiosJogo = [Edificio Posto (0,2), Edificio Refinaria (0,6)] }

-- | Válido-dois Postos, em posições diferentes
doisPostos :: Jogo
doisPostos = jogoExemplo
  { edificiosJogo = [Edificio Posto (0,2), Edificio Posto (0,4), Edificio Refinaria (0,6), Edificio Oficina (0,10)] }

-- | Inválido-o mapa é vazio 
mapaVazio :: Jogo
mapaVazio = jogoPequeno { mapaJogo = [] }

-- | Inválido- a linha 1 tem 3 colunas e as outras tem 4
mapaNaoRetangular :: Jogo
mapaNaoRetangular = jogoPequeno
  { mapaJogo = [ [Ar,    Ar,    Ar,           Ar   ]
               , [Terra, Ar,    Rocha              ]
               , [Terra, Ar,    Minerio Ouro, Lava ]
               , [Rocha, Terra, Terra,        Terra]
               ]
  }

-- | Inválido uma dinamite enterrada em Terra, em (5,3)
dinamiteEmTerra :: Jogo
dinamiteEmTerra = jogoExemplo { objetosJogo = [Dinamite (5,3) 2] }

-- | Inválido- uma dinamite com o temporizador negativo
dinamiteTempoNegativo :: Jogo
dinamiteTempoNegativo = jogoExemplo { objetosJogo = [Dinamite (3,3) (-1)] }

-- | Válido-dinamite com tempo 0, na mesma posição da escavadora
dinamiteNaEscavadora :: Jogo
dinamiteNaEscavadora = jogoExemplo { objetosJogo = [Dinamite (4,3) 0] }

-- | Inválido: a escavadora está na linha 7, fora do mapa (regra 2a)
escavadoraForaDoMapa :: Jogo
escavadoraForaDoMapa = comEscavadora (escavadoraExemplo { posicaoEscavadora = (7,3) })

-- | Inválido: mais combustível do que o depósito (regra 2b)
combustivelAcimaDoDeposito :: Jogo
combustivelAcimaDoDeposito = comEscavadora (escavadoraExemplo { combustivelEscavadora = 250 })

-- | Inválido: vida negativa (regra 2b)
vidaNegativa :: Jogo
vidaNegativa = comEscavadora (escavadoraExemplo { vidaEscavadora = -1 })

-- | Inválido: depósito com capacidade 0 (regra 2c) O combustível também é 0, para a 2b continuar válida
depositoZero :: Jogo
depositoZero = comEscavadora (escavadoraExemplo { depositoEscavadora = 0, combustivelEscavadora = 0 })

-- | Inválido: capacidade de carga 0 (regra 2c)
capacidadeZero :: Jogo
capacidadeZero = comEscavadora (escavadoraExemplo { capacidadeEscavadora = 0 })

-- | Inválido: broca de nível 0 (regra 2d)
brocaZero :: Jogo
brocaZero = comEscavadora (escavadoraExemplo { brocaEscavadora = 0 })

-- | Inválido: 6 Ouros pesam 24, acima da capacidade 20 (regra 2e)
cargaAcimaDaCapacidade :: Jogo
cargaAcimaDaCapacidade = comEscavadora (escavadoraExemplo { cargaEscavadora = replicate 6 Ouro })

-- | Válido no limite: 5 Ouros pesam 20, exatamente a capacidade (regra 2e)
cargaNoLimite :: Jogo
cargaNoLimite = comEscavadora (escavadoraExemplo { cargaEscavadora = replicate 5 Ouro })

-- | Inválido: queda negativa (regra 2f)
quedaNegativa :: Jogo
quedaNegativa = comEscavadora (escavadoraExemplo { quedaEscavadora = -1 })

-- | Inválido: número de dinamites negativo (regra 2f)
dinamitesNegativas :: Jogo
dinamitesNegativas = comEscavadora (escavadoraExemplo { dinamitesEscavadora = -1 })

-- | Inválido: a voar com queda 2 (regra 2g)
voarComQueda :: Jogo
voarComQueda = comEscavadora (escavadoraExemplo { voandoEscavadora = True, quedaEscavadora = 2 })

-- | Válido: a voar com queda 0 (regra 2g)
voarSemQueda :: Jogo
voarSemQueda = comEscavadora (escavadoraExemplo { voandoEscavadora = True })

-- | Os testes da Tarefa 1, que são avaliados pelos docentes
testesTarefa1 :: [Jogo]
testesTarefa1 = [jogoExemplo, jogoPequeno, semCombustivel,
                escavadoraEmTerra, creditosNegativos, superficieComTerra,
                edificioForaDaSuperficie, edificioForaDoMapa, edificiosSobrepostos,
                semOficina, doisPostos, mapaVazio,
                mapaNaoRetangular, lojaVazia, precoNegativo,
                melhoriaSemValor, dinamiteEmTerra, dinamiteNaEscavadora,
                dinamiteTempoNegativo, voarSemqueda, voarComQueda,
                dinamitesNegativas, quedaNegativa, cargaNoLimite,
                cargaAcimaDaCapacidade, brocaZero, capacidadeZero,
                depositoZero, vidaNegativa, combustivelAcimaDoDeposito,
                escavadoraForaDoMapa]



-- | Corre os testes e abre a página de feedback. Não alterar.
main :: IO ()
main = do
  resultados <- mapM (runTest . validaJogo) testesTarefa1
  runFeedback (T1 testesTarefa1 resultados)
