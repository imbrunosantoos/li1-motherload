{-# OPTIONS_GHC -w #-}

{-|
Module      : Magic
Description : Magia para correr o feedback.
Copyright   : Henrique Faria <d14410@di.uminho.pt>
              Emanuel Rodrigues <d14960@di.uminho.pt>
              Nelson Estevão <d12733@di.uminho.pt>
              Olga Pacheco <omp@di.uminho.pt>
              Sérgio Alves <sfa@di.uminho.pt>
              Xavier Pinho <d12736@di.uminho.pt>

Este ficheiro contém /magia/ para correr o feedback. Não tem nada de
útil para o projeto. Ignorem o mais que puderem!
-}
module Magic
  ( -- * Feedback
    Error
  , TaskData (..)
  , taskNumber
  , runTest
  , runFeedback

    -- * Codificação
  , Bits
  , encToBase64
  , bitsTaskData
  , bitsJogo
  , bitsJogada
  ) where

import Control.Exception (SomeException, evaluate, try)
import Data.Bits (shiftR, testBit, (.&.), (.|.))
import Data.Char (chr, isAlphaNum, ord)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitFailure)
import System.Info (os)
import System.Process (callProcess)
import System.Timeout (timeout)

import Labs2026

-- * Feedback

-- | O resultado de um teste: o valor calculado, ou a mensagem do erro que ocorreu a calculá-lo.
type Error a = Either String a

-- | Os testes de uma tarefa: os /inputs/ do grupo e o que a função do grupo devolveu para cada um.
data TaskData
  = T1 {t1_inputs :: [Jogo], t1_outputs :: [Error Bool]}
  | T2 {t2_inputs :: [(Jogada, Jogo)], t2_outputs :: [Error Jogo]}
  | T3 {t3_inputs :: [Jogo], t3_outputs :: [Error Jogo]}
  deriving (Eq, Show)

-- | O número da tarefa.
taskNumber :: TaskData -> Int
taskNumber T1 {} = 1
taskNumber T2 {} = 2
taskNumber T3 {} = 3

-- | Segundos que um teste pode demorar.
tempoLimite :: Int
tempoLimite = 2

-- | Calcula o resultado de um teste por completo, apanhando os erros (@undefined@, @error@,
-- padrões em falta, ...) e os testes que não terminam.
runTest :: Show a => a -> IO (Error a)
runTest a = do
  r <- try (timeout (tempoLimite * 1000000) (evaluate (length (show a))))
  return $ case r of
    Left e -> Left (show (e :: SomeException))
    Right Nothing -> Left ("Tempo esgotado: o teste demorou mais de " ++ show tempoLimite ++ " segundos")
    Right (Just _) -> Right a

-- | Grava os resultados em @t\<N\>.feedback@ e abre a página de feedback no browser, exceto com a
-- opção @--no-open@ (@cabal run t1-feedback -- --no-open@).
--
-- __NB:__ Se o /input/ de algum teste der erro não há nada para mostrar: indica qual é e termina.
runFeedback :: TaskData -> IO ()
runFeedback dta = do
  abre <- abreBrowserOpcao
  verificaInputs dta
  pagina <- fromMaybe paginaFeedback <$> lookupEnv "LI1_FEEDBACK_URL"
  let ficheiro = "t" ++ show (taskNumber dta) ++ ".feedback"
      dados = encToBase64 (bitsTaskData dta)
      uri = pagina ++ "#flags=" ++ concatMap codificaURI dados
      upload = "faça upload do ficheiro " ++ show ficheiro ++ " em \x1b[36m" ++ pagina ++ "\x1b[0m"
  writeFile ficheiro dados
  if abre
    then do
      aberto <-
        if length uri > tamanhoMaximoURI
          then return False
          else abreBrowser uri
      if aberto
        then putStrLn "\x1b[33m*\x1b[0m Página de feedback aberta no browser."
        else putStrLn "\x1b[33m*\x1b[0m Não foi possível abrir a página de feedback no browser."
      putStrLn $ "\x1b[33m*\x1b[0m Se não conseguir visualizar " ++ upload
    else putStrLn $ "\x1b[33m*\x1b[0m Para ver o feedback " ++ upload

-- | Lê as opções da linha de comandos e devolve se deve abrir o browser. A única opção é
-- @--no-open@; com outra qualquer, mostra o uso e termina.
abreBrowserOpcao :: IO Bool
abreBrowserOpcao = do
  args <- getArgs
  case filter (/= "--no-open") args of
    [] -> return ("--no-open" `notElem` args)
    (opcao : _) -> do
      programa <- getProgName
      putStrLn ("\x1b[31m*\x1b[0m Opção desconhecida: " ++ opcao)
      putStrLn ("Uso: cabal run " ++ programa ++ " [-- --no-open]")
      exitFailure

-- | Termina com uma mensagem se o /input/ de algum teste der erro, por exemplo por ter um
-- @undefined@. Os testes são numerados a partir de 0, como na página.
verificaInputs :: TaskData -> IO ()
verificaInputs dta = do
  resultados <- mapM runTest inputs
  case [(i, e) | (i, Left e) <- zip [0 :: Int ..] resultados] of
    [] -> return ()
    ((i, e) : _) -> do
      putStrLn ("\x1b[31m*\x1b[0m O teste #" ++ show i ++ " de testesTarefa" ++ show (taskNumber dta) ++ " dá erro:")
      putStrLn e
      exitFailure
  where
    inputs = case dta of
      T1 i _ -> map show i
      T2 i _ -> map show i
      T3 i _ -> map show i

-- | A página de feedback publicada.
paginaFeedback :: String
paginaFeedback = "https://haslab.github.io/Teaching/LI1/2627/feedback/testes/"

-- | Acima deste tamanho o endereço já não cabe num argumento da linha de comandos e é preciso
-- fazer upload do ficheiro.
--
-- __NB:__ Os dados vão no fragmento (@#flags=@) e não na query (@?flags=@): o fragmento não é
-- enviado ao servidor, que recusaria um endereço tão longo.
tamanhoMaximoURI :: Int
tamanhoMaximoURI = 100000

-- | Abre um endereço no browser do sistema. Devolve se conseguiu.
abreBrowser :: String -> IO Bool
abreBrowser uri = do
  r <- try (callProcess comando (argumentos ++ [uri]))
  return $ case r of
    Left (_ :: SomeException) -> False
    Right () -> True
  where
    (comando, argumentos) = case os of
      "darwin" -> ("open", [])
      "mingw32" -> ("rundll32", ["url.dll,FileProtocolHandler"])
      _ -> ("xdg-open", [])

-- | Codifica um carácter para um endereço. Os de base64 que não são letras nem dígitos são
-- @+@, @/@ e @=@.
codificaURI :: Char -> String
codificaURI c
  | isAlphaNum c = [c]
  | otherwise = '%' : hex (ord c `div` 16) : [hex (ord c `mod` 16)]
  where
    hex n = "0123456789ABCDEF" !! n

-- * Codificação

-- | Uma sequência de bits, o mais significativo primeiro.
type Bits = [Bool]

bitsBool :: Bool -> Bits
bitsBool b = [b]

-- | Um número natural com um dado número de bits.
bitsNatural :: Int -> Int -> Bits
bitsNatural n x = [testBit x i | i <- [n - 1, n - 2 .. 0]]

-- | Um inteiro com 32 bits, em complemento para dois.
bitsInt :: Int -> Bits
bitsInt = bitsNatural 32

-- | Uma lista: o comprimento com 32 bits e depois os elementos.
bitsLista :: (a -> Bits) -> [a] -> Bits
bitsLista f xs = bitsNatural 32 (length xs) ++ concatMap f xs

bitsPar :: (a -> Bits) -> (b -> Bits) -> (a, b) -> Bits
bitsPar f g (a, b) = f a ++ g b

bitsError :: (a -> Bits) -> Error a -> Bits
bitsError _ (Left s) = False : bitsString s
bitsError f (Right a) = True : f a

-- | Uma string: o número de bytes com 32 bits e depois os bytes em UTF-8.
bitsString :: String -> Bits
bitsString s = bitsNatural 32 (length bytes) ++ concatMap (bitsNatural 8) bytes
  where
    bytes = concatMap utf8 s

-- | Os bytes de um carácter em UTF-8.
utf8 :: Char -> [Int]
utf8 c
  | n < 0x80 = [n]
  | n < 0x800 = [0xC0 .|. shiftR n 6, continua 0]
  | n < 0x10000 = [0xE0 .|. shiftR n 12, continua 6, continua 0]
  | otherwise = [0xF0 .|. shiftR n 18, continua 12, continua 6, continua 0]
  where
    n = ord c
    continua k = 0x80 .|. (shiftR n k .&. 0x3F)

bitsPosicao :: Posicao -> Bits
bitsPosicao = bitsPar bitsInt bitsInt

bitsDirecao :: Direcao -> Bits
bitsDirecao = bitsNatural 2 . fromEnum

bitsMinerio :: TipoMinerio -> Bits
bitsMinerio = bitsNatural 3 . fromEnum

bitsTipoEdificio :: TipoEdificio -> Bits
bitsTipoEdificio = bitsNatural 2 . fromEnum

bitsBloco :: Bloco -> Bits
bitsBloco b = bitsNatural 4 $ case b of
  Ar -> 0
  Terra -> 1
  Rocha -> 2
  Lava -> 3
  Minerio m -> 4 + fromEnum m

bitsEscavadora :: Escavadora -> Bits
bitsEscavadora v =
  concat
    [ bitsPosicao (posicaoEscavadora v)
    , bitsDirecao (direcaoEscavadora v)
    , bitsInt (combustivelEscavadora v)
    , bitsInt (depositoEscavadora v)
    , bitsInt (vidaEscavadora v)
    , bitsInt (vidaMaximaEscavadora v)
    , bitsInt (brocaEscavadora v)
    , bitsLista bitsMinerio (cargaEscavadora v)
    , bitsInt (capacidadeEscavadora v)
    , bitsInt (quedaEscavadora v)
    , bitsBool (voandoEscavadora v)
    , bitsInt (dinamitesEscavadora v)
    ]

bitsEdificio :: Edificio -> Bits
bitsEdificio e = bitsTipoEdificio (tipoEdificio e) ++ bitsPosicao (posicaoEdificio e)

bitsObjeto :: Objeto -> Bits
bitsObjeto o = bitsPosicao (posicaoDinamite o) ++ bitsInt (tempoDinamite o)

bitsMelhoria :: Melhoria -> Bits
bitsMelhoria m = case m of
  Deposito n -> bitsNatural 3 0 ++ bitsInt n
  Vida n -> bitsNatural 3 1 ++ bitsInt n
  Broca n -> bitsNatural 3 2 ++ bitsInt n
  Carga n -> bitsNatural 3 3 ++ bitsInt n
  Dinamites n -> bitsNatural 3 4 ++ bitsInt n

-- | Um jogo, campo a campo, pela ordem da declaração.
bitsJogo :: Jogo -> Bits
bitsJogo j =
  concat
    [ bitsLista (bitsLista bitsBloco) (mapaJogo j)
    , bitsEscavadora (escavadoraJogo j)
    , bitsLista bitsEdificio (edificiosJogo j)
    , bitsLista bitsObjeto (objetosJogo j)
    , bitsInt (creditosJogo j)
    , bitsLista (bitsPar bitsInt bitsMelhoria) (lojaJogo j)
    ]

-- | Uma jogada: o construtor com 3 bits, pela ordem da declaração, e depois o argumento.
bitsJogada :: Jogada -> Bits
bitsJogada x = case x of
  Parado -> bitsNatural 3 0
  Move d -> bitsNatural 3 1 ++ bitsDirecao d
  Vende -> bitsNatural 3 2
  Abastece -> bitsNatural 3 3
  Repara -> bitsNatural 3 4
  Compra i -> bitsNatural 3 5 ++ bitsInt i
  LargaDinamite -> bitsNatural 3 6

-- | Os dados de uma tarefa: o número da tarefa menos um com 2 bits, os /inputs/ e os resultados.
bitsTaskData :: TaskData -> Bits
bitsTaskData t = case t of
  T1 i o -> bitsNatural 2 0 ++ bitsLista bitsJogo i ++ bitsLista (bitsError bitsBool) o
  T2 i o -> bitsNatural 2 1 ++ bitsLista (bitsPar bitsJogada bitsJogo) i ++ bitsLista (bitsError bitsJogo) o
  T3 i o -> bitsNatural 2 2 ++ bitsLista bitsJogo i ++ bitsLista (bitsError bitsJogo) o

-- | Agrupa os bits em bytes, completando o último com zeros, e escreve-os em base64.
encToBase64 :: Bits -> String
encToBase64 = base64 . bytes
  where
    bytes [] = []
    bytes bs = valor (take 8 (bs ++ replicate 7 False)) : bytes (drop 8 bs)
    valor = foldl (\acc b -> 2 * acc + fromEnum b) 0

-- | Codifica uma lista de bytes em base64: cada 3 bytes dão 4 símbolos de 6 bits, e o fim é
-- completado com @=@.
base64 :: [Int] -> String
base64 bs = case bs of
  [] -> ""
  [a] -> take 2 (grupo a 0 0) ++ "=="
  [a, b] -> take 3 (grupo a b 0) ++ "="
  (a : b : c : resto) -> grupo a b c ++ base64 resto
  where
    grupo a b c = [simbolo (shiftR n k .&. 63) | k <- [18, 12, 6, 0]]
      where
        n = a * 65536 + b * 256 + c

-- | O símbolo de base64 de um número entre 0 e 63.
simbolo :: Int -> Char
simbolo n
  | n < 26 = chr (ord 'A' + n)
  | n < 52 = chr (ord 'a' + n - 26)
  | n < 62 = chr (ord '0' + n - 52)
  | n == 62 = '+'
  | otherwise = '/'
