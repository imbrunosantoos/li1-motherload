# Laboratórios de Informática / Programação I — *Motherload*

Grupo 92:

- Adriano Alejandro Pino Ferreira <a110205@alunos.uminho.pt>
- Bruno Ribeiro Souza Santos <a113332@alunos.uminho.pt>

## Interpretador

Para abrir o interpretador do Haskell (GHCi) com o projeto carregado, utilize o comando `repl` do Cabal

```bash
cabal repl
```

## Executar o Projeto

O jogo está na pasta `app/`: o estado do jogo em `Motherload.hs`, o desenho em `Desenhar.hs`, a reação ao teclado e ao rato em `Eventos.hs` e a passagem do tempo em `Tempo.hs`. Para o correr, utilize o comando `run` do Cabal

```bash
cabal run motherload-game
```

O jogo usa a biblioteca [Gloss](https://hackage.haskell.org/package/gloss), que precisa do freeglut do sistema operativo. No Ubuntu, por exemplo, instala-se com o comando:

```bash
sudo apt install -y freeglut3-dev
```

A partir do Ubuntu 24.04, o pacote chama-se `libglut-dev`. O `freeglut3-dev` continua a funcionar, mas só serve para instalar o `libglut-dev`.

O projeto inclui também a biblioteca [gloss-relative](https://hackage.haskell.org/package/gloss-relative), que pode usar se quiser.

## Testes e Feedback

A plataforma de feedback associada ao projecto fornece também a funcionalidade de correr testes unitários sobre as várias tarefas.
Para isso, o projeto contém vários executáveis, `t<N>-feedback`, em que `N` é o número da tarefa. Por exemplo, para a Tarefa 1 o executável é `t1
-feedback`.

Pode correr cada executável, por exemplo para a Tarefa 1, pode utilizar o comando:

```bash
cabal run t1-feedback
```

## Cobertura dos Testes

Ao executar os testes para uma tarefa, pode também medir a cobertura dos mesmos, ou seja, quanto do código da tarefa é utilizado na resolução dos testes.
Para isso, podem utilizar o script `bin/coverage`, por exemplo para a Tarefa 1:

```bash
./bin/coverage t1
```

## Documentação

A documentação do projeto pode ser gerada recorrendo ao [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```

## Exemplos da Documentação

Os exemplos `>>>` dos comentários das funções podem ser corridos como testes com o [doctest](https://hackage.haskell.org/package/doctest). Instale-o uma vez com o comando:

```bash
cabal install --ignore-project doctest --flag cabal-doctest
```

Depois, para correr os exemplos do projeto:

```bash
cabal doctest
```

## Qualidade do Código

Um dos eixos que devem ter em consideração no desenvolvimento do vosso projeto é a qualidade do código, que se prende com questões como a estrutura, modularidade, tamanho e documentação das funções, etc. Uma ferramenta que apoia na análise da qualidade do vosso código é o homplexity, que podem instalar com o seguinte comando:

```
cabal install homplexity --flags="html" --allow-newer
```

Para correr a ferramenta para o vosso projeto e gerar uma página web com um relatório podem executar o comando:

```
homplexity-cli --format=HTML lib/ > homplexity.html

