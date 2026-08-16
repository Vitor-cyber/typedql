[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/yBGNOhhi)

# TypedQL

SGBD relacional embarcado em Haskell onde o esquema das tabelas vive no nivel de
tipos. Projeto da disciplina Desenvolvimento Orientado a Tipos (UFABC).

Pergunta do projeto: em um banco de dados tradicional quase todo erro e de
runtime (coluna inexistente, tipo incompativel, NULL inesperado, plano fisico
malformado). Quantos desses o sistema de tipos do Haskell elimina antes de
rodar, e a que custo?

## Entrega

| O que | Onde |
|---|---|
| Relatorio, 1 pagina em PDF | [`docs/relatorio.pdf`](docs/relatorio.pdf) |
| Video da apresentacao, 178 s | <https://youtu.be/kNeXjRnFuDk> |
| Codigo da biblioteca, 8 modulos | [`src/TypedQL/`](src/TypedQL) |
| Consultas que NAO devem compilar | [`negativos/`](negativos) |
| Diario de desenvolvimento | [`docs/diario.md`](docs/diario.md) |

## Uso

```
stack run                                  # demonstracao dos 8 modulos
stack test                                 # 90 testes de valor
python scripts/testar_negativos.py         # 14 consultas que NAO devem compilar
```

Requer Stack (testado na 3.11.1) e Python 3. O GHC 9.6.7 e o snapshot
`lts-22.44` sao baixados pelo proprio `stack` na primeira execucao, o que leva
alguns minutos. Nao ha dependencia fora de `base`, `containers` e `text`.

O `stack run` imprime a demonstracao dos oito modulos e termina. Se o terminal
ficar preso depois do build, o executavel pode ser chamado direto:
`stack exec typedql`.

## Por onde ler

O argumento do projeto esta em tres arquivos, nesta ordem:

1. [`src/TypedQL/Schema.hs`](src/TypedQL/Schema.hs) - o esquema como tipo, e os singletons escritos a mao.
2. [`negativos/AbsorcaoEmLeftJoin.hs`](negativos/AbsorcaoEmLeftJoin.hs) - uma reescrita de otimizador que e valida em `INNER JOIN` e errada em `LEFT JOIN`, rejeitada pelo tipo sem nenhuma verificacao escrita.
3. [`src/TypedQL/Engine.hs`](src/TypedQL/Engine.hs) - o construtor `HashJoin` exige chave `NotNull` nos dois lados, porque em SQL `NULL = NULL` nao e verdadeiro.

## Os testes negativos

A garantia central do projeto e negativa: certas consultas nao existem como
programa valido. `scripts/testar_negativos.py` compila cada arquivo de
`negativos/`, falha se algum compilar e tambem confere se a mensagem do GHC e a
esperada (um import errado tambem faria a compilacao falhar). Saida atual:

```
[OK] AcessoAColunaInexistente.hs  TypedQL: a coluna "taxa" nao existe neste esquema.
                                  Colunas disponiveis: ["vendor_code", "open_rate"]
[OK] ColunaInexistente.hs         TypedQL: a coluna "vendor_cod" nao existe neste
                                  esquema. Colunas disponiveis: ["vendor_code",
                                  "vendor_name", "open_rate", "defeitos"]
[OK] ColunaNulavelSemMaybe.hs     Couldn't match type 'Text' with 'Maybe Text'
                                  Expected: Slot ("cnpj" :? TText)
[OK] FiltroNulavel.hs             TypedQL: este filtro pode ser NULL, entao ele nao
                                  decide nada. Um WHERE precisa escolher entre
                                  verdadeiro e falso. Trate o NULL antes, com
                                  EIsNull ou ECoalesce.
[OK] JuncaoAmbigua.hs             TypedQL: juncao ambigua, a coluna "vendor_code"
                                  aparece nos dois lados.
[OK] LeituraNulavelSemMaybe.hs    Couldn't match type 'Maybe Text' with 'Text'
                                  Actual: Slot (ColumnOf "cnpj" Vendors)
[OK] TipoErrado.hs                Couldn't match type 'TDouble' with 'TInt'
[OK] EvalSemCompilar.hs           TypedQL: esta consulta ainda nao foi compilada.
                                  Aplique compile antes de executar.
[OK] ProjecaoSqlInexistente.hs    TypedQL: a coluna "taxa" nao existe neste
                                  esquema. (via quasiquoter: o SQL gera project
                                  @'["taxa"] e o GHC rejeita.)
[OK] AcessoColunaDoExistencial.hs Could not deduce ... ColumnOf "vendor_code" s
                                  ~ T.Text (s e universal no withSomeTable;
                                  acesso tipado por nome exige esquema concreto.)
[OK] AbsorcaoEmLeftJoin.hs        Couldn't match type 'Nullable' with 'NotNull'
                                  (absorver o WHERE no ON e valido em INNER JOIN e
                                  errado em LEFT JOIN; aqui a reescrita errada nem
                                  typecheca, porque o predicado do LEFT JOIN vive
                                  no esquema nao-nulavel do lado direito.)
[OK] OtimizadorMudaEsquema.hs     Could not deduce s1 ~ s, from s ~ Project ns s1
                                  (uma reescrita de plano que descarta a projecao
                                  mudaria o esquema do resultado; o tipo de
                                  QueryF r s -> r s proibe.)
[OK] HashJoinChaveNulavel.hs      Couldn't match type 'Nullable' with 'NotNull'
                                  (hash join com chave que aceita NULL: em SQL
                                  NULL = NULL nao e verdadeiro, entao o operador
                                  devolveria linhas que nao existem.)
[OK] HashJoinChavesIncompativeis  Couldn't match type 'TInt' with 'TText'
                                  (sondar uma tabela de hash de Text com chave
                                  Int; o construtor usa o mesmo t nas duas.)
```

14 de 14 rejeitados corretamente.

## Estado

- [x] Modulo 1: Schema (esquema no nivel de tipos, singletons a mao)
- [x] Modulo 2: Row (lista heterogenea indexada pelo esquema, acesso por prova)
- [x] Modulo 3: Expr (expressoes tipadas, nulabilidade calculada, WHERE total)
- [x] Modulo 4: Algebra (algebra relacional com estagios)
- [x] Modulo 5: Frontend estatico (quasiquoter [sql| ... |] que gera a consulta tipada em compile time)
- [x] Modulo 6: Frontend dinamico (existencial SomeTable/SomeResult, singleton All Show s, runDynSQL)
- [x] Modulo 7: Optimize (catamorfismo indexado de reescrita, contrato de esquema no tipo)
- [x] Modulo 8: Engine (operadores fisicos indexados, hash join com chave NotNull exigida pelo tipo, EXPLAIN, modelo de custo)
