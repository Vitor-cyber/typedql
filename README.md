# TypedQL

SGBD relacional embarcado em Haskell onde o esquema das tabelas vive no nivel de
tipos. Projeto da disciplina Desenvolvimento Orientado a Tipos (UFABC).

Pergunta do projeto: em um banco de dados tradicional quase todo erro e de
runtime (coluna inexistente, tipo incompativel, NULL inesperado, plano fisico
malformado). Quantos desses o sistema de tipos do Haskell elimina antes de
rodar, e a que custo?

## Uso

```
stack run                                  # demonstracao dos modulos ja feitos
stack test                                 # testes de valor e de tipo
python scripts/testar_negativos.py         # consultas que NAO devem compilar
```

Requer Stack 3.11.1 e GHC 9.6.7 (snapshot lts-22.44). O `stack` baixa o GHC
sozinho na primeira execucao.

### Os testes negativos

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
```

12 de 12 rejeitados corretamente.

## Estado

- [x] Modulo 1: Schema (esquema no nivel de tipos, singletons a mao)
- [x] Modulo 2: Row (lista heterogenea indexada pelo esquema, acesso por prova)
- [x] Modulo 3: Expr (expressoes tipadas, nulabilidade calculada, WHERE total)
- [x] Modulo 4: Algebra (algebra relacional com estagios)
- [x] Modulo 5: Frontend estatico (quasiquoter [sql| ... |] que gera a consulta tipada em compile time)
- [x] Modulo 6: Frontend dinamico (existencial SomeTable/SomeResult, singleton All Show s, runDynSQL)
- [x] Modulo 7: Optimize (catamorfismo indexado de reescrita, contrato de esquema no tipo)
- [ ] Modulo 8: Engine (executor indexado)
