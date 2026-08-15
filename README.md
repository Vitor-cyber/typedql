# TypedQL

SGBD relacional embarcado em Haskell onde o esquema das tabelas vive no nivel de
tipos. Projeto da disciplina Desenvolvimento Orientado a Tipos (UFABC).

Pergunta do projeto: em um banco de dados tradicional quase todo erro e de
runtime (coluna inexistente, tipo incompativel, NULL inesperado, plano fisico
malformado). Quantos desses o sistema de tipos do Haskell elimina antes de
rodar, e a que custo?

## Uso

```
stack run                                  # demonstracao do modulo atual
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
[OK] AcessoAColunaInexistente.hs  No instance for (KnownIndex "taxa" TText '[])
[OK] ColunaInexistente.hs         TypedQL: a coluna "vendor_cod" nao existe neste
                                  esquema. Colunas disponiveis: ["vendor_code",
                                  "vendor_name", "open_rate", "defeitos"]
[OK] JuncaoAmbigua.hs             TypedQL: juncao ambigua, a coluna "vendor_code"
                                  aparece nos dois lados.
[OK] TipoErrado.hs                Couldn't match type 'TDouble' with 'TInt'
```

4 de 4 rejeitados corretamente.

## Estado

- [x] Modulo 1: Schema (esquema no nivel de tipos, singletons a mao)
- [x] Modulo 2: Row (lista heterogenea indexada pelo esquema, acesso por prova)
- [ ] Modulo 3: Expr (expressoes tipadas com nulabilidade)
- [ ] Modulo 4: Algebra (algebra relacional com estagios)
- [ ] Modulo 5: Frontend estatico (quasiquoter)
- [ ] Modulo 6: Frontend dinamico (existenciais e singletons)
- [ ] Modulo 7: Optimize (catamorfismo)
- [ ] Modulo 8: Engine (executor indexado)
