# TypedQL

SGBD relacional embarcado em Haskell onde o esquema das tabelas vive no nivel de
tipos. Projeto da disciplina Desenvolvimento Orientado a Tipos (UFABC).

Pergunta do projeto: em um banco de dados tradicional quase todo erro e de
runtime (coluna inexistente, tipo incompativel, NULL inesperado, plano fisico
malformado). Quantos desses o sistema de tipos do Haskell elimina antes de
rodar, e a que custo?

## Uso

```
stack run
stack test
```

## Estado

- [x] Modulo 1: Schema (esquema no nivel de tipos, singletons a mao)
- [ ] Modulo 2: Row (lista heterogenea indexada pelo esquema)
- [ ] Modulo 3: Expr (expressoes tipadas com nulabilidade)
- [ ] Modulo 4: Algebra (algebra relacional com estagios)
- [ ] Modulo 5: Frontend estatico (quasiquoter)
- [ ] Modulo 6: Frontend dinamico (existenciais e singletons)
- [ ] Modulo 7: Optimize (catamorfismo)
- [ ] Modulo 8: Engine (executor indexado)
