# TypedQL: um SGBD relacional cujo esquema vive no nivel de tipos

**Vitor Martins Bueno Lima** | Desenvolvimento Orientado a Tipos | UFABC | 2026 | **Video (178 s):** <https://youtu.be/kNeXjRnFuDk>

## Pergunta

Em um banco de dados tradicional quase todo erro e de runtime: coluna
inexistente, tipo incompativel, `NULL` inesperado, plano fisico malformado.
**Quantos desses o sistema de tipos do Haskell elimina antes de rodar, e a que
custo?** TypedQL e a resposta empirica: 2.084 linhas, GHC 9.6.7, sem
dependencia alem de `base`, `containers` e `text`.

## Construcao

O esquema de uma tabela e um tipo: `Vendors = '[ "vendor_code" := TText, "cnpj" :? TText ]`.
Toda a pilha e indexada por ele.

| Camada | Modulo | Ideia |
|---|---|---|
| Dados | `Schema`, `Row` | lista heterogenea indexada; acesso por *prova* `KnownIndex` |
| Consulta | `Expr`, `Algebra` | nulabilidade **calculada** por type family; estagio `Logical`/`Physical` no tipo |
| Superficie | `Frontend.Static`, `.Dynamic` | quasiquoter de SQL em compile time; existencial `SomeTable` + singleton |
| Plano | `Optimize`, `Engine` | catamorfismo indexado de reescrita; operadores fisicos; EXPLAIN e custo |

Um unico combinador, `hcata :: (forall x. QueryF g x -> g x) -> QueryNode s -> g s`,
serve para otimizar, contar nos e escolher algoritmo fisico. A algebra do plano
foi escrita uma vez.

## Resultado: 14 erros que deixaram de existir

`scripts/testar_negativos.py` compila cada arquivo de `negativos/` e falha se
algum compilar, conferindo tambem a mensagem do GHC. Estado: **14 de 14
rejeitados**, ao lado de 90 testes de valor passando e zero aviso sob
`-Wall -Wextra`. Sete dos quatorze produzem mensagem escrita por mim via
`TypeError`, nao ruido do compilador:

> `TypedQL: este filtro pode ser NULL, entao ele nao decide nada. Um WHERE`
> `precisa escolher entre verdadeiro e falso. Trate o NULL antes.`

## Os dois achados

**1. Uma reescrita de otimizador errada em SQL virou erro de tipo.** Absorver o
`WHERE` de cima na condicao `ON` e valido em `INNER JOIN` e **incorreto** em
`LEFT JOIN`, porque muda quais linhas recebem preenchimento nulo. Nao foi
preciso proibir: o predicado do `LJoin` tem tipo `Predicate (Append l r)` e o
filtro de cima tem `Predicate (Append l (MakeNullable r))`. Os dois nao unificam.
A regra de semantica relacional caiu fora do espaco de programas validos sem que
eu escrevesse uma verificacao.

**2. Hash join com chave nulavel nao e construivel.** Em SQL `NULL = NULL` nao e
verdadeiro. Um hash join ingenuo joga todos os `NULL` no mesmo balde e casa todos
com todos, devolvendo linhas que nao existem no resultado correto: um bug
silencioso e classico. O construtor `HashJoin` exige
`ColumnOf kl l ~ Col kl t NotNull` nos **dois** lados, com o mesmo `t`. O
operador errado nao tem representacao.

## Custo, medido e nao estimado

Com 50 linhas de cada lado, o laco aninhado faz 2.500 comparacoes e o hash join
faz 100, e os dois devolvem as mesmas linhas na mesma ordem. O custo de
*programar* foi outro. Tres registros honestos:

- **Type family que nao reduz.** Uma familia so avalia se alguem consome o
  resultado. `Proxy` nao dispara `TypeError`; foi preciso forcar com uma
  constraint real (`All Show`).
- **`Append` nao e injetiva.** Dois negativos de hash join passavam pelo motivo
  errado (ambiguidade, nao incompatibilidade) ate eu anotar `scan "a" [] :: PhysOp A`.
  Testar um teste negativo tambem e necessario.
- **Limite que ficou.** Promover `ON (a = b)` a hash join automaticamente nao
  typecheca: a prova que `ECol` carrega fala de `Append l r`, e `HashJoin` quer
  uma prova em cada lado. `Append` apagou de qual lado a coluna veio. A correcao
  seria um `Predicate2 l r`, reescrita do modulo de expressoes.

## Conclusao

A tese se sustenta com uma ressalva. Erros de nome, de tipo, de nulabilidade, de
ordem de estagio e de preservacao de esquema foram eliminados por construcao. Mas
o tipo nao captura ordem de linhas: o `flip` em `M.fromListWith (flip (++))` e a
diferenca entre a saida do hash join casar ou nao com a do laco aninhado, e
nenhum tipo reclamaria. **O sistema de tipos elimina a classe estrutural de
erros; a observacional continua sendo trabalho de teste.** Reproduzir:
`stack test` e `python scripts/testar_negativos.py`. Video: <https://youtu.be/kNeXjRnFuDk>
