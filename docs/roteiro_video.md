# Roteiro do video (YouTube nao listado, 180s)

Gravar com OBS ou Xbox Game Bar (Win+G). Duas janelas na tela: terminal a
esquerda, VS Code a direita. Fonte do terminal em 16pt no minimo.

## Preparar antes de gravar

```
cd C:\Users\marvitox\Documents\typedql
```

Deixar abertos em abas do VS Code, nesta ordem:
1. `negativos/AbsorcaoEmLeftJoin.hs`
2. `negativos/HashJoinChaveNulavel.hs`
3. `src/TypedQL/Engine.hs` (rolado ate o construtor `HashJoin`)

Comando de compilar um negativo (deixar no historico do terminal, seta pra cima):

```
stack exec -- ghc -fno-code -package typedql -hide-all-packages -package base -package text -package template-haskell -outputdir .stack-work/negativos negativos/AbsorcaoEmLeftJoin.hs
```

Rodar a demo uma vez antes, para o build estar quente e nao gastar tempo no video.

---

## 0:00 - 0:20 | Abertura

**Tela:** README.md aberto no paragrafo "Pergunta do projeto".

> Em um banco de dados tradicional, quase todo erro e de runtime: coluna que nao
> existe, tipo incompativel, NULL inesperado, plano fisico malformado. TypedQL e
> um SGBD relacional embarcado em Haskell onde o esquema da tabela vive no nivel
> de tipos. A pergunta e: quantos desses erros o compilador elimina antes de
> rodar, e a que custo?

## 0:20 - 0:45 | Funciona de verdade

**Tela:** terminal, rodar `stack test`. Enquanto passa, mostrar o final.

> Oito modulos, duas mil linhas de biblioteca. Noventa testes de valor passando,
> zero aviso com Wall e Wextra. Isso e a parte comum. A parte interessante sao os
> quatorze arquivos que **nao** compilam de proposito, e por que nao compilam.

## 0:45 - 1:35 | Achado 1: uma regra de SQL que virou erro de tipo

**Tela:** aba `AbsorcaoEmLeftJoin.hs`. Apontar as duas assinaturas com o cursor.

> Um otimizador de banco costuma empurrar o WHERE de cima para dentro da condicao
> ON da juncao. Isso e valido em INNER JOIN e **errado** em LEFT JOIN, porque muda
> quais linhas recebem preenchimento nulo. E um bug classico de otimizador.
>
> Aqui eu nao escrevi nenhuma verificacao proibindo isso. Olha os tipos: a
> condicao do ON fala de `Append A B`, o lado direito real. O filtro de cima fala
> de `Append A (MakeNullable B)`, porque depois do LEFT JOIN tudo do lado direito
> pode ser nulo. Sao dois tipos diferentes.

**Acao:** rodar a compilacao desse arquivo. Deixar o erro na tela e ler:

```
Couldn't match type `Nullable' with `NotNull'
  Expected: Expr (Append l0 r0) TBool NotNull
    Actual: Predicate (Append A (MakeNullable B))
* In the second argument of `EAnd', namely `filtroDeCima'
```

> A reescrita errada nao existe como programa valido. A regra de semantica
> relacional caiu fora sozinha, pela nulabilidade.

## 1:35 - 2:20 | Achado 2: hash join com chave nulavel nao e construivel

**Tela:** `src/TypedQL/Engine.hs`, no construtor `HashJoin`. Destacar as
constraints `ColumnOf kl l ~ Col kl t NotNull` e `ColumnOf kr r ~ Col kr t NotNull`.

> Em SQL, `NULL = NULL` nao e verdadeiro. Uma linha com chave nula nunca casa com
> nada. Mas um hash join ingenuo joga todos os NULL no mesmo balde e casa todos
> com todos, devolvendo linhas que nao existem no resultado correto. Silencioso, e
> so aparece nos dados.
>
> O construtor HashJoin exige, no tipo, que a chave seja NotNull nos **dois**
> lados, e que as duas tenham o mesmo tipo SQL.

**Acao:** trocar para a aba `HashJoinChaveNulavel.hs`, mostrar que `a_cnpj` usa
`:?` em vez de `:=`, e compilar. Ler o erro:

```
negativos\HashJoinChaveNulavel.hs:29:3: error: [GHC-18872]
    * Couldn't match type `Nullable' with `NotNull'
        arising from a use of `hashJoin'
```

> Nao e um teste que roda e falha. E um operador que nao tem representacao.

## 2:20 - 2:45 | Custo

**Tela:** terminal, rodar a demo (`.stack-work/dist/.../typedql.exe`), na secao
do modulo 8.

> O EXPLAIN mostra o algoritmo escolhido, nao o SQL equivalente. Com cinquenta
> linhas de cada lado: laco aninhado, duas mil e quinhentas comparacoes. Hash
> join, cem. E os dois devolvem as mesmas linhas na mesma ordem.

## 2:45 - 3:00 | Fecho honesto

**Tela:** `src/TypedQL/Engine.hs`, na linha `M.fromListWith (flip (++))`.

> E o que o tipo **nao** pega. Esse `flip` aqui e a diferenca entre a saida do
> hash join casar ou nao com a do laco aninhado, e nenhum tipo reclamaria: o
> esquema nao diz nada sobre ordem de linhas. Ou seja, o sistema de tipos elimina
> a classe estrutural de erros. A classe observacional continua sendo trabalho de
> teste.

---

## Notas de gravacao

- Falar devagar; o texto acima tem cerca de 430 palavras, o que da os 180s. Se
  estourar, cortar a secao 0:20-0:45 (o `stack test`) e citar o numero de cabeca.
- Nao gravar o build a frio: leva 4 minutos.
- Se o terminal ficar apertado, compilar os negativos pelo script
  `python scripts/testar_negativos.py` e mostrar so as duas linhas relevantes.
