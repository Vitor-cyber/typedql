# Diario de bordo

Anotacoes por modulo. Este arquivo e o rascunho do relatorio de 1 pagina e do
roteiro do video. Cada secao responde tres perguntas: qual problema aparece, por
que a tecnica escolhida resolve, e o que doeu.

## Modulo 1: Schema

### O problema
Um banco de dados precisa saber quais colunas uma tabela tem e de que tipo cada
uma e. Em Python ou em SQL cru essa informacao existe apenas em runtime: se voce
escrever `vendor_cod` em vez de `vendor_code`, descobre quando o programa
quebra, ou pior, quando o resultado sai errado e ninguem nota.

A pergunta do modulo: e possivel guardar o esquema **no tipo** da tabela, de modo
que o compilador recuse a consulta errada?

### A solucao e por que ela funciona
1. `data SqlType = TInt | TDouble | TText | TBool` com `DataKinds`. A extensao
   promove esse tipo a um kind, e os construtores a tipos. Passamos a poder
   escrever `'TInt` em posicao de tipo.
2. `data Column = Symbol := SqlType`. Promovido, `':=` tem kind
   `Symbol -> SqlType -> Column`. Um esquema e so `[Column]`, uma lista de tipos.
3. `Interp :: SqlType -> Type` e uma type family que traduz o kind para o tipo
   Haskell de verdade (`'TInt` vira `Int`). E o unico ponto de contato entre o
   esquema e os dados. Declarada como familia injetiva (`= r | r -> t`), o que
   permite ao GHC inferir o tipo SQL a partir do tipo Haskell.
4. `Lookup` e `TypeOf` sao familias fechadas que fazem busca na lista de tipos.
   Fechada e importante: as equacoes sao testadas em ordem, logo a sobreposicao
   entre "a cabeca casa" e "a cabeca nao casa" e permitida. Em familia aberta
   isso seria rejeitado.
5. `TypeError` transforma a falha de busca em uma mensagem legivel, listando as
   colunas disponiveis. Sem isso o erro do GHC seria um `Lookup` nao reduzido.
6. `All :: (Type -> Constraint) -> Schema -> Constraint` usa `ConstraintKinds`:
   a familia devolve uma restricao, nao um tipo. E isso que vai permitir derivar
   `Show` para uma linha inteira sem conhecer o esquema.
7. Singletons a mao (`SSqlType`) porque o esquema de um CSV so aparece em
   runtime. `demote` e a reflexao (tipo para valor) e `parseSqlType` e a
   reificacao (valor para tipo). O tipo reificado nao pode escapar da assinatura,
   entao ele fica preso em um existencial `SomeSqlType`.

### Dificuldades e surpresas
- Padroes nao lineares (a variavel `n` repetida em
  `Lookup n ((n ':= t) ': _)`) sao permitidos em type family, ao contrario do que
  vale para funcao comum. Isso deixa `Lookup` bem mais curto do que a versao com
  `If` e comparacao de simbolos.
- `Symbol` tem kind `Type`, e e por isso que `data Column = Symbol := SqlType`
  compila apesar de nao existir nenhum valor de tipo `Symbol`. O tipo de dados so
  interessa promovido, os valores nunca sao construidos.
- Sem `StandaloneKindSignatures` as assinaturas de kind ficariam espalhadas e
  ilegiveis. Com elas, cada familia se le como uma funcao comum.

### Descoberta importante (vale para o relatorio)
Uma type family **nao reduz se ninguem consome o resultado**. A primeira versao do
teste negativo era assim:

```haskell
consulta :: Proxy (Project '["vendor_cod"] Vendors)
consulta = Proxy
```

e **compilou sem erro**, apesar de a coluna nao existir. O motivo: `Proxy` nunca
olha para dentro do tipo, entao o GHC nao precisa reduzir `Project`, e portanto o
`TypeError` nunca e alcancado. A versao correta forca a reducao:

```haskell
consulta :: All Show (Project '["vendor_cod"] Vendors) => Bool
```

Agora `All` percorre a lista e pede `Show (Interp t)` para cada coluna, o que
obriga a reduzir `TypeOf`, e o erro aparece com a mensagem customizada.

A licao geral: garantia no nivel de tipos so vale se algo no programa realmente
consumir o tipo. Uma prova que ninguem olha nao e uma prova.

### Nota de infraestrutura
Os instaladores do Stack e do Git via winget reportaram sucesso mas nao
escreveram nada em disco (bloqueio de endpoint protection em maquina
corporativa). A solucao foi usar os binarios portateis, extraidos em
`C:\Users\marvitox\haskell`. Vale registrar porque afeta a reprodutibilidade:
o projeto compila com Stack 3.11.1 e GHC 9.6.6, snapshot lts-22.43.
