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
o projeto compila com Stack 3.11.1 e GHC 9.6.7, snapshot lts-22.44.

## Modulo 2: Row

### O problema
O modulo 1 descreve o esquema, mas nao guarda dado nenhum. Falta a linha. Uma
linha tem um problema chato: cada posicao tem um tipo diferente. Uma lista comum
`[a]` exige que todos os elementos tenham o mesmo tipo, e a saida usual em
linguagem sem tipos dependentes e usar um dicionario de `Any` ou uma uniao
(`Value = VInt Int | VText Text | ...`), o que devolve o problema para o runtime:
todo acesso vira um `case` com um caso de falha.

A pergunta do modulo: e possivel uma lista heterogenea cujo tipo de cada posicao
seja ditado pelo esquema, e um acesso por nome de coluna que nao tenha caso de
falha?

### A solucao e por que ela funciona
1. `Row s` e um GADT indexado pelo esquema:

   ```haskell
   data Row s where
     RNil  :: Row '[]
     RCons :: Interp t -> Row s -> Row ((n := t) : s)
   ```

   Cada `RCons` carrega um valor de tipo `Interp t`, ou seja, o tipo Haskell que
   o esquema manda. Trocar a ordem das colunas ou o tipo de uma delas nao
   compila. E a mesma forma do `Stack` da lista 07: o indice do tipo cresce junto
   com a estrutura.

2. Acesso por nome vira uma **prova**, nao uma busca:

   ```haskell
   data Index n t s where
     Here  :: Index n t ((n := t) : s)
     There :: Index n t s -> Index n t (c : s)
   ```

   `Index n t s` e um valor que so pode ser construido se a coluna `n`, de tipo
   `t`, realmente estiver em `s`. `getAt` consome essa prova andando na linha, e
   **nao tem caso de falha**: os dois padroes cobrem tudo, porque a prova diz que
   a coluna existe. Ausencia de coluna deixa de ser um erro de runtime e passa a
   ser um programa que nao existe.

3. `KnownIndex` constroi a prova por inducao nas instancias: a instancia base
   casa quando a cabeca do esquema e a coluna procurada, a instancia
   `OVERLAPPABLE` anda uma posicao e recorre. E a mesma tecnica de `KnownNat`,
   com a resolucao de instancias fazendo o papel da recursao.

4. `col @"open_rate" linha` esconde tudo isso. Custo em runtime: zero, a prova e
   apagada na compilacao (o codigo gerado e o mesmo de um acesso posicional).

5. `SSchema` e o singleton do esquema inteiro, necessario para percorrer uma
   linha generica: o esquema foi apagado pelo compilador, o singleton e a copia
   que sobrevive. `header` reflete o esquema de volta para valores comuns
   (`[(String, SqlType)]`), e `showRow` usa a restricao `All Show` do modulo 1
   para renderizar a linha sem enumerar as colunas.

6. `SomeRow` + `withRow` fecham o modulo com o existencial, preparando o modulo 6
   (esquema descoberto na leitura do CSV).

### Dificuldades e surpresas
- A primeira versao de `Index` tinha assinatura
  `getAt :: Index n s -> Row s -> Interp (TypeOf n s)`, sem o `t` no indice. Nao
  compila: no caso recursivo o GHC tem um esquema abstrato `c : s` e nao sabe
  decidir se `c` e ou nao a coluna procurada, entao as duas equacoes de `Lookup`
  ficam empatadas e a familia nao reduz. A correcao foi **carregar o tipo na
  prova** (`Index n t s`). Licao geral: quando a familia de tipos travar, mova a
  informacao para o indice do GADT. A prova sabe o que a familia teria que
  calcular.
- A dependencia funcional `| n s -> t` e o que permite escrever
  `col @"open_rate" linha` sem anotar o tipo do resultado. Sem ela `t` fica
  ambiguo, porque nao aparece nos argumentos.
- `AllowAmbiguousTypes` e necessario em `col` porque `n` so aparece na restricao.
  `TypeApplications` e o que torna a funcao usavel.
- Dois tropecos de compilacao que nada tem a ver com teoria de tipos, mas valem
  registro: `Text` de `Data.Text` colide com o construtor `Text` de
  `ErrorMessage` (em posicao de tipo o GHC prefere o tipo, entao a mensagem de
  erro quebra); e `-Wall` mais assinatura de kind autonoma exigem `RankNTypes`
  explicito, porque nao ha default-extensions no `package.yaml`.

### Testes negativos que passaram a existir
`negativos/AcessoAColunaInexistente.hs` pede `col @"taxa"` num esquema que so tem
`vendor_code` e `open_rate`. O GHC responde
`No instance for (KnownIndex "taxa" TText '[])`: a busca por inducao chegou ao
fim da lista sem achar. O script `scripts/testar_negativos.py` agora confere
tambem **a mensagem**, nao so o fato de a compilacao falhar, porque um import
errado tambem faria o teste passar por acidente.
