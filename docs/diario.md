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

(Nota posterior: no modulo 3 a assinatura passou a ser
`KnownIndex "taxa" (Col "taxa" t nl) '[]`, porque a prova de pertinencia passou a
carregar a coluna inteira em vez de nome e tipo separados. A forma da mensagem
mudou, o conteudo dela nao.)

## Modulo 3: Expr

### O problema
Um SGBD erra de tres formas ao avaliar uma expressao escalar: comparando coisas de
tipos incompativeis (`WHERE nome < 3`), somando texto, e esquecendo que um valor
pode ser NULL. Os dois primeiros o modulo 1 ja resolve, porque o tipo SQL de cada
coluna esta no esquema. O terceiro e o assunto deste modulo, e e o mais
interessante dos tres, por dois motivos.

Primeiro, porque NULL em SQL nao e um valor, e um marcador de ausencia que
contamina tudo o que toca: `1 + NULL` e NULL, `x = NULL` e NULL (nao FALSE, o que
e a fonte classica de bug), e o `AND` opera em logica de tres valores. Segundo,
porque a linguagem tem um lugar onde a ausencia simplesmente nao pode acontecer: o
`WHERE`. Um filtro tem que decidir entre incluir e excluir a linha; "nao sei" nao e
uma resposta. O SQL de verdade resolve isso escondendo a decisao (trata NULL como
falso, silenciosamente). A pergunta deste modulo e se o sistema de tipos consegue
tornar essa decisao **impossivel de esquecer**.

### A solucao e por que ela funciona
A nulabilidade virou um indice do tipo da expressao:

```haskell
type Expr :: Schema -> SqlType -> Nullability -> Type
data Expr s t nl where ...
```

`Expr s t nl` se le "expressao que faz sentido no esquema `s`, produz o tipo SQL
`t`, e pode (`Nullable`) ou nao pode (`NotNull`) ser NULL". Os tres pontos que
fazem isso funcionar:

1. **A nulabilidade e calculada, nao declarada.** Os construtores binarios tem
   tipo `Expr s t n1 -> Expr s t n2 -> Expr s t (MergeNull n1 n2)`, e `MergeNull`
   e uma familia fechada de duas equacoes: `MergeNull NotNull NotNull = NotNull` e
   um curinga que devolve `Nullable`. O programador nao anota nada; ele monta a
   expressao e o compilador propaga a contaminacao. Detalhe util: por ser fechada e
   por o curinga so ser alcancado quando a primeira equacao e comprovadamente
   inaplicavel, `MergeNull Nullable nl` reduz para `Nullable` mesmo com `nl`
   desconhecido.

2. **O tipo do resultado da avaliacao tambem e calculado.**

   ```haskell
   type family Result nl t where
     Result NotNull  t = Interp t
     Result Nullable t = Maybe (Interp t)

   evalExpr :: Row s -> Expr s t nl -> Result nl t
   ```

   Isso e mais forte do que parece. Numa implementacao comum, avaliar devolveria
   sempre `Maybe`, e toda expressao pagaria alocacao de `Just` e teste de `Nothing`
   mesmo quando nenhum operando pode ser NULL. Aqui, `evalExpr` sobre uma expressao
   total devolve o valor cru: a soma de duas colunas obrigatorias e a mesma
   aplicacao de funcao que haveria em Haskell comum, sem embrulho. **A informacao de
   tipo virou otimizacao**, nao so verificacao. E o mesmo `Result` e usado no
   modulo 1 para definir o slot de uma coluna no esquema, o que garante que
   "coluna nulavel" e "expressao nulavel" nao possam divergir: ha uma unica fonte
   de verdade.

3. **O `WHERE` exige totalidade, e a exigencia tem mensagem propria.**

   ```haskell
   type Predicate s = Expr s TBool NotNull
   evalWhere :: Total nl => Row s -> Expr s TBool nl -> Bool
   ```

   `Total` e uma classe com duas instancias: `Total NotNull`, que funciona, e
   `Total Nullable`, cujo **contexto e um `TypeError`**. Essa segunda instancia
   nunca pode ser usada; ela existe apenas para o GHC ter algo especifico a dizer.
   Sem ela a mensagem seria `Couldn't match type Nullable with NotNull`, que nao
   ensina nada. Com ela:

   ```
   TypedQL: este filtro pode ser NULL, entao ele nao decide nada.
   Um WHERE precisa escolher entre verdadeiro e falso.
   Trate o NULL antes, com EIsNull ou ECoalesce.
   ```

   O truque geral vale registro: **uma instancia que existe so para falhar bem** e
   uma ferramenta de design de API, nao um hack. Ela transforma um erro de
   unificacao numa instrucao.

E preciso haver saida da nulabilidade, senao o sistema so acumula `Nullable` e o
`WHERE` fica inatingivel. Sao duas valvulas, e as duas sao honestas:
`EIsNull :: Expr s t nl -> Expr s TBool NotNull` (perguntar se algo e NULL sempre
tem resposta) e `ECoalesce :: Expr s t nl -> Expr s t NotNull -> Expr s t NotNull`
(fornecer um padrao). Note que o segundo argumento do `ECoalesce` e obrigado a ser
total: nao ha como "resolver" um NULL com outro NULL.

A logica de tres valores foi implementada de verdade, nao aproximada. `FALSE AND
NULL` e `FALSE`, porque o resultado ja esta determinado. Isso obrigou os
conectivos booleanos a receberem **duas** implementacoes:

```haskell
liftBool :: SNullability n1 -> SNullability n2
         -> (Bool -> Bool -> Bool)                    -- caso total
         -> (Maybe Bool -> Maybe Bool -> Maybe Bool)  -- caso Kleene
         -> Result n1 TBool -> Result n2 TBool -> Result (MergeNull n1 n2) TBool
```

O tipo obriga a fornecer a versao total; nao ha como despachar o caso `NotNull
NotNull` para a versao de tres valores e depois extrair com `fromJust`.

### Dificuldades e surpresas
- **A refatoracao que nao dava para evitar.** A primeira versao do modulo tentou
  colocar nulabilidade so na expressao, deixando o esquema como estava
  (`"nome" := TText`). Nao funciona: se toda coluna do esquema e obrigatoria, a
  unica forma de produzir uma expressao `Nullable` e `ENull` literal, e a
  maquinaria inteira fica decorativa. Foi preciso voltar ao modulo 1 e trocar
  `Column` de um par nome/tipo para uma tripla nome/tipo/nulabilidade, com dois
  sinonimos de operador: `n := t` para obrigatoria e `n :? t` para nulavel. Licao
  de projeto: **a propriedade tem que existir na fonte dos dados, nao so no
  consumidor**. Isso tambem obrigou a mexer no `Index` do modulo 2, que passou a
  carregar a coluna inteira (`Index c s`) em vez de nome e tipo separados.
- **A armadilha do sinonimo de tipo em posicao de padrao.** Dentro da biblioteca e
  obrigatorio casar com `Col n t nl`, nunca com `n := t`. `:=` e um sinonimo que
  expande para `Col n t NotNull`, entao um padrao escrito com ele **compila e
  silenciosamente ignora todas as colunas nulaveis**. Nao ha aviso. Custou tempo, e
  e o tipo de erro que testes de tipo positivo nao pegam (o codigo compila; ele so
  esta errado).
- **`Result` nao e injetiva, e isso se paga em verbosidade.** De `Result nl t` o
  compilador nao consegue voltar para `nl` e `t`, logo ele nao infere a
  nulabilidade a partir do valor avaliado. Cada chamada dos helpers precisa dizer
  explicitamente qual e, com aplicacao de tipos: `liftBin @n1 @n2 @u @u @TBool`. Os
  helpers ganharam `forall` explicito so para fixar a ordem dessas aplicacoes.
  Um `type family Result nl t = r | r -> nl t` resolveria? Nao: `Result NotNull
  TInt = Int` e `Result Nullable TInt = Maybe Int` sao distintos, mas a injetividade
  exigiria que o GHC soubesse que nenhum `Interp t` e um `Maybe`, e ele nao sabe.
  A verbosidade e o preco real da familia de tipos aqui, e vale citar no relatorio
  como custo medido.
- **Nulabilidade de operando e existencial.** Em `EAnd a b`, as nulabilidades de
  `a` e `b` nao aparecem no tipo do resultado (so dentro de `MergeNull`), portanto
  sao variaveis existenciais e nao ha como nomea-las com `@`. No GHC 9.6 o jeito e
  assinatura de padrao no sub-padrao: `EAnd (a :: Expr s TBool n1) (b :: Expr s
  TBool n2) -> ...`. `TypeAbstractions` em padrao de construtor (`EAnd @n1 @n2 a
  b`) so chega no 9.10.
- **Por que os construtores carregam `KnownNullability`.** O avaliador precisa
  saber **em runtime** se cada lado e um `Maybe` ou nao, para escolher a equacao
  certa de `liftBin`. Um GADT guarda dicionarios: exigir a restricao na construcao
  e obte-la de volta na desconstrucao. E o mesmo mecanismo que faz `ELit` exigir
  `Show` e permitir que `renderExpr` imprima o literal sem restricao nenhuma na
  propria assinatura.
- **Ambiguidade na impressao.** `renderExpr :: Expr s t nl -> String` nao menciona
  `s`, `t` nem `nl` no resultado, entao uma expressao escrita direto no argumento
  nao tem como ser resolvida: o GHC pediu anotacao em sete pontos de uma unica
  chamada. A correcao foi extrair a expressao para uma definicao de topo com
  assinatura (`predicadoImpresso :: Predicate Vendors`). Isso e um sintoma
  generalizavel: **funcoes que consomem sem devolver o indice forcam anotacao**, e
  uma API polimorfica precisa de pontos de ancoragem nomeados.

- **A refatoracao piorou uma mensagem de erro, e isso so apareceu porque o teste
  negativo verifica a mensagem.** Antes, `col @"taxa"` numa coluna inexistente dava
  `No instance for (KnownIndex "taxa" TText '[])`. Depois de a prova passar a
  carregar a coluna inteira, passou a dar
  `Couldn't match expected type Text with actual type Slot c0; the type variable c0
  is ambiguous`, que nao diz nada sobre coluna nenhuma. Motivo: a coluna era uma
  variavel resolvida pela dependencia funcional, e quando a instancia nao existe a
  variavel simplesmente fica ambigua; o GHC reporta a ambiguidade e a causa
  desaparece.

  A correcao foi trocar a assinatura de `col` de
  `KnownIndex n c s => Row s -> Slot c` para
  `KnownIndex n (ColumnOf n s) s => Row s -> Slot (ColumnOf n s)`. Agora quem
  determina a coluna e a familia de tipos, e a familia ja tem o `TypeError` bom (o
  que lista as colunas disponiveis). A mensagem ficou melhor do que era antes da
  refatoracao.

  Ha uma tensao interessante aqui, porque o modulo 2 registrou o oposto: usar
  `ColumnOf` **travava** a recursao. As duas coisas sao verdadeiras e a distincao e
  onde o codigo esta. Na recursao interna o esquema e abstrato, `Lookup` nao reduz e
  a familia trava; e preciso carregar a informacao na prova. Na fronteira da API o
  esquema e concreto, a familia reduz, e usar a familia da erro melhor. **Regra que
  vale generalizar: prova por dentro, familia de tipos por fora.**
- Duas licoes de processo, nao de tipos. Primeira: o valor do teste negativo esta em
  checar a **mensagem**, nao a falha. Se o script so checasse "nao compilou", a
  regressao acima passaria despercebida e o projeto teria uma mensagem ruim na
  entrega. Segunda: um teste negativo pode passar pelo motivo errado. O
  `ColunaNulavelSemMaybe.hs` original usava um literal com `OverloadedStrings` e era
  rejeitado por falta de instancia `IsString (Maybe Text)`, um sintoma indireto que
  casava com a string esperada `"Maybe"` por acidente. Trocar o literal por um
  `Text` nomeado fez o erro passar a ser a incompatibilidade que interessa,
  `Couldn't match type Text with Maybe Text`.

### Testes negativos que passaram a existir
- `negativos/FiltroNulavel.hs` passa para `evalWhere` uma comparacao com coluna
  nulavel. Rejeitado com a mensagem propria de `Total Nullable`.
- `negativos/ColunaNulavelSemMaybe.hs` tenta **guardar** um valor cru numa coluna
  nulavel. Rejeitado com `Expected: Slot ("cnpj" :? TText), Actual: Text`.
- `negativos/LeituraNulavelSemMaybe.hs` tenta **ler** uma coluna nulavel como se
  fosse crua. Rejeitado com `Couldn't match type Maybe Text with Text`. Este e o
  lado que mais importa: nao existe caminho na API que entregue o valor de uma
  coluna nulavel sem obrigar a tratar a ausencia.

O `scripts/testar_negativos.py` tem agora 7 casos, e cada um confere um trecho
esperado **da mensagem**, nao apenas o fato de a compilacao falhar.

### Contagem parcial (para o relatorio)
Ao fim do modulo 3, a suite tem 43 testes positivos e 7 negativos. As classes de
erro de runtime que deixaram de existir sao quatro: coluna inexistente, tipo
errado de coluna, comparacao entre tipos incompativeis, e uso de valor
possivelmente ausente sem trata-lo. O custo medido, por enquanto, e verbosidade de
aplicacao de tipos no avaliador, concentrada em quatro funcoes auxiliares.

## Modulo 4: Algebra

### O problema
Os tres modulos anteriores descrevem como o dado e: o esquema (modulo 1), uma
linha (modulo 2), e uma expressao sobre essa linha (modulo 3). Nenhum deles
descreve como chegar ao dado. O modulo 4 e a algebra relacional: selecao,
projecao e juncao como operacoes verificadas em compile time.

Ha duas perguntas. Primeira: e possivel verificar em compile time que a projecao
escolhe colunas que existem, que a juncao nao produz nomes ambiguos, e que o
resultado tem o esquema certo? Segunda: e possivel distinguir um plano logico
(arvore de operacoes tal como o usuario escreveu) de um plano fisico (pronto para
executar), de forma que chamar o executor num plano nao compilado seja um erro de
compilacao?

### A solucao e por que ela funciona
A primeira pergunta ja estava quase respondida: ColumnOf, Disjoint, Append e
MakeNullable eram o vocabulario. O modulo 4 liga isso num GADT de plano:

```haskell
type QueryNode :: Schema -> Type
data QueryNode s where
  Table  :: String -> [Row s] -> QueryNode s
  Filter :: Predicate s -> QueryNode s -> QueryNode s
  Pick   :: (ProjectRow ns s, KnownSymbols ns)
         => Proxy ns -> QueryNode s -> QueryNode (Project ns s)
  Join   :: Disjoint l r
         => Predicate (Append l r) -> QueryNode l -> QueryNode r -> QueryNode (Append l r)
  LJoin  :: (Disjoint l r, NullRow r, ToNullable r)
         => Predicate (Append l r) -> QueryNode l -> QueryNode r
         -> QueryNode (Append l (MakeNullable r))
```

Cada construtor carrega as restricoes que a avaliacao vai precisar -- o mesmo
mecanismo de GADT como dicionario do modulo 3. Se os nomes da juncao colidirem,
a restricao Disjoint nao resolve e o programa nao existe. Se o esquema resultante
for diferente do esperado, o indice de QueryNode nao unifica. Nao ha verificacao
em runtime: o compilador ja fez tudo.

Para LEFT JOIN, o predicado e escrito sobre o esquema nao-nulavel do lado direito
(Append l r), porque ele e avaliado contra as linhas reais. Linhas sem
correspondencia recebem um Row preenchido com Nothing (nullRow), e as que casaram
passam por toNullable (embrulha cada valor em Just). Esse ponto e onde MakeNullable
do modulo 1 paga sua existencia: o tipo do resultado, Append l (MakeNullable r),
e calculado automaticamente.

A segunda pergunta e o estagio fantasma:

```haskell
data Stage = Logical | Physical

newtype Query (st :: Stage) (s :: Schema) = UnsafeQuery (QueryNode s)

compile :: Query Logical s -> Query Physical s
compile (UnsafeQuery q) = UnsafeQuery q  -- identidade em runtime

eval :: CanExecute st => Query st s -> [Row s]
```

Em runtime Logical e Physical sao o mesmo bit pattern: compile e uma identidade.
A diferenca e so de tipo, e e o suficiente para que eval nao aceite um plano
logico -- a instancia CanExecute Logical tem um TypeError, igual a Total do
modulo 3. O modulo 7 vai substituir compile por uma passagem de otimizacao real.

### Dificuldades e surpresas
- **appendRow como funcao simples, sem typeclass.** A primeira tentativa usou uma
  typeclass AppendRow s1 s2, com duas instancias. Nao e necessario. Quando o GHC
  casa o padrao GADT de Row (RNil ou RCons), ele sabe que s1 e '[] ou c : s1', e
  Append reduz diretamente. A funcao simples `appendRow :: Row s1 -> Row s2 -> Row
  (Append s1 s2)` compila sem nenhum helper adicional.
- **Anotar o predicado do LJoin nao funciona.** A tentativa `LJoin (p :: Predicate
  (Append l r)) ql qr` introduz skolems l e r novos, que o GHC nao consegue
  unificar com os existenciais l1 e r1 do GADT porque Append nao e injetiva. O
  GHC sabe que p :: Predicate (Append l1 r1) e que queremos Predicate (Append l
  r), mas sem injetividade nao pode concluir Append l r ~ Append l1 r1.
  A correcao foi anotar os sub-nos: `LJoin p (ql :: QueryNode l) (qr :: QueryNode
  r)`. QueryNode e um GADT injetivo no indice, entao l ~ l1 e r ~ r1 resolvem
  trivialmente, e p ganha o tipo certo via unificacao. Regra que vale generalizar:
  **para nomear existenciais escondidos atras de familias nao injetivas, anote o
  argumento cujo tipo e direto**.
- **CR no lugar de backslash no patch Python.** Em strings Python, `
` e um
  retorno de carro (0x0D), nao backslash + r. Para escrever um lambda Haskell
  `\r ->` num arquivo via Python, o patch precisa de `\\r ->` no codigo-fonte
  Python (quatro barras). Ou melhor: fazer o patch em bytes (`b"\r ->"`) para
  evitar a dupla interpretacao. O bug causou erros de parse no GHC que so
  apareceram na compilacao do executavel, nao da biblioteca -- mais um caso onde
  o teste negativo (ou neste caso o build) e o que expoe o problema.

### Testes negativos que passaram a existir
- `negativos/EvalSemCompilar.hs` chama `eval` diretamente em `fromTable`, sem
  passar por `compile`. Rejeitado com:
  `TypedQL: esta consulta ainda nao foi compilada. Aplique compile antes de executar.`

### Contagem parcial
Ao fim do modulo 4: 52 testes positivos, 8 negativos, zero warning.
Classes de erro de runtime eliminadas pelo tipo: coluna inexistente, tipo errado,
comparacao incompativel, NULL sem tratar, coluna ambigua em juncao, esquema
incompativel em juncao, execucao de plano nao compilado.

## Modulo 5: Frontend.Static (quasiquoter de SQL)

### A pergunta
Da para escrever SQL de verdade, com a sintaxe familiar `SELECT ... FROM ... WHERE`,
e ainda assim ter todas as garantias de tipo dos modulos 1 a 4? A resposta e o
Template Haskell. O quasiquoter `[sql| ... |]` roda *antes* da compilacao
propriamente dita: le a string, analisa a gramatica e *gera codigo Haskell* -- as
mesmas chamadas a `project`, `select`, `colE` que eu escreveria a mao. O GHC entao
verifica esse codigo gerado exatamente como verificaria o codigo escrito por mim.

### O ponto central: o quasiquoter nao verifica tipos
Ele so traduz sintaxe. Toda a checagem continua sendo do GHC sobre a `Exp` gerada.
Se o SQL menciona uma coluna que nao existe, o quasiquoter gera um
`project (Proxy @'["taxa"]) ...` perfeitamente bem-formado, e e o GHC que rejeita,
com a *mesma mensagem do modulo 2*. O erro de nome de coluna, que num banco
tradicional so aparece quando a query roda, aqui aparece ao compilar. O negativo
`ProjecaoSqlInexistente.hs` prova isso: escrever `SELECT taxa FROM vendorsQ` com
`taxa` inexistente nao produz erro de runtime, produz um programa que nao existe.

### A implementacao
Um pipeline curto: `String -> tokens -> arvore -> Exp`.
- **Tokenizador** manual (`tokeniza`): reconhece `*`, `,`, `=`, identificadores,
  texto entre aspas e numeros. Erro de lexico vira `Left`.
- **Parser** descida-recursiva (`analisa`): `SELECT` (`*` ou lista de colunas)
  `FROM` identificador, `WHERE` opcional (`coluna = literal`). Palavras-chave sem
  diferenciar maiusculas.
- **Gerador** (`geraExp`): monta a `Exp` na ordem da semantica SQL -- tabela,
  depois `select` (WHERE), depois `project` (SELECT). A lista de colunas vira
  `Proxy :: Proxy '["c1", "c2"]` (ponte para o nivel de tipos). O literal de texto
  vira `ELit (T.pack "...")`; inteiro e fracionario ficam polimorficos para o GHC
  unificar com o tipo da coluna.

O nome depois de `FROM` e um identificador Haskell em escopo: `VarE (mkName tabela)`.
O quasiquoter nao inventa a tabela, ele referencia a `Query` que eu ja defini
(`vendorsQ`). `mkName` gera um nome nao-higienico que resolve no ponto do splice,
entao um `let vendorsQ = ...` no mesmo bloco funciona.

### Dificuldades e surpresas
- **`TemplateHaskellQuotes` vs `TemplateHaskell`.** Usei `TemplateHaskellQuotes`
  (mais restrito, so habilita as aspas `'nome` e `''Tipo`, nao splices). Basta,
  porque o modulo *define* o quasiquoter, quem *usa* e que precisa de `QuasiQuotes`.
  Isso evita o custo de habilitar TH completo na biblioteca.
- **`Q` fora do escopo.** O import inicial de `Language.Haskell.TH` esqueceu `Q`,
  dando GHC-76037. `compilaSql :: String -> Q Exp` precisa de `Q` explicito na
  lista de import.
- **Aspas duplas no literal renderizado.** `renderExpr` faz `ELit x -> show x`, e
  `show` de `Text` usa aspas duplas. Entao `WHERE vendor_code = "VFAKE"` renderiza
  como `(vendor_code = "VFAKE")`, nao com aspas simples de SQL. O teste de
  `renderSQL` teve que refletir isso.
- **Splice TH exige a lib registrada.** Rodar o negativo com `ghc -package typedql`
  so funciona depois de `stack build` registrar `typedql` no pkgdb local. Matar um
  build no passo copy/register deixa o pkgdb vazio e quebra o splice.

### Testes que passaram a existir
- Grupo `Frontend.Static` no Spec: `SELECT *`, projecao de lista, `WHERE` com texto,
  `WHERE` com numero, equivalencia com a algebra escrita a mao, e o formato do
  `renderSQL` do plano gerado. 6 casos.
- `negativos/ProjecaoSqlInexistente.hs`: coluna inexistente via SQL, rejeitada com
  `TypedQL: a coluna "taxa" nao existe neste esquema.`

### Contagem parcial
Ao fim do modulo 5: 58 testes positivos, 9 negativos, zero warning. O frontend de
SQL nao abriu nenhuma brecha: toda garantia dos modulos anteriores atravessa o
quasiquoter, porque ele so gera o codigo que o GHC ja sabia verificar.

## Modulo 6: Frontend.Dynamic (existenciais e singletons)

### A pergunta
O modulo 5 sabia tudo em compile time: a string SQL era uma literal no
codigo-fonte, o quasiquoter a processava antes da compilacao, e o GHC verificava
o resultado. O que acontece quando a string so existe em runtime -- quando ela
vem de um arquivo, de um parametro, de uma entrada do usuario? E possivel executar
consultas dinamicas e ainda manter qualquer garantia de corretude?

### A resposta: existencial + singleton
A solucao e empacotar o esquema num existencial ('SomeTable', 'SomeResult') e
carregar dentro do construtor duas coisas que seriam necessarias em runtime:

1. O singleton 'SSchema s': permite chamar 'header' e 'showRow' sem conhecer o
   tipo 's' estaticamente. Sem o singleton, nao ha como percorrer as colunas.
2. A prova 'All Show s': 'showRow' exige que cada 'Slot col' tenha instancia
   'Show'. Guardando a prova no existencial, ela volta ao escopo quando o padrao
   e casado, e 'showRow' pode ser chamado normalmente dentro do eliminador.

O tipo de fora -- 'SomeTable' e 'SomeResult' -- nao menciona 's'; quem recebe
esses valores so pode usar os observadores ('dynHeader', 'dynRows', 'dynRowCount')
ou o eliminador ('withSomeTable', 'withSomeResult'). Nao da para chamar
'col @"vendor_code"' num resultado dinamico porque o GHC nao sabe que 's' tem
essa coluna: so teria se a chamada fosse verificada contra um esquema estatico.

### O contraste com o modulo 5
| Dimensao                 | Modulo 5 (estatico)         | Modulo 6 (dinamico)       |
|--------------------------|-----------------------------|-----------------------------|
| SQL                      | literal no codigo-fonte     | string em runtime           |
| Erros de nome de coluna  | erro de compilacao          | Left em runtime             |
| Acesso a colunas         | col @"nome", verificado     | somente via observadores    |
| Tipo do resultado        | Query Logical s (s = tipo)  | SomeResult (s escondido)    |

A garantia nao desaparece; ela muda de lugar: do tipo para o 'Either'.

### Dificuldades e surpresas
- **'All Show s' no existencial.** A primeira versao nao guardava o constraint.
  'showRow' falhou com "Could not solve: All Show s" porque dentro do pattern
  match em 'SomeTable sg rows', o GHC so sabe que 'sg :: SSchema s' e
  'rows :: [Row s]', mas nao que 'All Show s' vale para esse 's' especifico.
  A correcao foi adicionar 'All Show s' ao existencial e a todos os eliminadores.
  Isso tambem exigiu o pragma 'ConstraintKinds' em Dynamic.hs (familia 'All' devolve
  um 'Constraint', que precisa de 'ConstraintKinds' para aparecer em contextos).
- **Parser extraido para Parser.hs.** Para evitar duplicacao, o tokenizador e o
  analisador foram extraidos do Static.hs para um modulo compartilhado
  'TypedQL.Frontend.Parser'. Static e Dynamic importam dele.
- **Comparacao via 'show'.** O filtro WHERE avalia comparando strings: extrai o
  valor com 'showRow', serializa o literal com 'show', e compara. Funciona porque
  'show' e consistente: 'show (T.pack "VFAKE")' e 'show "VFAKE"' (String) produzem
  a mesma saida '"VFAKE"'. E uma limitacao documentada: o tipo SQL real da coluna
  nao e verificado em compile time no filtro dinamico.

### Negativo que passou a existir
- 'negativos/AcessoColunaDoExistencial.hs': tentativa de usar
  'col @"vendor_code"' dentro de 'withSomeTable'. Rejeitado com:
  'Could not deduce ... ColumnOf "vendor_code" s ... ~ T.Text'
  porque 's' e universal na continuacao; o GHC nao sabe que 's' tem a coluna.

### Contagem parcial
Ao fim do modulo 6: 66 testes positivos, 10 negativos, zero warning. Cada
modulo adicionou uma camada: o tipo garante o esquema (1), as linhas (2), as
expressoes (3), a algebra (4), o SQL estatico (5) e, agora, a fronteira com o
mundo dinamico (6), onde as garantias migram do compilador para o Either.

## Modulo 7: Optimize (catamorfismo indexado)

### Pergunta
Um otimizador de consultas e um programa que reescreve arvores. E a parte de um
SGBD onde os bugs custam mais caro, porque um plano reescrito errado nao trava:
ele devolve a resposta errada, silenciosamente. Quanto dessa classe de bug o
sistema de tipos elimina?

### Resposta: o contrato esta na assinatura
O gancho que o modulo 4 abriu e

```haskell
compileWith :: (QueryNode s -> QueryNode s) -> Query Logical s -> Query Physical s
```

O `s` e o mesmo dos dois lados. Uma reescrita que perca uma projecao, troque a
ordem das colunas de uma juncao ou esqueca que um LEFT JOIN torna o lado direito
nulavel nao e um programa valido. O otimizador nao precisa de teste para isso,
precisa de compilador. O negativo `OtimizadorMudaEsquema.hs` e exatamente essa
tentativa: descartar o `PickF` e devolver o filho. O GHC responde
`Could not deduce s1 ~ s, from the context s ~ Project ns s1`.

### O achado do modulo: a reescrita errada nao typecheca
Em SQL,

```sql
SELECT * FROM a INNER JOIN b ON p WHERE q
```

e equivalente a `... ON (p AND q)`. Com LEFT JOIN a mesma reescrita esta errada:
mover o `q` para o `ON` muda quais linhas do lado esquerdo ganham NULL em vez de
serem descartadas. Todo livro de banco de dados avisa; todo otimizador de verdade
tem um caso especial escrito a mao para isso.

Aqui nao ha caso especial e nao ha aviso. O predicado de um LEFT JOIN tem tipo
`Predicate (Append l r)` (ele e avaliado contra as linhas reais, antes de decidir
se houve correspondencia) e o filtro acima dele tem tipo
`Predicate (Append l (MakeNullable r))`. Sao tipos diferentes, entao `EAnd` nao
os aceita e a reescrita errada simplesmente nao existe. A semantica do SQL virou
um erro de tipo. Ver `negativos/AbsorcaoEmLeftJoin.hs`:

```
Couldn't match type 'Nullable' with 'NotNull'
  Expected: Expr (Append l0 r0) TBool NotNull
    Actual: Predicate (Append A (MakeNullable B))
```

Essa foi a hora em que a tese do projeto ficou concreta para mim. Nao e que o
tipo "documenta" a regra: e que a regra deixou de precisar ser lembrada.

### O catamorfismo
`QueryNode` e recursivo e indexado por `Schema`, logo o funtor base tambem tem
que ser indexado. `QueryF r s` e uma camada da arvore com um buraco
`r :: Schema -> Type` no lugar dos filhos, e o `fmap` dela e uma transformacao
natural, nao uma funcao comum:

```haskell
hmap  :: (forall x. f x -> g x) -> QueryF f s -> QueryF g s
hcata :: (forall x. QueryF g x -> g x) -> QueryNode s -> g s
hcata alg = alg . hmap (hcata alg) . out
```

O `forall x` nao e generalidade gratuita: os filhos de uma juncao tem indices
existenciais (`l` e `r` nao aparecem no indice do resultado, porque `Append` e
`MakeNullable` os consomem), entao a funcao aplicada aos filhos precisa funcionar
para um esquema que nao tem nome.

Nao foi preciso um `Fix`: `QueryNode` **ja e** o ponto fixo de `QueryF`, e o par
`out` / `into` e a testemunha do isomorfismo. Ha um teste positivo para isso
(`into . out` e a identidade sobre o plano).

As reescritas implementadas, todas locais, todas descobrindo composicao de graca
porque o catamorfismo sobe de baixo para cima:

| Regra | Efeito |
| --- | --- |
| filtro sempre verdadeiro | o no desaparece |
| filtro sempre falso | a subarvore inteira vira `Table "vazio" []` |
| filtros consecutivos | fundem num `AND` |
| filtro sobre INNER JOIN | absorvido pela condicao de juncao |

O carregador do catamorfismo e `Otimizado s`, que leva o plano novo mais o log das
reescritas aplicadas. Como o `s` do plano e o mesmo `s` do `Otimizado`, o log nao
tem como se referir a um plano de outro esquema.

### Dificuldades e surpresas
- **Kind do carregador.** `hcata` exige carregador de kind `Schema -> Type`. A
  metrica `tamanhoPlano` queria devolver um `Int`, e `Int` nao serve: precisou de
  um `newtype Contagem s = Contagem Int` com assinatura de kind explicita
  (`type Contagem :: Schema -> Type`). Sem a assinatura o GHC infere `* -> *` e
  reclama `Couldn't match kind '*' with '[Column]'`. Um newtype que ignora o
  proprio indice, so para ter o kind certo.
- **`EAnd` de dois predicados totais.** `EAnd p q` tem nulabilidade
  `MergeNull NotNull NotNull`. A familia reduz pela primeira equacao, entao o
  resultado e `Predicate s` sem precisar de coercao. Foi de graca, mas so porque
  a familia foi escrita fechada e com a equacao especifica antes do curinga
  (decisao do modulo 3 pagando juros aqui).
- **Refinamento de GADT dentro de tupla.** A primeira versao de `reescreveFiltro`
  casava `(constante p, filho)` numa tupla. Reescrevi como `case` aninhado: o
  refinamento de tipo do padrao GADT fica mais legivel, e o codigo diz na ordem
  certa que a avaliacao constante tem prioridade sobre a fusao.
- **`compile` nao foi removido.** O modulo 4 prometia que o 7 substituiria
  `compile`. Preferi manter os dois: `compile` como a compilacao sem otimizacao e
  `compileOtimizado` como a com. Isso deixou os testes de equivalencia possiveis
  ("otimizar preserva o resultado"), que sao a unica garantia que o tipo **nao**
  da: o tipo garante que o esquema nao muda, nao que as linhas nao mudam.

### Negativos que passaram a existir
- `negativos/AbsorcaoEmLeftJoin.hs`: absorver o WHERE no ON de um LEFT JOIN.
  Rejeitado por `Couldn't match type 'Nullable' with 'NotNull'`.
- `negativos/OtimizadorMudaEsquema.hs`: reescrita que descarta a projecao.
  Rejeitado por `Could not deduce s1 ~ s`.

### Contagem parcial
Ao fim do modulo 7: 76 testes positivos, 12 negativos, zero warning.
