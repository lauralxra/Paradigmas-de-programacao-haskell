# Projeto de Paradigmas de Programação - Interpretador em Haskell

### Como rodar

Recomenda-se usar GHCI. No terminal na pasta do projeto: 

```ghci -package silently -package containers -package HUnit interpretador-21-09-26-2.hs```

Dentro do GHCI, para executar a suíte de testes:

```runTestTT suite```

### Equipe e divisão:

* Ana Luísa Vieira da Silva: Suporte ao tipo Float (TFloat, Flit, VFloat), operações e comparações mistas entre Int e Float e os testes correspondentes, feitos em TDD com um commit por ciclo
* Aline Oliveira: Verificação de tipos e conversão implícita entre Int e Float na declaração (Decl), na atribuição (Assign), na atribuição a posições de array (AssignIndex) e na passagem de argumentos para parâmetros de função (Call), e os testes correspondentes, feitos em TDD com um commit por ciclo
* 
* 

### Mudanças e Decisões de Projeto

* Nenhuma função existente teve o tipo alterado. Foram acrescentados novos construtores de Float, mantendo a compatibilidade com os testes que já existiam.
* Operações: Int com Int gera Int; Presença de Float promove Float. o Operador Mod com Float gera erro explícito, e comparações mistas como 2 == 2.0 resultam em verdadeiro.
* Atribuição: o valor é ajustado ao tipo da variável, como em C (ver tabela abaixo). Combinações incompatíveis terminam com `error`.

### Verificação de tipos na atribuição

| Variável | Valor | Resultado                                         |
|----------|-------|---------------------------------------------------|
| int      | Int   | guarda como está                                  |
| int      | Float | só a parte inteira (truncate): 3.9 → 3, -3.9 → -3 |
| float    | Int   | converte (fromIntegral): 2 → 2.0                  |
| float    | Float | guarda como está                                  |
| bool     | Bool  | guarda como está                                  |

A mesma tabela vale para `Decl`, `Assign`, `AssignIndex` (elementos de array) e para
os argumentos de uma chamada de função, que são ajustados ao tipo declarado de cada
parâmetro (`metade(5)` com `float x` recebe 2.0; `dobro(2.7)` com `int x` recebe 2).
Qualquer outra combinação termina com `error` e a mensagem
`Tipo incompativel: variavel do tipo <tipo> recebeu valor do tipo <tipo>`.

O `Decl` usa o tipo declarado. Já o `Assign` descobre o tipo da variável pelo
valor que ela já guarda (`tipoDoValor`), porque o `Env` só guarda valores. O
`AssignIndex` faz o mesmo com o elemento que já está na posição do array. No `Call`,
o tipo vem da declaração do parâmetro (`funParams`).

### Sintaxes e recursos não vistos:

* fromIntegral : função padrão para conversão segura entre Int e Float.
* truncate : pega a parte inteira, arredondando em direção a zero, como em C (`floor` daria -4 para -3.9).
* zipWith : aplica uma função a pares de elementos de duas listas. É usada no `Call` para ajustar cada argumento ao tipo do seu parâmetro.
* foldr seq : avalia todos os elementos de uma lista antes de seguir; no `Call` garante que um argumento incompatível dê erro mesmo que o parâmetro não seja lido.
* seq : ``a `seq` b`` avalia `a` antes de devolver `b`. É usada no `Decl`, no `Assign`, no `AssignIndex` e no `Call` porque o `Map` é preguiçoso: sem ela, o erro de tipo só apareceria se a variável fosse lida depois.

### Pendências

* Testes automáticos dos casos de erro de tipo (aguardando autorização do professor para usar Control.Exception).

### Uso de IA

A IA foi usada apenas com o intuito de tirar dúvidas e revisar, não para gerar código.

* Ferramentas utilizadas: Claude, Gemini

  
### Prompts utilizados:

* "Como estruturo os ciclos de TDD em Haskell para colocar Float sem quebrar os testes de Int?"
* "Como usa o fromIntegral para misturar int com Float no Haskell?"
* "O operador Mod com Float deve dar erro explícito no intepretador?"
* "Numa comparação mista como 2 == 2.0, o retorno esperado é o VBool True?"
* "Acrescentaram mais coisas no github, veja o que mais eu posso fazer da minha parte"
* "Faça tudo que eu posso fazer e adicione o que for necessário no main"
* "Tente fazer mais commits"
