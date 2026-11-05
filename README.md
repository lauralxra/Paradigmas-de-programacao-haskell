# Projeto de Paradigmas de Programação - Interpretador em Haskell

### Como rodar

Recomenda-se usar GHCI. No terminal na pasta do projeto: 

```ghci -package silently -package containers -package HUnit interpretador-21-09-26-2.hs```

Dentro do GHCI, para executar a suíte de testes:

```runTestTT suite```

### Equipe e divisão:

* Ana Luísa Vieira da Silva: Suporte ao tipo Float (TFloat, Flit, VFloat), operações e comparações mistas entre Int e Float e os testes correspondentes, feitos em TDD com um commit por ciclo
* Aline Oliveira: Verificação de tipos e conversão implícita entre Int e Float na declaração (Decl), na atribuição (Assign), na atribuição a posições de array (AssignIndex) e na passagem de argumentos para parâmetros de função (Call), e os testes correspondentes, feitos em TDD com um commit por ciclo
* Maria Eduarda de Souza: Chamada de função, funções passam a enxergar as variáveis globais, com a variável local tendo prioridade quando há nomes iguais, e os argumentos são avaliados da esquerda para a direita, de modo que o argumento seguinte vê a global já atualizada. Testes correspondentes feitos em TDD com um commit por ciclo
* Laura de Lima Cavalcante: responsável por implementar o suporte a arrays (vetores) de uma dimensão no interpretador, permitindo trabalhar com coleções de elementos dos tipos Int, Bool e Float.

### Mudanças e Decisões de Projeto

* Nenhuma função existente teve o tipo alterado. Foram acrescentados novos construtores de Float, mantendo a compatibilidade com os testes que já existiam.
* Operações: Int com Int gera Int; Presença de Float promove Float. o Operador Mod com Float gera erro explícito, e comparações mistas como 2 == 2.0 resultam em verdadeiro.
* Atribuição: o valor é ajustado ao tipo da variável, como em C (ver tabela abaixo). Combinações incompatíveis terminam com `error`.
* Chamada de função: a lógica do caso Call foi movida para a função nova chamaFuncao, que devolve o resultado e a memória atualizada. O tipo do evalExpr não foi alterado (o professor autorizou, mas optamos por funções novas para não quebrar o código dos outros integrantes).
* Variáveis globais: a memória da função passou a ser Map.union dos parâmetros com a memória de quem chamou. Como os parâmetros vêm primeiro, quando há variável local com o mesmo nome de uma global vale a local, conforme confirmado pelo professor por e-mail.
* Ordem dos argumentos: a função recursiva evalArgs substitui o mapM e avalia os argumentos um por vez, da esquerda para a direita. Se um argumento altera uma global, o seguinte já vê o valor novo, como pedido pelo professor (ex.: com g = 0, soma(incrementa(), g) retorna 2). A função atualizaMemoria devolve essas mudanças para quem chamou, sem deixar parâmetros ou variáveis novas declaradas dentro da função vazarem para fora.
* Testes da chamada de função: testLeGlobal (função lê global), testParamVence (parâmetro vence variável de fora) e testArgVeGlobal (argumento vê global atualizada), usando as funções de exemplo funUsaGlobal e funIncrementa.
* Limitações da chamada de função: só chamadas passadas diretamente como argumento atualizam a memória para os próximos (em soma(incrementa() + 1, g), g ainda vê o valor antigo); fora dos argumentos, mudanças em globais feitas por uma função não são mantidas, porque o evalExpr continua devolvendo só o valor; e se a função declarar uma variável com o mesmo nome de uma global, o valor dela substitui o da global ao final da chamada.

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
* Map.union : junta dois mapas; quando a mesma chave aparece nos dois, fica o valor do primeiro.
* Map.intersection : mantém só as chaves que aparecem nos dois mapas, com o valor do primeiro.
* zipWith : aplica uma função a pares de elementos de duas listas. É usada no `Call` para ajustar cada argumento ao tipo do seu parâmetro.
* foldr seq : avalia todos os elementos de uma lista antes de seguir; no `Call` garante que um argumento incompatível dê erro mesmo que o parâmetro não seja lido.
* seq : ``a `seq` b`` avalia `a` antes de devolver `b`. É usada no `Decl`, no `Assign`, no `AssignIndex` e no `Call` porque o `Map` é preguiçoso: sem ela, o erro de tipo só apareceria se a variável fosse lida depois.

### Exemplos da sintaxe da funcionalidade de arrays implementada no interpretador

| Operação | Exemplo |
|---|---|
| Declaração de array | `int a[5];` |
| Leitura de elemento | `a[0]` |
| Atribuição de valor | `a[1] = 42;` |
| Expressão no índice | `a[i + 1]` |

### Pendências

* Testes automáticos dos casos de erro de tipo (aguardando autorização do professor para usar Control.Exception).

### Uso de IA

A IA foi usada apenas com o intuito de tirar dúvidas e revisar, não para gerar código.

* Ferramentas utilizadas: Claude, Gemini
* Exceção (Aline): na verificação de tipos em elementos de array e na integração da branch `tipos` com o main, o código foi escrito pelo Claude Code e revisado por mim.
* (Eduarda): Claude usado para aprender elementos de Haskell do zero, entender o código, interpretar erros do ghci e receber orientação. O código foi digitado e testado por mim a cada ciclo.
* Laura:O ChatGPT foi utilizado como ferramenta de apoio ao aprendizado de Haskell, esclarecendo dúvidas sobre conceitos básicos, sintaxe e funções nativas da linguagem. Também auxiliou na compreensão do funcionamento de interpretadores e das funções presentes no código-base. Foram solicitados exemplos práticos e trechos de código ilustrativos para facilitar o entendimento dos conceitos estudados. Além disso, foi elaborado um quiz para verificar a compreensão dos conteúdos. A IA foi utilizada como apoio ao estudo e à interpretação do código.
  
### Prompts utilizados:

* "Como estruturo os ciclos de TDD em Haskell para colocar Float sem quebrar os testes de Int?"
* "Como usa o fromIntegral para misturar int com Float no Haskell?"
* "O operador Mod com Float deve dar erro explícito no intepretador?"
* "Numa comparação mista como 2 == 2.0, o retorno esperado é o VBool True?"
* "Acrescentaram mais coisas no github, veja o que mais eu posso fazer da minha parte"
* "Faça tudo que eu posso fazer e adicione o que for necessário no main"
* "Como funciona a assinatura de tipo de uma função em Haskell e como definir a da chamaFuncao?"
* "Por que é necessário alterar o case result of para usar a memória do fim da função?"
* "Qual o papel da função evalArgs e por que ela substitui o mapM?"
* "No conflito de merge do caso Call, por que aceitar as duas versões e depois mover o caso Index para dentro do evalExpr?"
* "Tente fazer mais commits"
* "A partir do código do interpretador, identifique as funções criadas e funções nativas e explique cada uma delas."
