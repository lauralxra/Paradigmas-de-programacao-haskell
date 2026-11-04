# Projeto de Paradigmas de Programação - Interpretador em Haskell

### Como rodar

Recomenda-se usar GHCI. No terminal na pasta do projeto: 

```ghci -package silently -package containers -package HUnit interpretador-21-09-26.hs```

Dentro do GHCI, para executar a suíte de testes:

```runTestTT suite```

### Equipe e divisão:

* Ana Luísa Vieira da Silva: Suporte ao tipo Float (TFloat, Flit, VFloat), operações e comparações mistas entre Int e Float e os testes correspondentes, feitos em TDD com um commit por ciclo
*  
* 
* 

### Mudanças e Decisões de Projeto

* Nenhuma função existente teve o tipo alterado. Foram acrescentados novos construtores de Float, mantendo a compatibilidade com os testes que já existiam.
* Operações: Int com Int gera Int; Presença de Float promove Float. o Operador Mod com Float gera erro explícito, e comparações mistas como 2 == 2.0 resultam em verdadeiro.

### Sintaxes e recursos não vistos:

* fromIntegral : função padrão para conversão segura entre Int e Float.

### Uso de IA

A IA foi usada apenas com o intuito de tirar dúvidas e revisar, não para gerar código.

* Ferramentas utilizadas: Claude, Gemini
  
### Prompts utilizados:

* "Como estruturo os ciclos de TDD em Haskell para colocar Float sem quebrar os testes de Int?"
* "Como usa o fromIntegral para misturar int com Float no Haskell?"
* "O operador Mod com Float deve dar erro explícito no intepretador?"
* "Numa comparação mista como 2 == 2.0, o retorno esperado é o VBool True?"
  



  



