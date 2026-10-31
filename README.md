## Verificação de tipos na atribuição (Pessoa 4)

### Tabela de conversões

| Variável | Valor | Resultado                                         |
|----------|-------|---------------------------------------------------|
| int      | Int   | guarda como está                                  |
| int      | Float | só a parte inteira (truncate): 3.9 → 3, -3.9 → -3 |
| float    | Int   | converte (fromIntegral): 2 → 2.0                  |
| float    | Float | guarda como está                                  |
| bool     | Bool  | guarda como está                                  |

Qualquer outra combinação termina com `error` e a mensagem
`Tipo incompativel: variavel do tipo <tipo> recebeu valor do tipo <tipo>`.

### Por que nenhuma assinatura mudou

O `Decl` usa o tipo declarado. Já o `Assign` descobre o tipo da variável pelo
valor que ela já guarda (`tipoDoValor`), porque o `Env` só guarda valores.

### Funções que talvez não tenham sido vistas na disciplina

- `truncate`: pega a parte inteira, arredondando em direção a zero, como em C.
  `floor` daria -4 para -3.9.
- `fromIntegral`: converte Int para Float.
- `seq`: ``a `seq` b`` avalia `a` antes de devolver `b`. É usada no `Decl` e no
  `Assign` porque o `Map` é preguiçoso: sem ela, o erro de tipo só apareceria se
  a variável fosse lida depois.

### Pendências

- Verificação em elementos de array (depende dos arrays).
- Testes automáticos dos casos de erro (depende do professor).
- Limitação extra (depende do professor).
