# Necessarily Valid Proofs (nvproof)
Necessarily Valid Proofs is a logic proof validator.

```
$ nvproof complier examples/example_2.nvp
1) (P ⇒ Q) AS
2) P AS
3) Q MP 1, 2
-------
Valid ✓
```

## Supported Rules
### Assumption (AS)
```
1) (P -> Q) AS
```

### Modus Ponens (MP)
```
1) (P -> Q) AS
2) P AS
3) Q MP 1, 2
```

### Contraposition (Contra)
```
1) (P -> Q) AS
2) (~Q -> ~P) Contra 1
```

### Double Negation (DN\*)
```
1) (P -> ~~~Q) AS
2) ~~(~~~~P -> ~Q) DN* 1
```

### L1
```
1) (P -> (Q -> P)) L1
```

### L2
```
1) ((P -> (Q -> R)) -> ((P -> Q) -> (P -> R))) L2
```

### L3
```
1) ((~P -> ~Q) -> ((~P -> Q) -> P)) L3
```

### M1
```
1) ([]P -> P) M1
```

### M2
```
1) ([](P -> Q) -> ([]P -> []Q)) M2
```

### M3
```
1) (P -> []<>P) M3
```

### M4
```
1) ([]P -> [][]P) M4
```

### M5
```
1) (<>P -> []<>P) M5
```

### Necessitation (Necess)
```
1) ([]P -> P) M1
2) []([]P -> P) Necess 1
```

The line that the necessitation is based on must be a tautology.

### Conditional Proof (ACP + CP)
```
1) (P -> (Q -> R)) AS
2) Q AS
3) P ACP
4) (Q -> R) MP 1, 3
5) R MP 4, 2
6) (P -> R) CP 3-5
```

### By definition of modal logic (by def modal)
```
1) ~<>~([]P -> <>P) AS
2) [](~<>~P -> ~[]~P) 1 by def modal
```

## Grammar
```
proof ::= [<step>]

step ::= <line_number> ") " <statement> " " <rule>

line_number ::= positive integer

statement ::= <binary_expression> | <unary_expression> | symbol

unary_expression ::= <unary_operator> <statement>

unary_operator ::= "~" | "[]" | "<>"

binary_expression ::= "(" <statement> <binary_operator> <statement> ")"

binary_operator ::= "->"

rule ::= "AS"
  | "MP " <line_number> "," <line_number>
  | "Contra " <line_number>
  | "DN* " <line_number>
  | "M1"
```
