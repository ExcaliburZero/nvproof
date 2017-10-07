# nvproof
A logic proof validator.

```
1) (P -> Q) AS
2) P AS
3) Q MP 1, 2
```

## Grammar
```
proof ::= [<step>]

step ::= <line_number> ") " <statement> "\t" <rule>

line_number ::= positive integer

statement ::= <binary_expression> | <unary_expression> | symbol

unary_expression ::= <unary_operator> <statement>

unary_operator ::= "~" | "[]" | "<>"

binary_expression ::= "(" <statement> <binary_operator> <statement> ")"

binary_operator ::= "->"

rule ::= "AS"
  | "MP " <line_number> "," <line_number>
  | "Contra " <line_number>
```
