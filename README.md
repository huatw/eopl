## EOPL

### Workflow

1. string input => `lexer.ml` => tokens
2. tokens => `parser.ml` => AST(represented in `expr` variant type)
3. AST => `interp` => expressed value(represented in `exp_val` variant type)

### Interpreters
#### [list](https://github.com/huatw/eopl/tree/master/list)
#### [proc](https://github.com/huatw/eopl/tree/master/proc)
```bash
Interp.interp "let f = proc (x) { x + 1 } in (f 5)"
```