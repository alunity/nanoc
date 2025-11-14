# nano c

## Yap

We're going to make a compiler for a really simple C style language targetting mips
in Haskell

Clear components
- Tokeniser (Super duper simple)
- Parser (Fairly simpler)
- Type checker (Kinda simple)
- Codegen (I've never done before)

Language features
- You can call functions
- Data Types: Int, Double, Char (In that order)
- Mathematical expressions
- Pass by reference????


## Goal program 1

- Functions, ints, math on ints
```c

int add(int x, int y){
    return x + y;
}

int main(){
    int x = 12;
    int y = 43;

    int res1 = add(x, y);
    int res2 = add(49, y);
    int res3 = add(x, 948);

    int resa = add(res1, res2);
    int res = add(resa, res3);

    outInt(res); 
    // Hard code output functions for now, since we don't even have strings to hardcode printf into
    // In codegen we can implement using printf

    return(0); // Define return as a built in function :)
}
```

### Grammar

```
prog ::= functions
functions := function functions | epsilon
function ::= type identifier(t_args){statements} # function definition
type ::= int
identifier ::= [a.z]  # No numbers, they're evil
t_args ::= t_arg t_args_tail | epsilon # t for typed
t_args_tail ::= ,t_arg t_args_tail | epsilon # t for typed
t_arg ::= type identifier
args ::= expression args_tail | epsilon
args_tail ::= ,expression args_tail | epsilon
statements ::= statement; statements | epsilon
statement ::= t_arg = expression | expression
expression ::= atom expression_tail 
expression_tail ::= + atom expression_tail | epsilon
atom ::= identifier | int_literal | function_call
int_literal ::= [2^16-1] | -[2^16-1]
function_call ::= identifier(args)
```


