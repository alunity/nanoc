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

## Goal program 2

```c
int factorial_one(int n){
    if (n == 0){
        return(1);
    }
    return(n * factorial_one(n-1));
}

int factorial_two(int n){
    int res = 1;
    while (n > 0){
        res = res * n;
        n = n - 1;
    }
    return(res);
}

int main(){
    outInt(factorial_one(5));
    outInt(factorial_two(5));
    return(0);
}
```

### Grammar

```
program ::= functions

functions := function functions | epsilon

function ::= type identifier "(" t_args ")" scope               # function definition
type ::= int
identifier ::= [a.z]+                                           # No numbers, they're evil

while :== "while" "(" expression ")" scope
if :== "if" "(" expression ")" scope
scope :== "{" statements "}"

t_args ::= t_arg t_args_tail | epsilon                          # t for typed
t_args_tail ::= "," t_arg t_args_tail | epsilon                 # t for typed
t_arg ::= type identifier

args ::= expression args_tail | epsilon
args_tail ::= "," expression args_tail | epsilon

statements ::= statement statements | epsilon
statement ::= simple_statement ";" | compound_statement
simple_statement ::= t_arg "=" expression
                  | t_arg 
                  | identifier "=" expression
                  | expression
compound_statement ::= if | while | scope

expression      ::= logical_or

logical_or      ::= logical_and logical_or_tail
logical_or_tail ::= "||" logical_and logical_or_tail | ε

logical_and     ::= equality logical_and_tail
logical_and_tail::= "&&" equality logical_and_tail | ε

equality        ::= relational equality_tail
equality_tail   ::= ("==" | "!=") relational equality_tail | ε

relational      ::= additive relational_tail
relational_tail ::= ("<" | "<=" | ">" | ">=") additive relational_tail | ε

additive        ::= multiplicative additive_tail
additive_tail   ::= ("+" | "-") multiplicative additive_tail | ε

multiplicative  ::= unary multiplicative_tail
multiplicative_tail ::= ("*" | "/") unary multiplicative_tail | ε

unary ::= ("-" | "!") unary | atom
atom ::= identifier
      | int_literal
      | function_call
      | "(" expression ")"
int_literal ::= digits

function_call ::= identifier "(" args ")"
```
