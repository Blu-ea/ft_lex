# Lexer

This is a recreation of the lex/flex command in haskell

This is to generate a file `lex.yy.c` from a `.l` file.
The Goal being create a Lexer in C to be used in lexical processing of character input, to posibly an interface to `yacc`

## Flex keyword definition

- `yytext` -> The matched string (null-terminated)
- `int yyleng` -> The lenght of the matched string

## Flex source general format

```l
Definitions
%%
Rules
%%
UserSubroutines
```

The 1st `%%` is mandatory, the 2nd one is required only if user subroutines follow

For every line in the `Definitions section`, if it start with a `<blank>` or anything between `{%` `%}`   shall be considered C code and will be copied to the <u>external definition area</u> of the `lex.yy.c` file </br>
*Maybe before the declaration of `yylex()` or with the includes*

Anything at the start of `Rules Section` starting with `<blank>` or inside `{%` `%}` shall be copied after the variables definition inside `yylex()` and so before any line of code.

Other line defined as such (starting with a `<blank>` or inside `{%` `%}`)inside the `Rules Section`, after a first rule definition are illformed and result in an error.

Anything inside the `UserSubroutine` shall be copied after the `yylex()` definition.

---

As such, a possible lex file could look like this

with `<C-Code>` equivalente to

```
<blank><C-code>
{%
<Multi Line C-Code>
%}
```

```l
[Lex Definitions | <C-Code>] // Before the definition of yylex()
%%
<C-Code> // at the start yylex()
Lex Rules
%%
<C-Code> // after yylex()
```

All `<C-Code>` will be copied to the result file with no modification.

### Definition in LexFile

The definition being all that appears before the first and mandatory `%%`.
All non `<C-Code>` line in it will be interpreted as a **Lex substitution string**. They shall be formed as such </br>

``name substring``

Name in an ISO C stardar name definition, used to replace `{name}` in the rules definitions, with `substring`.
The name shall be recognized only when the braces are provided, and not inside bracket expression or double quote expression.
<!-- 
Line starting with `%s`/`%S` shall define that as an **acticated** state</br>
Line starting with `%x`/`%X` shall define that as an **deacticated** state
 -->

Stuff happend with %s and %x and states. (to figure out later)

You can have either :

- `%array`  
    Mark that `yytext` shall be of type `char[YYLMAX]`
- `%pointer`  
    Mark that `yytext` shall be of type `char *`

One of the two will be choose by default if not defined

| Declaration | Description                        | Min Values |
|-------------|------------------------------------|------------|
| %p n        | Number of positions                | 2500       |
| %n n        | Numnber of states                  | 500        |
| %a n        | Number of transitions              | 2000       |
| %e n        | Number of parse tree nodes         | 1000       |
| %k n        | Number of packed character classes | 1000       |
| %o n        | Size of the output array           | 3000       |

```c
    // Will need to Specify more what those do later
```

### Rules in LexFile

This section contain a List of Rules defined as follow  
`<ERE><BLANCK><Action>` (see [there](#regular-expressions-in-lex) for more information about `<ERE>`)

### User Subroutines in LexFile

Anything in the user subroutines section shall be copied to lex.yy.c following yylex().

---

## Regular Expressions in lex

The lex utility shall support the set of extended regular expressions (see XBD 9.4 [Extended Regular Expressions](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap09.html#tag_09_04)), with the following additions and exceptions to the syntax:

"..."  
    : Any string enclosed in doubled quote shall represent itself  
    Expected that `<backslash>-escapes` shall be recognized.

\<state>\<state1,state2,...>r  
    : The regular expression `r` shall be matched **ONLY** when the program is in the indicated state.

---

## Other

The Longest matches prevaile, (If `BCD` and `BC` are possible matches, only `BCD` will be used because of it's lenght)
If multiple rules matches, The 1st one defined is used
