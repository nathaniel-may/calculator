# calculator
[![CircleCI](https://circleci.com/gh/nathaniel-may/calculator.svg?style=svg&circle-token=d4aaaf5f775f37c059cd7278a6dad0baf76de183)](https://circleci.com/gh/nathaniel-may/calculator)
[![codecov](https://codecov.io/gh/nathaniel-may/calculator/branch/master/graph/badge.svg?token=DUjyjy1m0j)](https://codecov.io/gh/nathaniel-may/calculator)

A basic four function calculator which respects the order of operations with a commandline interface. This project was a good introduction into building a simple programming language.

### Syntax
- Input language is whitespace agnostic
- `-` is the subtraction operator
- `~` denotes a negative number
- numbers must be of the form `~?\d+\.?\d*`
- four supported operators: `+`, `-`, `*`, `/`

### Examples

#### Prerequisites
- sbt must be installed
- repo is cloned

#### System Terminal
```
> cd calculator/
>
> sbt assembly
> ... lots of output while it builds
>
> cd target/scala-2.12/
>
> java -jar calc.jar "1+2"
> 3
>
> java -jar calc.jar "1+2*3.5"
> 8.0
>
> java -jar calc.jar "1+2*~3.5"
> -6.0
>
> java -jar calc.jar "1 + 2 * 3.5"
> 8.0
>
> java -jar calc.jar "1++2"
> "++" is not a valid sequence
>
> java -jar calc.jar "+"
> cannot start input with an operator: started with "+"
>
> java -jar calc.jar "5 +"
> operator "+" missing right-hand input
>
> java -jar calc.jar calc.jar "hello world"
> "h" is not a number or one of the following operators +, -, *, /
>
```