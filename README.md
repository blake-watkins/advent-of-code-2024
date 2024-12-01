# Common Lisp solutions to Advent of Code 2024

## How to run

* Checkout this repository and my [advent of code](https://github.com/blake-watkins/advent-of-code) repository into `~/quicklisp/local-projects`
* Load the package `(ql:quickload "advent-of-code-2024")`
* Change to package `(in-package :aoc-2024)`

### Downloading input
`(get-problem PROBLEM-NUMBER YEAR)` will retrieve the input, cache it in a file, and return it as a string. It needs your session cookie set up in the advent-of-code package first.

### Running problems
Generally running `(dayN (get-problem N YEAR) :part P)` will get the answer, where N is the day and P is 1 or 2. Some days are different though. 

## Other years' repositories

* [2021](https://github.com/blake-watkins/advent-of-code-2021)
* [2022](https://github.com/blake-watkins/advent-of-code-2022)
* [2023](https://github.com/blake-watkins/advent-of-code-2023)
