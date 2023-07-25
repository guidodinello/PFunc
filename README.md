## Features

- **Checking Names and Types on the Abstract Syntax Tree (AST):** In this stage, the compiler conducts an analysis of the source code written in the functional language program. It thoroughly examines names and types, and if any syntactic errors, duplicate names, incorrect number of parameters, undeclared names, or type errors are detected, they will be promptly reported to the user. This ensures that the program's integrity and coherence are maintained before advancing to the next phase.

- **Source Code Optimization:** Once the program has passed the analysis and error correction phase, the compiler focuses on optimization. Here, expressions based on occurrences of free variables within the program are transformed. This optimization aims to improve the efficiency and performance of the resulting code.

- **Generation of Compilable and Executable C Language Code:** In this final phase, the compiler carries out the translation of the optimized functional code into C language. The output is code that can be compiled and executed in a C environment.

## Tests
![Test_err](https://img.shields.io/badge/Test_err-100%25-brightgreen?style=plastic&logo=github)
![Test_fun](https://img.shields.io/badge/Test_fun-100%25-brightgreen?style=plastic&logo=github)
![Test_opt](https://img.shields.io/badge/Test_opt-100%25-brightgreen?style=plastic&logo=github)
