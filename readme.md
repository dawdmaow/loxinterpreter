# NimLang Interpreter

A dynamic language interpreter with C-like syntax, built using the tree-walking approach. Features web integration through WebAssembly.
Based on Chapter II of "Crafting Interpreters" by Robert Nystrom

## Online editor

https://dawdmaow.github.io/loxinterpreter/

## Features

- **Multi-target compilation**: C, JavaScript, and WebAssembly
- **Interactive web interface**
- **Object-oriented programming** with classes and inheritance
- **Control flow** with conditionals, loops, and functions
- **Dynamic typing** with runtime error handling

- Variables and expressions
- Functions with closures
- Classes with methods and inheritance
- Control flow (if/else, while loops)
- Built-in functions like `clock()`

## Prerequisites
- Nim >= 2.2.2
- Node.js (for web interface)

## Installation
```bash
nimble install
```

## Usage

**Run tests:**
```bash
nimble test
```

**Build WebAssembly:**
```bash
nimble wasmRelease
```

