# NimLang Interpreter

A dynamic language interpreter with C-like syntax, built using the tree-walking approach. Features web integration through WebAssembly.

> 📚 **Inspired by**: Chapter II of "Crafting Interpreters" by Robert Nystrom

## ✨ Features

- **Multi-target compilation**: Native C, JavaScript, and WebAssembly
- **Interactive web interface** powered by Svelte
- **Object-oriented programming** with classes and inheritance
- **Control flow** with conditionals, loops, and functions
- **Dynamic typing** with runtime error handling

## 🚀 Quick Start

### Prerequisites
- Nim >= 2.2.2
- Node.js (for web interface)

### Installation
```bash
nimble install
```

### Usage

**Run tests:**
```bash
nimble test
```

**Build WebAssembly:**
```bash
nimble wasmRelease
```

## 📝 Language Features

- Variables and expressions
- Functions with closures
- Classes with methods and inheritance
- Control flow (if/else, while loops)
- Built-in functions like `clock()`

## 🏗️ Project Structure

- `src/` - Core interpreter implementation
- `svelte/` - Web frontend
- `tests/` - Test files
- `dist/` - Compiled outputs
