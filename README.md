# Abstract Interpreter for While Language

A static analysis tool that performs abstract interpretation on While language programs using the interval domain. This tool analyzes programs to determine possible values of variables at each program point without executing the code.

## Overview

This project implements an abstract interpreter that:
- Parses While language source code
- Constructs Control Flow Graphs (CFG)
- Performs interval analysis using abstract interpretation
- Supports widening and narrowing operators for convergence
- Provides a REPL interface for interactive analysis

## Features

- **Interval Domain Analysis**: Track variable values as intervals (e.g., `[1, 10]`, `(-inf, 5]`)
- **Parametric Intervals**: Configure the interval range `[m, n]` for analysis
- **Constant Propagation**: Set `m > n` to use constant propagation domain
- **Widening Operator**: Accelerate convergence for loops (configurable)
- **Narrowing Operator**: Refine analysis results after widening (configurable)
- **CFG Visualization**: View the control flow graph of your program
- **AST Display**: Inspect the abstract syntax tree
- **Interactive REPL**: Configure and run analysis interactively

## While Language Syntax

The While language supports:

```
// Variables and expressions
x := [5, 10]          // Assign interval
y := x + [1, 2]       // Arithmetic operations
z := -x               // Negation

// Control flow
if x > 0 then
    y := x * 2
else
    y := 0
fi

while x > 0 do
    x := x - 1
done

skip                   // No-op statement
```

### Supported Operations
- **Arithmetic**: `+`, `-`, `*`, `/`
- **Comparisons**: `=`, `<`, `<=`, `>`, `>=`, `<>` (all against 0)
- **Intervals**: `[lower, upper]` with support for `inf` and `-inf`
- **Control flow**: `if-then-else-fi`, `while-do-done`

## Installation

### Prerequisites
- GHC (Glasgow Haskell Compiler)
- Cabal or Stack (Haskell build tools)

### Build
```bash
# Clone the repository
git clone https://github.com/MatteoCus/abstract-while-interpreter.git
cd abstract-while-interpreter

# Build with cabal
cabal build

# Or with stack
stack build
```

## Usage

### REPL Mode

Start the interactive REPL:

```bash
cabal run
# or
stack run
```

### REPL Commands

| Command | Description |
|---------|-------------|
| `:load` | Load a While source file from the examples directory |
| `:analyze` | Perform abstract interpretation on the loaded file |
| `:ast` | Display the abstract syntax tree |
| `:cfg` | Display the control flow graph |
| `:configuration` | Show current analyzer configuration |
| `:addBinding` | Add a new variable binding to the custom-defined initial configuration for the entry label |
| `:removeBinding` | Remove a variable binding from the custom-defined initial configuration for the entry label |
| `:interval` | Set the interval bounds `[m, n]` |
| `:widening` | Toggle widening operator (for loop convergence) |
| `:narrowing` | Toggle narrowing operator (for result refinement) |
| `:reset` | Reset configuration to defaults |
| `:info` | Display information about the analyzer |
| `:help` | Show help message |
| `:quit` | Exit the REPL |

### Example Session

```
> :load
Available example files:
1) simple-loop.txt
2) relational-loop.txt
3) fibonacci.txt
Select file: 1

> :interval
If lower bound is greater than upper bound, fallback to constant propagation domain. 

New lower bound (-inf, +inf for infinite bounds): 
>interval 0
New upper bound (-inf, +inf for infinite bounds): 
>interval 100
New interval: [0, 100]

> :widening
Widening: ON

> :analyze
Analysis Results:
  Label 0: { x ↦ ⊤ }
  Label 1: { x ↦ [0, +∞) }
  Label 2: { x ↦ [0, +∞) }
  Label 3: { x ↦ [5, +∞) }
...
```

## Algorithm

The analyzer uses the classic abstract interpretation framework:

1. **Parse** the While program into an AST
2. **Build CFG** from the AST with labeled nodes
3. **Initialize** abstract states (⊤ at entry, ⊥ elsewhere)
4. **Iterate** through CFG arcs, computing abstract transformers
5. **Detect loops** and apply widening at loop headers (if enabled)
6. **Apply narrowing** to refine results (if enabled)
7. **Converge** when fixed point is reached

### Interval Domain

The interval domain tracks possible values as ranges:
- `[a, b]` represents all integers from `a` to `b`
- `⊥` (Empty) represents unreachable/impossible states
- `⊤` ([-inf, +inf]) represents any possible value

Operations are performed over intervals:
- `[1, 3] + [2, 4] = [3, 7]`
- `[1, 3] * [2, 4] = [2, 12]`
- Guards refine intervals: `x > 0` with `x = [-5, 5]` becomes `x = [1, 5]`

## Configuration Options

### Interval Bounds
Set the parametric interval `[m, n]`:
- **Infinite intervals**: `m = -inf`, `n = +inf` (default)
- **Bounded intervals**: `m = 0`, `n = 100`
- **Constant propagation**: `m = 5`, `n = 3` (m > n)

### Widening
Enable to ensure termination for loops with infinite ascending chains. Applied at loop headers.

### Narrowing
Enable to refine over-approximations from widening. Applied after convergence.

## Examples

Example files should be placed in the `./examples/` directory. See the examples directory for sample While programs.

### Simple Loop Example (`examples/simple-loop.txt`)
```
x := 0;

while x - 5 < 0 do
    x := x + 1
done
```

### Relational Loop Example (`examples/relational-loop.txt`)
```
y := 10;
x := 0;

while x - y < 0 do
    x := x + 1;
    y := y - 1
done
```

## References

- Cousot, P., & Cousot, R. (1977). "Abstract Interpretation: A Unified Lattice Model for Static Analysis of Programs"
- Mine, A. (2017). "Tutorial on Static Inference of Numeric Invariants by Abstract Interpretation"

## License

BSD-3-Clause - Matteo Cusin