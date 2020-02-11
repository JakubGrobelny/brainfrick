# BrainFrick
Optimizing compiler for brainfuck

***

Currently supported targets:
- C

Planned targets:
- JVM

***

### Compilation

In order to compile the project you need to have `stack` installed.

```bash
$ stack setup
$ stack build
```

To run the compiler use
```bash
$ stack exec brainfrick
```

***

### Usage

```bash
brainfrick [OPTION...] file
  -o FILE    --output=FILE    output file
  -t TARGET  --target=TARGET  compilation target
  file                        input file
```

***