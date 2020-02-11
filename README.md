# brainfrick
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
$ ./brainfrick --[TARGET]
```

where the `TARGET` can be one of the following:
- `C`
<!-- TODO: add more -->

