# some unnamed language

## Develop

To build the program:

```bash
stack build
```

To run the program after build:

```bash
stack exec pi
```

To enter the REPL:

```bash
stack repl
```

### Adding dependencies

Append to `dependencies` at `package.yaml`.

```
dependencies:
- base >= 4.7 && < 5
- mtl
- ...
- <insert new dependency here>
```

**`pi.cabal` NO TOUCHY!**
