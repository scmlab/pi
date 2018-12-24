# Language Pi

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

### Testing

Build:

```bash
stack build pi:test:pi-tests
```

GHCi

```bash
stack build pi:test:pi-tests
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
