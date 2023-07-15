## Project Setup

### Install

```bash
$ npm install
```

### Compilation
Before running the app, you need to compile fortran program.
```bash
npm run compileall
```
Windows compilation should work, but if it is broken, compile prognoz.f with gfrotran and put an exe into ```./resources/generated/```

### Development

```bash
$ npm run dev
```

### Build

```bash
# For windows
$ npm run build:win

# For macOS
$ npm run build:mac

# For Linux
$ npm run build:linux
```
