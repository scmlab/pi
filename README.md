# Language Pi

## Where to download

For Linux or macOS users, please try out these prebuilt files

* Linux: [pi-linux-x64](https://github.com/scmlab/pi/releases/download/v1.2.0/pi-linux-x64)
* macOS: [pi-osx-x64](https://github.com/scmlab/pi/releases/download/v1.2.0/pi-osx-x64)

`chmod` the downloaded file with the following command to make it executable (`pi-linux-x64` for example):

```
sudo chmod +x pi-linux-x64
```

and then execute it to see if it worked:

```
./pi-linux-x64
```

If the files listed above didn't worked out, or if you are using Windows,
please proceed to follow the steps below to build the program.

## How to build

### Prerequisite

You will need `stack` and `git` on your machine:

* Install `stack`: https://docs.haskellstack.org/en/stable/README/#how-to-install
* Install `git`: https://git-scm.com/downloads

### Steps

1. Clone this repo with `git`

```
git clone https://github.com/scmlab/pi.git
```

2. Change the directory to the cloned repo

```
cd pi
```

3. Build the program with `stack` inside the repo's directory

```
stack install
```

The built program should be available as `pi`
(or execute `stack path --local-install-root` to know where it's placed)

```
./pi
========================================
  ** Pi Tracer **
  arrow keys          for navigation
  :help               for this help message   (:h)
  :load FILEPATH      for loading files       (:l)
  :reload             for reloading           (:r)
  :exec               for execution           (:x)
========================================
```

## Syntax Highlighting

If you are using [Atom](https://atom.io/), please install the language grammar [`language-pi`](https://atom.io/packages/language-pi) for syntax highlighting.

## How to use this

To trace the program:

```
pi -t file.pi
```

To execute the program (very likely to explode in your face):

```
pi file.pi
```
