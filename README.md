# Language Pi

## How to download

* Linux:
* macOS:
* Windows (32-bit):
* Windows (64-bit):

## How to build

### Prerequisite

You will need `stack` and `git` on your machine:

* Install `stack`: https://docs.haskellstack.org/en/stable/README/#how-to-install
* Install `git`: https://git-scm.com/downloads

### Steps

1. Clone this repo with `git`

```
git clone git@github.com:scmlab/pi.git
```

2. Change the directory to the cloned repo

```
cd pi
```

3. Build the program with `stack`

```
stack install
```

The built program should be available as `pi`

```
pi
========================================
  ** Pi Tracer **
  arrow keys          for navigation
  :help               for this help message   (:h)
  :load FILEPATH      for loading files       (:l)
  :reload             for reloading           (:r)
  :exec               for execution           (:x)
========================================
```
