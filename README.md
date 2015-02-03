# cabal-test-bin: A program finding temporal build-binary for cabal-test

[![Hackage version](https://img.shields.io/hackage/v/cabal-test-bin.svg?style=flat)](https://hackage.haskell.org/package/cabal-test-bin)  [![Build Status](https://travis-ci.org/junjihashimoto/cabal-test-bin.png?branch=master)](https://travis-ci.org/junjihashimoto/cabal-test-bin)


cabal-test-bin finds exe-file for cabal test(like following commands)

```
$ cabal install --enable-tests --run-tests
$ cabal test
```

When a project uses cabal-sandbox, cabal-test-bin checks following paths.

```
<project root>/dist/dist-sandbox-<hash>/build/<exe-file>/<exe-file> 
<project root>/dist/build/<exe-file>/<exe-file>
```

cabal-test-bin is a program find 

## Getting started

Install this from Hackage.

```
$ cabal update && cabal install cabal-test-bin
```

## Usage

Command Line is below.

```
$ cabal-test-bin 'project-directory' 'program-name(this is just command-name, not path)'
```

cabal-test-bin generates jenkins hash from cabal-sandbox-dir of current project.
Then it finds 'project-directory'/dist/build/'program-name' and 'project-directory'/dist/dist-sandbox-'hash'/build/'program-name'.
When both files exits, it chooses newer one.
