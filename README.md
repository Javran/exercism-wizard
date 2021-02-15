# Exercism wizard

[![Build Status](https://travis-ci.org/Javran/exercism-wizard.svg?branch=master)](https://travis-ci.org/Javran/exercism-wizard)

A wrapper around [Exercism CLI](https://github.com/exercism/cli) to provide unified interface to common operations.

Note that most of the stuff are just customized for myself, open to suggestions though.

## Features

Put built binary (which should be called `ew`) somewhere in `$PATH`, then:

- `ew proxy <args...>`: Proxy mode, same as executing `exercism <args...>`.
- `ew test`: Run language-specific test suite. (extra arguments supported)
- `ew lint`: Run language specific linter. (extra arguments supported)
- `ew fmt`: Format source code. (extra arguments supported)
- `ew get`: Fetch problem template, avoid overwriting.
- `ew on`: Spawn a sub-shell and switch to problem's project directory (might download problem template if missing)
- `ew edit`: Spawn a detached editor with solution file or project open.
- `ew rmignore`: Remove "ignore" annotation from tests.

## Planned features

Planned features by priority:

- `ew submit`: submit default stuff
- `ew peekrepo`: open a URL to language repo - for taking a look at problems locked away
- `ew peeksol`: open a URL to community solutions
- `ew submit`: support an explicit list, with ability to disable the default submit set
  note that this feature is non-trivial as we do want to avoid submitting random files.
  So if an explicit list is provided, all path must be:

  + relative to solution project home
  + exist
