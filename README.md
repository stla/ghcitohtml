# ghcitohtml
Convert a Haskell script to html, including the outputs

## Usage 

```
$ ghcitohtml --help
ghcitohtml - based on HsColour

Usage: ghcitohtml FILE [-m|--monooutputs] [-n|--nooutputs]
                  [-p|--package-db package database]
  Convert a Haskell script to html

Available options:
  -h,--help                Show this help text
  FILE                     The Haskell script
  -m,--monooutputs         In case every output takes only one line
  -n,--nooutputs           Don't include the outputs
  -p,--package-db package database
                           Add the package database
```

#### Options:

- `-m`: use this option in case every output of the script fits on one line, to gain speed.

- `-n`: don't include the outputs, but only placeholders.

- `-p`: the package database, the same as the option `-package-db` of `ghc`.


## Compilation

- The compilation of `ghcitohtml.hs` requires the libraries `optparse-applicative` and `MissingH`.

- As it is given, it only works on Linux. For Windows, change the occurences of `"/tmp/..." to another temporary directory.