# tsv2csv [![Hackage](https://img.shields.io/hackage/v/tsv2csv.svg)](https://hackage.haskell.org/package/tsv2csv) 

**Convert tab-delimited text to Excel-style comma-delimited**

According to some, Excel does it right. Some disagree. See
http://superuser.com/questions/302334/true-difference-between-excel-csv-and-standard-csv.

This program follows the rules mentioned in the linked page:

If a cell contains a comma, a newline, or a double-quote, surround it in double-quotes.
Otherwise, output it as is.

**Build**

Cabal:

```
cabal sandbox init
cabal install --dependencies-only
./install.sh
```

The result is `tsv2csv` in the `$HOME/.cabal/bin` directory,
which should be on your path.

```
which tsv2csv | xargs ls -l
```

Stack:

```
make install
```

**Run**

```
tsv2csv < input.tsv > output.csv
```

The intention is to compose a pipeline such as

```
sed 's/NOTE_TYPE_ID/14/g' db/stock-target-vs-downside.sql | ssh central@central '~/bin/mys devh' | tsv2csv
```

To run tests, use:

```
cabal build && echo runTests | cabal repl
```

**Publish new version to Hackage**

* ensure version has already been bumped in cabal file
* commit any final documentation changes
* `stack --resolver nightly-2017-04-05 sdist` , observe output
* select generated tarball from specified location, and perform an upload `https://hackage.haskell.org/packages/upload` (consider switching to using package candidates to avoid possible rework on failure below)
* observe status field for "Last success reported on ..." at the hackage homepage https://hackage.haskell.org/package/tsv2csv
* once success has been confirmed, `git tag 0.1.0.9`, using the appropriate version
* `git push --tags` to upload the tag you created
* merge branch, if a branch was created
* prepare for next version by bumping the version in cabal file, commit change
