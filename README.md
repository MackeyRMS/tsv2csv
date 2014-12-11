#tsv2csv

**Convert tab-delimited text to Excel-style comma-delimited**

According to some, Excel does it right. Some disagree. See
http://superuser.com/questions/302334/true-difference-between-excel-csv-and-standard-csv.

This program follows the rules mentioned in the linked page:

If a cell contains a comma, a newline, or a double-quote, surround it in double-quotes.
Otherwise, output it as is.

**Build**

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

**Run**

```
tsv2csv < input.tsv > output.csv
```

The intention is to compose a pipeline such as

```
sed 's/NOTE_TYPE_ID/14/g' db/stock-target-vs-downside.sql | ssh central@central '~/bin/mys devh' | tsv2csv
```
