# lsql-csv
Lapidary Structured Query Language implementation for csv files. The tool for fast text data manipulation.

The project is now under development and the syntax can be futher changed. 

## Installation
It is necessary, you had GHC (>8), Parsec (>3) and Glob (Haskell package) installed. Run then

    make
    sudo make install
    
Now should be lcsv installed in `/usr/local/bin`.

If you have installed `cabal`, you can alternativaly run

    cabal install
   
It will install the dependecies for you.    

## Usage

    lsql-csv [OPTIONS] COMMAND

### Options

    -n
    --named

Enables first line naming convension in csv files.
    
    -d CHAR
    --delimiter CHAR

Changes default primary delimiter. The default value is ';'.

    -s CHAR
    --secondary-delimiter CHAR
    
Changes default secondary delimiter. The default value is ','. It is used for arrays in csv files.

    -q CHAR
    --quote CHAR
    
This changes default quote char. The default value is '"'.

## LSQL - quick introduction 
SQL is really pleonastic language. It can be fine to use it for critical mission projects, because your code will be easy to read. But when you are trying to use it as write-only scripting language for your daily life, you will find yourself writting a lot of useless pieces of code. And here comes LSQL.

### Examples
We will show a few interesting examples of usage of the language in this implementation. If you have installed lsql-csv, you can try this yourself in your shell.

    lsql-csv '-, &1.2 &1.1'

This will print second and first column of csv file on stdin. You can read it like `from stdio S select S.second, S.first`

    lsql-csv -d: '-, &1.*, if &1.3>=1000' < /etc/passwd
    
This will print lines of users whose UID >=1000. It can be also written as
  
    lsql-csv -d: 'p=/etc/passwd, p.*, if p.3 >= 1000'
    lsql-csv -d: '/etc/passwd, *, if &1.3 >= 1000'
    lsql-csv '/etc/passwd -d:, *, if &1.3 >= 1000'
    
You can read it as `from /etc/passwd P select * where P.UID >= 1000`. As you can see, lsql style is much more compressed then standard SQL.

Let's see more complicated examples.

    lsql-csv -d: 'p=/etc/passwd g=/etc/group, p.1 g.1, if p.1 in g.4'
    
This will print all all pairs user <-> group excluding the default group. You can read it as `from /etc/passwd P, /etc/group G select P.1, G.1 where P.1 in G.4`. But this will give not much readable output. We can use `group by` to improve it (shortened as `g`).

    lsql-csv -d: 'p=/etc/passwd g=/etc/group, p.1 cat(g.1", "), if p.1 in g.4, by p.1'
    
This will cat all groups in one line delimeted by ", ". But I want there also default groups! How can it be done? Really easily.

    lsql-csv -d: 'p=/etc/passwd g=/etc/group, p.1 cat(g.1", "), if p.1 in g.4, by p.1' |
    lsql-csv -d: '- /etc/passwd /etc/group, &1.1 &1.2""&3.1, if &1.1 == &2.1 && &2.4 == &3.3'
    
What a nice oneliner (twoliner)!

### Documantion of language
Each column have number and may have name. If the source is file and have been given a name XXX by a user and can be addressed by XXX.N or XXX.NAME. It can be also addressed using &M.N syntax, where M is m-th input file or stdio.

Each command is made from blocks separated by comma. There are these types of blocks.

If you want to write exotic identifiers/names, put them in \`EXOTIC NAME\`

#### Select blocks
These blocks determine output. They are similar to bash expressions. They are made by statements separeted by whitespaces. These statements are expanded, evaluated, matched to column name and printed in delimitered format.

Each statement can consist
* Wildcard (Each wildcard will be expanded to multiple statements during processing)
* Bash brace expansion (e.g. {22..25} -> 22 23 24 25)
* Aritmetic expression in `$(expr)` format
* Quotes "anything" to prevent wildcards, expansions and matching (NOT SUPPORTED YET)
* Overnaming (alias) in format `NAME=stmt` (NOT SUPPORTED YET)
* Call of aggregate function `FUNCTION(next select block)` - there can't be any space after FUNCTION

Examples of select blocks:

    &1.[3-6]

This will print column 3, 4, 5 and 6 from first file.

    ax*.{6..4} 
    
This will print 6th, 5th, 4th of all files which name begins with ax.

If you want to concatenate strings without cat, you can write `a.1","a.2`.
#### From blocks
There must be exactly one from block (possibly empty) in the beginning of the command. The block can contain any files (and stdio in `-` format). You can use any syntax you would otherwise use in bash to select these files (wildcards, expansion,...). You can also overname the file using `NAME=stmt`. If there are more than 1 matching of stmt, the files will be named `(NAME, NAME1, NAME2,...)`.

Example:

    /etc/{passwd,group}
    
This will select `/etc/passwd` and `/etc/group` file. They can be addressed ether as `&1` or `/etc/passwd`, and `&2` or `/etc/group`.

You can also add custom attributes to files in formats `FILE -a "xyz" --attribute "z" -a;`. The attributes will be applied to all files which will be matched using `FILE` bash expression.

##### Possible attributes
    
    -n
    --named

It means that csv file have first line with names of columns

    -N
    --not-named

You can also set the exact opposite. This can be useful, if you changed the default behavior.

    -d CHAR
    --delimiter CHAR
    
This change the primary delimiter.

    -q CHAR
    --quote CHAR
    
This change the quote char.

Example:

    /etc/passwd -d:
    
### If block
This block always begins with if. The statement uses classical awk logic. You can use keywords >, <, <=, >=, ==, ||, &&, +, -, \*, /, div, %. You must also quote all string, or they can be behaved as numbers.

There are also new nonstandard keywords:
* `A in B` - means that A is substring of B
* `A.X =>= B.Y` - means that `A` is left outer joined on `B` with condition `A.X == B.Y` - *It can't be negated. Not supported yet.*

You can imagine if statement as where clausule in SQL.

### By block
This statement always begins with `by` and the rest of statement follows the same syntax as Select block. There can be only one By block in the whole command.

You can imagine by block as the group by clausule in SQL.

### Sort
This block can be at the end of the command. It begins with `sort` keyword and the rest is almost the same as the select block.

IN FUTURE: If you want numeric sorting instead of alphabetical sorting, you can add `-n` attribute after sort. If you want descendent sort, use `-D` attribute after sort.
