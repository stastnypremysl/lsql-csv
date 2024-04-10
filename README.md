# lsql-csv
`lsql-csv` is a tool for CSV file data querying from the shell with short queries. It makes it possible to work with small CSV files like with a read-only relational database.

The tool implements a new language LSQL similar to SQL, specifically designed for working with CSV files in shell. 

## Installation
It is necessary, you had GHC (`>=8 <9.29`) and Haskell packages Parsec (`>=3.1 <3.2`), Glob (`>=0.10 <0.11`), base (`>=4.9 <4.17`), text (`>=1.2 <1.3`) and containers (`>=0.5 <0.7`)
 installed. (The package boundaries given are identical to cabal boundaries.) Run then:

    make
    sudo make install
    
Now the lsql-csv is installed in `/usr/local/bin`. If you want, you can specify `INSTALL_DIR` like:

    sudo make INSTALL_DIR=/custom/install-folder install

This will install the package into `INSTALL_DIR`.

If you have installed `cabal`, you can alternatively run:

    cabal install
   
It will also install the dependencies for you.    

### Running the unit tests
If you want to verify, that the package has been compiled correctly, it is possible to test it by running:

    make test

This will run all unit tests for you.


## lsql-csv - quick introduction 
LSQL, the language of `lsql-csv`, aims to be a more lapidary language than SQL. Its design purpose is to enable its user to quickly write simple queries directly to the terminal - its design purpose is therefore different from SQL, where the readability of queries is more taken into account than in LSQL.


### Examples
One way to learn a new programming language is by understanding concrete examples of its usage. The following examples are written explicitly for the purpose of teaching a reader, how to use the tool `lsql-csv` by showing him many examples of its usage. 

The following examples might not be enough for readers, who don't know Unix/Linux scripting enough. If this is the case, please consider learning Unix/Linux scripting first before LSQL.

It is also advantageous to know SQL.

The following examples will be mainly about parsing of `/etc/passwd` and parsing of `/etc/group`. To make example reading more comfortable, we have added `/etc/passwd` and `/etc/group` column descriptions from man pages to the text.

File `/etc/passwd` has the following columns:
* login name;
* optional encrypted password;
* numerical user ID;
* numerical group ID;
* user name or comment field;
* user home directory;
* optional user command interpreter.

File `/etc/group` has the following columns:
* group name;
* password;
* numerical group ID;
* user list.

#### Hello World

    lsql-csv '-, &1.2 &1.1'

This will print the second (`&1.2`) and the first column (`&1.1`) of csv file on stdin. If you know SQL, you can read it like `from stdio S select S.second, S.first`. 

Commands are split by commas into blocks. The first block is (*and always is*) the from block. There are file names or `-` (stdin) separated by space. The second block is the select block, also separated by space.

For example:

    lsql-csv '-, &1.2 &1.1' <<- EOF
    World,Hello
    EOF
    
It returns:
    
    Hello,World

#### Simple filtering 
    lsql-csv -d: '-, &1.*, if &1.3>=1000' < /etc/passwd

This will print lines of users whose UID >=1000. It can also be written as:
  
    lsql-csv -d: 'p=/etc/passwd, p.*, if p.3 >= 1000'
    
    lsql-csv -d: 'p=/etc/passwd, &1.*, if &1.3 >= 1000'

    lsql-csv -d: '/etc/passwd, &1.*, if &1.3 >= 1000'
    
The `-d:` optional argument means the primary delimiter is `:`. In previous examples we used overnaming, which allows us to give a data source file `/etc/passwd` a name `p`.

If you know SQL, you can read it as `from /etc/passwd P select * where P.UID >= 1000`. As you can see, lsql style is more compressed than standard SQL.
    
The output might be:

    nobody:x:65534:65534:nobody:/var/empty:/bin/false
    me:x:1000:1000::/home/me:/bin/bash

If you specify delimiter specifically for `/etc/passwd`, the output will be a comma delimited.
    
    lsql-csv '/etc/passwd -d:, &1.*, if &1.3 >= 1000'

It might return:

    nobody,x,65534,65534,nobody,/var/empty,/bin/false
    me,x,1000,1000,,/home/me,/bin/bash


#### Named columns
Let's suppose we have a file people.csv:
   
    name,age
    Adam,21
    Petra,23
    Karel,25

Now, let's get all the names of people in people.csv using the `-n` named switch:

    lsql-csv -n 'people.csv, &1.name'

The output will be:

    Adam
    Petra
    Karel

As you can see, we can reference named columns by a name. Named switch `-n` enables first-line headers.
If named columns are enabled, each column has two names under &X - the number name &X.Y and the actual name &X.NAME.

Now, we can select all columns with wildcard `&1.*`:

    lsql-csv -n 'people.csv, &1.*'

As the output, we get

    Adam,21,21,Adam
    Petra,23,23,Petra
    Karel,25,25,Karel

The output contains each column twice because wildcard `&1.*` was evaluated to `&1.1, &1.2, &1.age, &1.name`.
How to fix it?

    lsql-csv -n 'people.csv, &1.[1-9]*'

The output is now:

    Adam,21
    Petra,23
    Karel,25

The command can also be written as

    lsql-csv -n 'people.csv, &1.{1,2}'
    lsql-csv -n 'people.csv, &1.{1..2}'

The output will be in both cases still the same.


#### Simple join

Let's say, I am interested in the default group names of users. We need to join to tables: `/etc/passwd` and `/etc/group`. Let's do it.

    lsql-csv -d: '/etc/{passwd,group}, &1.1 &2.1, if &1.4 == &2.3'
    
What does `/etc/{passwd,group}` mean? Basically, there are three types of expressions. Select, from and arithmetic expression. In all select and from expressions, you can use expansion and wildcards just like in bash.
    
Finally, the output can be something like this:

    root:root
    bin:bin
    daemon:daemon
    me:me

The first column is the name of a user and the second column is the name of its default group.
    
#### Basic grouping
Let's say, I want to count users using the same shell. 

    lsql-csv -d: 'p=/etc/passwd, p.7 count(p.3), by p.7'
    
And the output?

    /bin/bash:7
    /bin/false:7
    /bin/sh:1
    /bin/sync:1
    /sbin/halt:1
    /sbin/nologin:46
    /sbin/shutdown:1
    
You can see here the first usage of `by` block, which is equivalent of `group by` in SQL. 

#### Basic sorting
Let's say, you want to sort your users with UID greater than or equal to 1000 ascendingly.

    lsql-csv -d: '/etc/passwd, &1.*, if &1.3 >= 1000, sort &1.3'

The output might look like:
  
    me1:x:1000:1000::/home/me1:/bin/bash
    me2:x:1001:1001::/home/me2:/bin/bash
    me3:x:1002:1002::/home/me3:/bin/bash
    nobody:x:65534:65534:nobody:/var/empty:/bin/false

The sort block is the equivalent of `order by` in SQL.

If we wanted descendingly sorted output, we might create a pipe to the `tac` command - the `tac` command prints the lines in reverse order:
    
    lsql-csv -d: '/etc/passwd, &1.*, if &1.3 >= 1000, sort &1.3' | tac

    
#### About nice outputs
There is a trick, how to concatenate two values in a select expression: Write them without space.

But how does the interpreter know the ends of the value name or value expression? You must use quotes for it - quotes themselves can't be part of the value name.
As an example, let's try to format our basic grouping example.

Let's try it!

    lsql-csv -d: 'p=/etc/passwd, "The number of users of "p.7" is "count(p.3)".", by p.7
    
The output might be:
 
    The number of users of /bin/bash is 7.
    The number of users of /bin/false is 7.
    The number of users of /bin/sh is 1.
    The number of users of /bin/sync is 1.
    The number of users of /sbin/halt is 1.
    The number of users of /sbin/nologin is 46.
    The number of users of /sbin/shutdown is 1.
   
As you can see, string formatting is sometimes very simple with LSQL.


#### Arithmetic expression

So far, we just met all kinds of blocks, and only if block accepts an arithmetic expression and the other accepts a select expression. 
What if we needed to run an arithmetic expression inside a select expression? There is a special syntax `$(...)` for it.

For example:

    lsql-csv -d: '/etc/passwd, $(sin(&1.3)^2 + cos(&1.3)^2)'
    
It returns something like:
    
    1.0
    1.0
    1.0
    0.9999999999999999
    ...
    1.0

If we run:
    
    lsql-csv -d: '/etc/passwd, $(&1.3 >= 1000), sort $(&1.3 >= 1000)'

We get something like:

    false
    false
    ...
    false
    true
    true
    ...
    true

#### More complicated join

Let's see more complicated examples.

    lsql-csv -d: 'p=/etc/passwd g=/etc/group, p.1 g.1, if p.1 in g.4'
    
This will print all pairs of user and its group excluding the default group. If you know SQL, you can read it as `from /etc/passwd P, /etc/group G select P.1, G.1 where P.1 in G.4`.

How does `in` work? It's one of the basic string level "consist". If A is a substring of B, then `A in B` is true. Otherwise, it is false.

And the output?

    root:root
    root:wheel
    root:floppy
    root:tape
    lp:lp
    halt:root
    halt:wheel

#### More complicated...

The previous example doesn't give a very readable output. We can use `group by` to improve it (shortened as `by`).

    lsql-csv -d: 'p=/etc/passwd g=/etc/group, p.1 cat(g.1","), if p.1 in g.4, by p.1'

The output will be something like:
    
    adm:adm,disk,sys,
    bin:bin,daemon,sys,
    daemon:adm,bin,daemon,
    lp:lp,
    mythtv:audio,cdrom,tty,video,
    news:news,
    
It groups all non-default groups of a user to one line and concatenates it delimited by ",". 

How can we add default groups too?

    lsql-csv -d: 'p=/etc/passwd g=/etc/group, p.1 cat(g.1","), if p.1 in g.4, by p.1' |
    lsql-csv -d: '- /etc/passwd /etc/group, &1.1 &1.2""&3.1, if &1.1 == &2.1 && &2.4 == &3.3'
    
This will output something like:

    adm:adm,disk,sys,adm
    bin:bin,daemon,sys,bin
    daemon:adm,bin,daemon,daemon
    lp:lp,lp
    mythtv:audio,cdrom,tty,video,mythtv
    news:news,news

The first part of the command is the same as in the previous example. The second part inner joins the output
of the first part with `/etc/passwd` on the username and `/etc/group` on the default GID number and prints
the output of the first part with an added default group name.

## Usage
Now, if you understand the examples, it is time to move forward to a more abstract description of the language and tool usage.

### Options

    -h
    --help

Shows short command line help and exits before doing anything else.

    -n
    --named

Enables first-line naming convention in csv files. This works only on input files. 
Output is always without first-line column names.
    
    -dCHAR
    --delimiter=CHAR

Changes default primary delimiter. The default value is `,`.

    -sCHAR
    --secondary-delimiter=CHAR
    
Changes default quote char (secondary delimiter). The default value is `"`.

### Datatypes
There are 4 datatypes considered: `Bool`, `Int`, `Double`, `String`. 
`Bool` is either true/false, `Int` is at least a 30-bit integer, `Double` double-precision floating point number, and `String` is an ordinary char string.

During CSV data parsing, the following logic of datatype selection is used: 
* `Bool`, if `true` or `false`;
* `Int`, if `[0-9]+` matches;
* `Double`, if `[0-9]+.[0-9]+(e[0-9]+)?` matches;
* `String`, if none of the above matches.

### Joins
Join means, that you put multiple input files into from block.

Joins always have the time complexity O(nm). There is no optimization made based on if conditions when you put multiple files into from block.

### Documentation of language

    lsql-csv [OPTIONS] COMMAND
    
    Description of the grammar
    
      COMMAND -> FROM_BLOCK, REST
    
      REST -> SELECT_BLOCK, REST
      REST -> BY_BLOCK, REST
      REST -> SORT_BLOCK, REST
      REST -> IF_BLOCK, REST
      REST ->
    
      FROM_BLOCK -> FROM_SELECTOR FROM_BLOCK
      FROM_SELECTOR ~~> FROM ... FROM //Wildcard and brace expansion

      FROM -> FROM_NAME=FROM_FILE OPTIONS
      FROM -> FROM_FILE OPTIONS

      OPTIONS -> -dCHAR OPTIONS
      OPTIONS -> --delimiter=CHAR OPTIONS
      OPTIONS -> -sCHAR OPTIONS
      OPTIONS -> --secondary-delimiter=CHAR OPTIONS
      OPTIONS -> -n OPTIONS
      OPTIONS -> --named OPTIONS
      OPTIONS -> -N OPTIONS
      OPTIONS -> --not-named OPTIONS
      OPTIONS ->

      
      SELECT_BLOCK -> SELECT_EXPR
      BY_BLOCK -> by SELECT_EXPR
      SORT_BLOCK -> sort SELECT_EXPR
      IF_BLOCK -> if ARITHMETIC_EXPR
    
 
      ARITHMETIC_EXPR -> ATOM
      ARITHMETIC_EXPR -> ONEARG_FUNCTION(ARITHMETIC_EXPR)
      ARITHMETIC_EXPR -> ARITHMETIC_EXPR OPERATOR ARITHMETIC_EXPR
      ARITHMETIC_EXPR -> (ARITHMETIC_EXPR)
      // Logical negation
      ARITHMETIC_EXPR -> ! ARITHMETIC_EXPR
      ARITHMETIC_EXPR -> - ARITHMETIC_EXPR
      
      SELECT_EXPR -> ATOM_SELECTOR SELECT_EXPR
      SELECT_EXPR ->
      
      ATOM_SELECTOR ~~> ATOM ... ATOM //Wildcard and brace expansion
      
      // eg. 1.0, "text", 'text', 1
      ATOM -> CONSTANT
      // eg. &1.1
      ATOM -> COLUMN_NAME
      ATOM -> pi
      ATOM -> e
      ATOM -> true
      ATOM -> false
      ATOM -> $(ARITHMETIC_EXPR)
      ATOM -> AGGREGATE_FUNCTION(SELECT_EXPR)
      ATOM -> ONEARG_FUNCTION(ARITHMETIC_EXPR)

      // # is not really char:
      // two atoms can be written without space 
      // and will be (string) appended, 
      // if they are separated using quote chars:
      //   left atom must end or right atom must begin with quote char
      // This rule doesn't apply inside ARITHMETIC_EXPR
      ATOM ~~> ATOM#ATOM     

      
      AGGREGATE_FUNCTION -> cat
      AGGREGATE_FUNCTION -> sum
      AGGREGATE_FUNCTION -> count
      AGGREGATE_FUNCTION -> max
      AGGREGATE_FUNCTION -> min
      AGGREGATE_FUNCTION -> avg
      
      //All trigonometric functions in radian
      ONEARG_FUNCTION -> sin
      ONEARG_FUNCTION -> cos
      ONEARG_FUNCTION -> tan
      
      ONEARG_FUNCTION -> asin
      ONEARG_FUNCTION -> acos
      ONEARG_FUNCTION -> atan
      
      ONEARG_FUNCTION -> sinh
      ONEARG_FUNCTION -> cosh
      ONEARG_FUNCTION -> tanh
      
      ONEARG_FUNCTION -> asinh
      ONEARG_FUNCTION -> acosh
      ONEARG_FUNCTION -> atanh
      
      ONEARG_FUNCTION -> exp
      ONEARG_FUNCTION -> sqrt
      
      //The length of the string
      ONEARG_FUNCTION -> size
      ONEARG_FUNCTION -> to_string
      
      ONEARG_FUNCTION -> negate
      ONEARG_FUNCTION -> abs
      ONEARG_FUNCTION -> signum
      
      ONEARG_FUNCTION -> truncate
      ONEARG_FUNCTION -> ceiling
      ONEARG_FUNCTION -> floor
      
      ONEARG_FUNCTION -> even
      ONEARG_FUNCTION -> odd
      
      // A in B means A is a substring of B
      OPERATOR -> in
      
      OPERATOR -> *
      OPERATOR -> **    //general power
      OPERATOR -> ^     //natural power
      OPERATOR -> /
      
      // Integer division truncated towards minus infinity
      // (x div y)*y + (x mod y) == x
      OPERATOR -> div
      OPERATOR -> mod
      
      //Integer division truncated towards 0
      // (x quot y)*y + (x rem y) == x  
      OPERATOR -> quot
      OPERATOR -> rem

      // greatest common divisor
      OPERATOR -> gcd
      // least common multiple
      OPERATOR -> lcm
      
      OPERATOR -> ++    //append
      
      OPERATOR -> +
      OPERATOR -> -
      
      OPERATOR -> <=
      OPERATOR -> >=
      OPERATOR -> <
      OPERATOR -> >
      OPERATOR -> !=
      OPERATOR -> ==
      
      OPERATOR -> ||
      OPERATOR -> &&

Each command is made from blocks separated by a comma. There are these types of blocks.
* From block
* Select block
* If block
* By block
* Sort block

The first block is always from block. If the block after the first block is without a specifier (`if`, `by`, or `sort`), then it is a select block. Otherwise, it is a block specified by the specifier.

From block accept specific grammar (as specified in the grammar description), select, by, and sort block select expression (`SELECT_EXPR` in the grammar) and if block arithmetic expression (`ARITHMETIC_EXPR` in the grammar).

Every source file has a number and may have multiple names - assign name, the name given to the source file by `ASSIGN_NAME=FILE_PATH` syntax in from block, and 
default name, which is given the path to the file or `-` in case of stdin in from block.

Each column of a source file has a number and may have a name (if the named option is enabled for the given source file). 

If the source file with index M (numbering input files from 1) has been given a name XXX, its columns can be addressed by &M.N or XXX.N, where N is the index of column (numbering columns from 1). 
If the named option is enabled and a column has the name `NAME`, it can also be addressed by &M.NAME or XXX.NAME.

If there is a collision in naming (two source files have the same name or two columns under the same source file have the same name), then the behavior is undefined.


#### Exotic chars
There are some chars that cannot be in symbol names (column names). For simplicity, we can suppose, they are everything but alphanumerical chars excluding `-`, `.`, `&` and `_`. 
Also first char of a symbol name must be non-numerical to not be considered as an exotic char.
Referencing names containing exotic chars without quotes is unsupported.

It is possible to reference columns with names with exotic chars using \` quote - like \`EXOTIC NAME\`. The source file name is always part of the column name from the syntax perspective of language - it must be inside the quotes.

#### Quote chars
There are 3 quotes (\`, " and ') used in Lsql. " and ' are always quoting a string. The \` quote is used for quoting symbol names.

These chars can be used for fast appending. If two atoms inside SELECT_EXPR are written without space and are separated using the quotes, they will be appended. For example, `abc"abc"` means: append column abc to the string abc.

#### Constants
There are 3 types of constants. String, Double, and Int. Everything quoted in " or ' is always String constant. Numbers without `[0-9]+` are considered Int constant and numbers `[0-9]+.[0-9]+` Double constant.

#### Operator precedence and associativity
The following list outlines the precedence and associativity of lsql-csv infix operators. The lower the precedence number, the higher the priority.
* 1: `in`, `**`, `^`
* 2: `*`, `/`, `div`, `quot`, `rem`, `mod`, `gcd`, `lcm`
* 3: `++`, `+`, `-`
* 4: `<=`, `>=`, `<`, `>`, `!=`, `==`
* 5: `||`, `&&`

All operators are right-to-left associative.


#### Select expression
They are similar to bash expressions. They are made by atom selector expressions separated by whitespaces. These expressions are expanded, evaluated, and matched to column names, constants, aggregate functions, or arithmetic expressions.

Every atom selector expression can consist:
* Wildcard (Each wildcard will be expanded to multiple statements during processing)
* Bash brace expansion (e.g. {22..25} -> 22 23 24 25)
* Arithmetic expression in `$(expr)` format
* Quotes \`anything\` to prevent wildcards and expansions
* Quotes " or ' to insert string
* Call of aggregate function `AGGREGATE_FUNCTION(next select block)` - there cannot be any space after FUNCTION
* Call of single arg function `ONEARG_FUNCTION(arithmetic expression)` - there cannot be any space after FUNCTION
* Constants
* Reference to a column name

If you want to concatenate strings without `++` operator, you can write: `a.1","a.2`.

Please, keep in mind, that operators must be put inside arithmetic expressions, or they will be matched to a column name or aggregate function.

#### Arithmetic expression
The statement uses mainly classical awk logic. You can use keywords `>`, `<`, `<=`, `>=`, `==`, `||`, `&&`, `+`, `-`, `*`, `/`... 

#### Select blocks
These blocks determine output. They accept select expressions and are evaluated and printed in a delimitered format. 

Every select block must contain at least one reference to the column name, or the behavior is undefined.

Examples of select blocks:

    &1.[3-6]

This will print columns 3, 4, 5, and 6 from the first file.

    ax*.{6..4} 
    
This will print the 6th, 5th, and 4th of all files whose name begins with ax.


#### From blocks
There must be exactly one from block at the beginning of the command. The block can contain any files (and `-` specifies standard input). You can use any syntax you would otherwise use in bash to select these files (wildcards, expansion...). You can also overname the file using `NAME=stmt`. If there is more than 1 matching of stmt, the files will be named `(NAME, NAME1, NAME2...)`.

Example:

    /etc/{passwd,group}
    
This will select `/etc/passwd` and `/etc/group` files. They can be addressed either as `&1` or `/etc/passwd`, and `&2` or `/etc/group`.

If `filename` is put inside \` quotes, no wildcard or expansion logic will apply to it.

You can also add custom attributes to files in the format `FILE -aX --attribute=X -b`. The attributes will be applied to all files which will be matched using `FILE` bash expression.

##### Possible attributes
    
    -n
    --named

It means that csv file has the first line with the names of the columns

    -N
    --not-named

You can also set the exact opposite. This can be useful if you change the default behavior.

    -dCHAR
    --delimiter=CHAR
    
This changes the primary delimiter.

    -sCHAR
    --secondary-delimiter=CHAR
    
This changes the secondary delimiter char.

Example:

    /etc/passwd -d:

Currently, commas and CHARs, which are also quotes in Lsql, are not supported as delimiters.
    
#### If block
This block always begins with `if`. They accept arithmetic expressions, which should be convertible to Bool - either String `false`/`true`, Int (`0` `false`, anything else `true`), or Bool. 
Rows with true are printed or aggregated, and rows with false are skipped.

Filtering is done before the aggregation.

You can imagine if block as where clause in SQL.

#### By block
This statement always begins with `by` and the rest of the block is a select expression. There can be only one By block in the whole command.

The by block is used to group the resulting set by the given atoms.

You can imagine by block as the group by clause in SQL. 

There must be at least one aggregate function in the select block if by block is present. Otherwise, behavior is undefined.

If there is an aggregate function present without by block present, aggregation runs over all rows at once.

#### Sort block
This block can be at the end of the command. It begins with the `sort` keyword and the rest is a select expression.

The sort block determines the order of the final output - given atoms are sorted in ascending order.

You can imagine a sort block as the order by clause in SQL.

There can be only one Sort block in the whole command.
