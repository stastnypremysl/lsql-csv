# `lsql-csv`
`lsql-csv` is a tool for CSV file data querying from a shell with short queries. It makes it possible to work with small CSV files like with a read-only relational database.

The tool implements a new language LSQL similar to SQL, specifically designed for working with CSV files in a shell. LSQL aims to be a more lapidary language than SQL. Its design purpose is to enable its user to quickly write simple queries directly
  to the terminal - its design purpose is therefore different from SQL, where the readability of queries is more taken into account than in LSQL.

## Installation
It is necessary, you had GHC (`>=8 <9.29`) and Haskell packages Parsec (`>=3.1 <3.2`), Glob (`>=0.10 <0.11`), base (`>=4.9 <4.20`), text (`>=1.2 <2.2`) and containers (`>=0.5 <0.8`)
 installed. (The package boundaries given are identical to the boundaries in Cabal package file.) For a build and an installation run:

    make
    sudo make install
    
Now the `lsql-csv` is installed in `/usr/local/bin`. If you want, you can specify `INSTALL_DIR` like:

    sudo make INSTALL_DIR=/custom/install-folder install

This will install the package into `INSTALL_DIR`.

If you have installed `cabal`, you can alternatively run:

    cabal install
   
It will also install the Haskell package dependencies for you.    

The package is also published at https://hackage.haskell.org/package/lsql-csv in the Hackage public repository. You can therefore also install it directly without the repository cloned with:

    cabal install lsql-csv

### Running the unit tests
If you want to verify, that the package has been compiled correctly, it is possible to test it by running:

    make test

This will run all tests for you.


## `lsql-csv` - quick introduction 


### Examples
One way to learn a new programming language is by understanding concrete examples of its usage. The following examples are written explicitly for the purpose of teaching a reader, how to use the tool `lsql-csv` by showing him many examples of its usage. 

The following examples might not be enough for readers, who don't know enough Unix/Linux scripting. If this is the case, please consider learning Unix/Linux scripting first before LSQL.

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
* user list separated by commas.

#### Hello World

    lsql-csv '-, &1.2 &1.1'

This will print the second (`&1.2`) and the first column (`&1.1`) of a CSV file on the standard input. If you know SQL, you can read it like `SELECT S.second, S.first FROM stdio S;`. 

Commands are split by commas into blocks. The first block is (*and always is*) the from block. There are file names or `-` (the standard input) separated by whitespaces. 
The second block in the example is the select block, also separated by whitespaces.

For example:

    lsql-csv '-, &1.2 &1.1' <<- EOF
    World,Hello
    EOF
    
It returns:
    
    Hello,World

#### Simple filtering 
    lsql-csv -d: '-, &1.*, if &1.3 >= 1000' </etc/passwd

This will print lines of users whose `UID >= 1000`. It can also be written as:
  
    lsql-csv -d: 'p=/etc/passwd, p.*, if p.3 >= 1000'
    
    lsql-csv -d: 'p=/etc/passwd, &1.*, if &1.3 >= 1000'

    lsql-csv -d: '/etc/passwd, &1.*, if &1.3 >= 1000'
    
The `-d:` optional argument means the primary delimiter is `:`. In the few examples we used overnaming, which allows us to give a data source file `/etc/passwd` a name `p`.

If you know SQL, you can read it as `SELECT * FROM /etc/passwd P WHERE P.UID >= 1000;`. As you can see, the LSQL style is more compressed than standard SQL.
    
The output might be:

    nobody:x:65534:65534:nobody:/var/empty:/bin/false
    me:x:1000:1000::/home/me:/bin/bash

If you specify delimiter specifically for `/etc/passwd`, the output will be a comma delimited.
    
    lsql-csv '/etc/passwd -d:, &1.*, if &1.3 >= 1000'

It might return:

    nobody,x,65534,65534,nobody,/var/empty,/bin/false
    me,x,1000,1000,,/home/me,/bin/bash

This happens because the default global delimiter, which is used for the output generation, is a comma.
The global delimiter changes by usage of the command-line optional argument, but remains unchanged by the usage of the attribute inside the from block.


#### Named columns
Let's suppose we have a file `people.csv`:
   
    name,age
    Adam,21
    Petra,23
    Karel,25

Now, let's get all the names of people in `people.csv` using the `-n` named optional argument:

    lsql-csv -n 'people.csv, &1.name'

The output will be:

    Adam
    Petra
    Karel

As you can see, we can reference named columns by the name. The named optional argument `-n` enables first-line headers.
If first-line headers are enabled by the argument, each column has two names under `&X` - the number name `&X.Y` and the actual name `&X.NAME`.

Now, we can select all columns with a wildcard `&1.*`:

    lsql-csv -n 'people.csv, &1.*'

As the output, we get:

    Adam,21,21,Adam
    Petra,23,23,Petra
    Karel,25,25,Karel

The output contains each column twice because the wildcard `&1.*` was evaluated to `&1.1, &1.2, &1.age, &1.name`.
How to fix it?

    lsql-csv -n 'people.csv, &1.[1-9]*'

The output is now:

    Adam,21
    Petra,23
    Karel,25

The command can also be written as

    lsql-csv -n 'people.csv, &1.{1,2}'
    lsql-csv -n 'people.csv, &1.{1..2}'
    lsql-csv 'people.csv -n, &1.{1..2}'

The output will be in all cases still the same.


#### Simple join

Let's say, I am interested in the default group names of users. We need to join tables `/etc/passwd` and `/etc/group`. Let's do it.

    lsql-csv -d: '/etc/{passwd,group}, &1.1 &2.1, if &1.4 == &2.3'
    
What does `/etc/{passwd,group}` mean? Basically, there are three types of expressions. The select, the from, and the arithmetic expression. 
In all select and from expressions, you can use the curly expansion and wildcards just like in `bash`.
    
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
    
You can see here the first usage of the by block, which is equivalent to `GROUP BY` in SQL. 

#### Basic sorting
Let's say, you want to sort your users by `UID` with `UID` greater than or equal to 1000 ascendingly.

    lsql-csv -d: '/etc/passwd, &1.*, if &1.3 >= 1000, sort &1.3'

The output might look like:
  
    me1:x:1000:1000::/home/me1:/bin/bash
    me2:x:1001:1001::/home/me2:/bin/bash
    me3:x:1002:1002::/home/me3:/bin/bash
    nobody:x:65534:65534:nobody:/var/empty:/bin/false

The sort block is the equivalent of `ORDER BY` in SQL.

If we wanted descendingly sorted output, we might create a pipe to the `tac` command - the `tac` command prints the lines in reverse order:
    
    lsql-csv -d: '/etc/passwd, &1.*, if &1.3 >= 1000, sort &1.3' | tac

    
#### About nice outputs
There is a trick, how to concatenate two values in the select expression: Write them without space.

But how will the interpreter know the ends of the values in a command? If the interpreter sees a char, that can't be part of the currently parsed value, it tries to parse it as a new value concatenated to the current one.
You can use quotes for it - quotes themselves can't be part of most value types like the column name or numerical constant.

As an example, let's try to format our basic grouping example.

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

So far, we just met all kinds of blocks, and only the if block accepts the arithmetic expression, and the other accepts the select expression. 
What if we needed to run the arithmetic expression inside the select expression? There is a special syntax `$(...)` for it.

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
    
This will print all pairs of users and its group excluding the default group. 
If you know SQL, you can read it as `SELECT P.1, G.1 FROM /etc/passwd P, /etc/group G WHERE G.4 LIKE '%' + P.1 + '%';` with operator `LIKE` case-sensitive and columns named by their column number.

How does `in` work? It's one of the basic string level "consist". If some string `A` is a substring of `B`, then `A in B` is `true`. Otherwise, it is `false`.

And the output?

    root:root
    root:wheel
    root:floppy
    root:tape
    lp:lp
    halt:root
    halt:wheel

The example will work under the condition, that there isn’t any username, which is an infix of any other username.

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
    
It groups all non-default groups of a user to a one line and concatenates it delimited by `,`. 

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

The examples will also work under the condition, that there isn’t any username,
which is an infix of any other username.

## Usage
Now, if you understand the examples, it is time to move forward to a more abstract description of the language and tool usage.

### Options

    -h
    --help

Shows a short command line help and exits before doing anything else.

    -n
    --named

Enables the first-line naming convention in CSV files. 
With this option, the first lines of CSV files will be interpreted as a list of column names.

This works only on input files. Output is always without first-line column
names.
    
    -dCHAR
    --delimiter=CHAR

Changes the default primary delimiter. The default value is `,`.

    -sCHAR
    --secondary-delimiter=CHAR
    
Changes the default quote char (secondary delimiter). The default value is `"`.

### Datatypes
There are 4 datatypes considered: `Bool`, `Int`, `Double`, and `String`. 
`Bool` is either `true`/`false`, `Int` is at least a 30-bit integer, `Double` is a double-precision floating point number, and `String` is an ordinary char string.

During CSV data parsing, the following logic of datatype selection is used: 
* `Bool`, if `true` or `false`;
* `Int`, if the POSIX ERE `[0-9]+` fully matches;
* `Double`, if the POSIX ERE `[0-9]+\.[0-9]+(e[0-9]+)?` fully matches;
* `String`, if none of the above matches.

### Joins
Join means, that you put multiple input files into the from block.

Joins always have the time complexity O(nm). 
There is no optimization made based on if conditions when you put multiple files into the from block.

### Documentation of language

    lsql-csv [OPTIONS] COMMAND
    
    Description of the grammar:
    
      COMMAND -> FROM_BLOCK, REST

    
      REST -> SELECT_BLOCK, REST
      REST -> BY_BLOCK, REST
      REST -> SORT_BLOCK, REST
      REST -> IF_BLOCK, REST
      REST -> LAST_BLOCK
        
      LAST_BLOCK -> SELECT_BLOCK   
      LAST_BLOCK -> BY_BLOCK
      LAST_BLOCK -> SORT_BLOCK
      LAST_BLOCK -> IF_BLOCK

    
      FROM_BLOCK -> FROM_EXPR

      FROM_EXPR -> FROM_SELECTOR FROM_EXPR
      FROM_EXPR -> FROM_SELECTOR

      // Wildcard and brace expansion
      FROM_SELECTOR ~~> FROM ... FROM 


      // Standard input
      FROM -> ASSIGN_NAME=- OPTIONS
      FROM -> - OPTIONS
  
      FROM -> ASSIGN_NAME=FILE_PATH OPTIONS
      FROM -> FILE_PATH OPTIONS


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

      ARITHMETIC_EXPR -> ARITHMETIC_EXPR OPERATOR ARITHMETIC_EXPR
      ARITHMETIC_EXPR -> (ARITHMETIC_EXPR)

      // Logical negation
      ARITHMETIC_EXPR -> ! ARITHMETIC_EXPR
      // Number negation
      ARITHMETIC_EXPR -> - ARITHMETIC_EXPR
      

      SELECT_EXPR -> ATOM_SELECTOR SELECT_EXPR
      SELECT_EXPR -> ATOM_SELECTOR
      
      // Wildcard and brace expansion
      ATOM_SELECTOR ~~> ATOM ... ATOM 
      

      ATOM -> pi
      ATOM -> e
      ATOM -> true
      ATOM -> false

      // e.g. 1.0, "text", 'text', 1
      ATOM -> CONSTANT
      // e.g. &1.1
      ATOM -> SYMBOL_NAME

      ATOM -> $(ARITHMETIC_EXPR)
      ATOM -> AGGREGATE_FUNCTION(SELECT_EXPR)
      ATOM -> ONEARG_FUNCTION(ARITHMETIC_EXPR)


      // # is not a char:       
      // Two atoms can be written without whitespace   
      // and their values will be String appended    
      // if the right atom begins with a char, 
      // which can't be a part of the left atom.     
      //
      // E.g. if the left atom is a number constant, 
      // and the right atom is a String constant 
      // beginning with a quote char,
      // the left atom value will be converted to the String 
      // and prepended to the right atom value.
      //
      // This rule doesn't apply inside ARITHMETIC_EXPR 
      ATOM ~~> ATOM#ATOM     

      
      // Converts all values to the String type and appends them.
      AGGREGATE_FUNCTION -> cat

      // Returns the number of values.
      AGGREGATE_FUNCTION -> count

      AGGREGATE_FUNCTION -> min
      AGGREGATE_FUNCTION -> max
      AGGREGATE_FUNCTION -> sum
      AGGREGATE_FUNCTION -> avg
      

      // All trigonometric functions are in radians.
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
      
      // Converts a value to the String type and returns its length.
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

      
      // A in B means A is a substring of B.
      OPERATOR -> in
      
      OPERATOR -> *
      OPERATOR -> /

      // General power
      OPERATOR -> **    
      // Natural power
      OPERATOR -> ^     
      
      // Integer division truncated towards minus infinity
      // (x div y)*y + (x mod y) == x
      OPERATOR -> div
      OPERATOR -> mod
      
      // Integer division truncated towards 0
      // (x quot y)*y + (x rem y) == x  
      OPERATOR -> quot
      OPERATOR -> rem

      // Greatest common divisor
      OPERATOR -> gcd
      // Least common multiple
      OPERATOR -> lcm
      
      // String append
      OPERATOR -> ++    
      
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

The first block is always the from block. If the block after the first block is without a specifier (`if`, `by`, or `sort`), then it is the select block. Otherwise, it is a block specified by the specifier.

The from block accepts a specific grammar (as specified in the grammar description), the select, the by, and the sort block accept the select expression (`SELECT_EXPR` in the grammar), 
and the if block accepts the arithmetic expression (`ARITHMETIC_EXPR` in the grammar).

Every source data file has a reference number based on its position in the from block and may have multiple names - the assign name, the name given to the source data file by `ASSIGN_NAME=FILE_PATH` syntax in the from block, and 
the default name, which is given by the path to the file or `-` in the case of the standard input in the from block.

Each column of a source data file has a reference number based on its position in it and may have a name (if the named option is enabled for the given source file). 

If a source data file with the reference number `M` (numbering input files from 1) has a name `XXX`, its columns can be addressed by `&M.N` or `XXX.N`, where `N` is the reference number of a column (numbering columns from 1). 
If the named option is enabled for the input file and a column has the name `NAME`, it can also be addressed by `&M.NAME` or `XXX.NAME`.

We call the address a symbol name - `SYMBOL_NAME` in the grammar description.

If there is a collision in naming (some symbol name addresses more than one column), then the behavior is undefined.


#### Exotic chars
Some chars cannot be in unquoted symbol names - exotic chars. For simplicity, we can suppose, they are all non-alphanumerical chars excluding `-`, `.`, `&`, and `_`. 
Also the first char of a symbol name must be non-numerical and must not be `-` or `.` to not be considered as an exotic char.

It is possible to use a symbol name with exotic chars using \` quote - like \`EXOTIC SYMBOL NAME\`. 

#### Quote chars
There are 3 quote chars (\`, " and ') used in LSQL. " and ' are always quoting a `String`. The \` quote char is used for quoting symbol names.

These chars can be used for `String` appending. If two atoms inside SELECT_EXPR are written consecutively without whitespace and the left atom ends by a quote char or the right begins by a quote char, 
they will be converted to the `String` and will be `String` appended. 
For example, `&1.1"abc"` means: convert the value of `&1.1` to the `String` and append it to the `String` constant `abc`.

#### Constants
Constants are in the grammar description as `CONSTANT`. In the following section, we speak only about these constants and not about built-in constant values like `pi` or `true`.

There are 3 datatypes of constants. `String`, `Double`, and `Int`. 
Every string quoted in " chars or ' chars in an LSQL command is always tokenized as a `String` constant. 
Numbers fully matching the POSIX ERE `[0-9]+` are considered `Int` constants and numbers fully matching the POSIX ERE `[0-9]+\.[0-9]+` `Double` constants.

#### Operator associativity and precedence
All operators are right-to-left associative.

The following list outlines the precedence of the `lsql-csv` infix operators. The lower the precedence number, the higher the priority.
* 1: `in`, `**`, `^`
* 2: `*`, `/`, `div`, `quot`, `rem`, `mod`, `gcd`, `lcm`
* 3: `++`, `+`, `-`
* 4: `<=`, `>=`, `<`, `>`, `!=`, `==`
* 5: `||`, `&&`



#### Select expression
Select expressions are in the grammar description as `SELECT_EXPR`.
They are similar to the `bash` expressions. They are made by atom selector expressions (`ATOM_SELECTOR`) separated by whitespaces. 
These expressions are wildcard and brace expanded to atoms (`ATOM`) and are further processed as they were separated by whitespace.
(In `bash` brace expansion is a mechanism by which arbitrary strings are generated. For example, `a{b,c,d}e` is expanded to `abe ace ade`, see [the `bash` reference manual](https://www.gnu.org/software/bash/manual/bash.html) for details.)

Wildcards and brace expansion expressions are only evaluated and expanded in unquoted parts of the atom selector expression,
which aren’t part of an inner arithmetic expression.

For example, if we have an LSQL command with symbol names `&1.1` and `&1.2`, then
* the atom selector expression `&1.{1,2}` will be expanded to `&1.1` and `&1.2`;
* the atom selector expression `&1.*` will be expanded to `&1.1` and `&1.2`;
* the atom selector expression `` `&1.*` `` will be expanded to `` `&1.*` ``;
* the atom selector expression `"&1.*"` will be expanded to `"&1.*"`;
* the atom selector expression `$(&?.1)` will be expanded to `$(&?.1)`;
* the atom selector expression `&*$(&1.1)` will be expanded to `&1.1$(&1.1)` and `&1.2$(&1.1)`.


Every atom selector expression can consist:
* A wildcard (Each wildcard is expanded against the symbol name list. If no symbol name matching the wildcard is found, the wildcard is expanded to itself.);
* A `bash` brace expansion expression (e.g. `{22..25}` -> `22 23 24 25`);
* An arithmetic expression in `$(expr)` format;
* A call of an aggregate function `AGGREGATE_FUNCTION(SELECT_EXPR)` - there cannot be any space after `FUNCTION`;
* A call of a one-argument function `ONEARG_FUNCTION(ARITHMETIC_EXPR)` — there cannot be any space after `FUNCTION`;
* A constant;
* A symbol name;
* A built-in constant value;
* A reference to a column name.

If you want to concatenate strings without `++` operator, you can write: `a.1","a.2`.

Please, keep in mind, that operators must be put inside arithmetic expressions, or they will be matched to a column name or aggregate function.

#### Arithmetic expression
Arithmetic expressions are in the grammar description as `ARITHMETIC_EXPR`.

The expressions use mainly the classical `awk` style of expressions.
You can use here operators `OPERATOR` keywords `>`, `<`, `<=`, `>=`, `==`, `||`, `&&`, `+`, `-`, `*`, `/`... 

Wildcards and brace expansion expressions are not evaluated inside the arithmetic expression.

#### Select blocks
Select blocks are referred in the grammar description as `SELECT_BLOCK`.
These blocks determine the output. They accept the select expression.

There must be at least one select block in an LSQL command, which refers to at least one symbol name, or the behavior is undefined.

Examples of select blocks:

    &1.[3-6]

This will print the 3rd, 4th, 5th, and 6th columns from the first file if the first file has at least 6 columns. 

    ax*.{6..4}

This will print the 6th, the 5th, and the 4th columns from all files whose name begins with ax if the files have at least 6 columns.



#### From blocks
These blocks are in the grammar description as `FROM_BLOCK`.
There must be exactly one from block at the beginning of an LSQL command. 

The from block contains input file paths (or `-` in the case of the standard input), and optionally their assign name `ASSIGN_NAME`. 

You can use the wildcards and the curly bracket expansion as you were in the `bash` to refer input files. 
If there is a wildcard with an assign name `NAME` matching more than one input file, the input files will be given assign names `NAME`, `NAME1`, `NAME2`...
If there is a wildcard, that matches to no file, it is expanded to itself. 

If `FILE_PATH` is put inside \` quotes, no wildcard or expansion logic applies to it.

You can also add custom attributes to input files in the format `FILE_PATH -aX --attribute=X -b`. The attributes will be applied to all files which will be matched against `FILE_PATH`.
The custom attributes are referred to as `OPTIONS` in the grammar description.

Examples:

    /etc/{passwd,group}


This will select `/etc/passwd` and `/etc/group` files. They can be addressed either as `&1` or `/etc/passwd`, and `&2` or `/etc/group`.

    passwd=/etc/passwd

This will select `/etc/passwd` and set its assign name to `passwd`. It can be addressed as `&1`, `passwd`, or `/etc/passwd`.




##### Possible attributes
    
    -n
    --named

Enables the first-line naming convention for an input CSV file.
With this option, the first line of a CSV file will be interpreted as a list of column names.


    -N
    --not-named

You can also set the exact opposite to an input file. This can be useful if you change the default behavior.

    -dCHAR
    --delimiter=CHAR
    
This changes the primary delimiter of an input file.

    -sCHAR
    --secondary-delimiter=CHAR
    
This changes the secondary delimiter of an input file.

Example:

    /etc/passwd -d:

This will select `/etc/passwd` and set its delimiter to `:`.

Currently, commas and `CHAR`s, which are also quotes in LSQL, are not supported as a delimiter or a secondary delimiter in `FILE_PATH` custom attributes.


#### If blocks
These blocks are in the grammar description as `IF_BLOCK`.
They always begin with the `if` keyword.        
They accept arithmetic expressions, which should be convertible to `Bool`: 
either `String` `false`/`true`, `Int` `0` `false`, anything else `true`), or `Bool`. 

Rows with the arithmetic expression converted to `Bool` `true` are printed or aggregated, and
rows with the arithmetic expression converted to `Bool` `false` are skipped.

Filtering is done before the aggregation.

You can imagine the if block as the `WHERE` clause in SQL.


#### By blocks
By blocks are referred in the grammar description as `BY_BLOCK`.
These blocks always begin with the `by` keyword. They accept the select expression.

There can be only one by block in a whole LSQL command.

The by block is used to group the resulting set by the given atoms for the evaluation by an aggregate function.
The by block is similar to the `GROUP BY` clause in SQL.

There must be at least one aggregate function in the select block if the by block is present. Otherwise, the behavior is undefined.

If there is an aggregate function present without the by block present in an LSQL command, the aggregate function runs over all rows at once.


#### Sort blocks
These blocks are in the grammar description as `SORT_BLOCK`.
It begins with the `sort` keyword. They accept the select expression.

The sort block determines the order of the final output - given atoms are sorted in ascending order.
If there is more than one atom in the sort block (`A`, `B`, `C`...), the data is first sorted by `A` 
and in the case of ties, the atoms (`B`, `C`...) are used to further refine the order of the final output.

You can imagine the sort block as the `ORDER BY` clause in SQL.

There can be only one sort block in the whole command.

