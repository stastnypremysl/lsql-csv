# lsql-csv
`lsql-csv` is a tool for CSV files data querying from shell with short queries. It makes possible to work with small CSV files like with read-only relational database.

The tool implements a new language LSQL similar to SQL, which is type-less, specifically designed for working with CSV files in shell. 

## Installation
It is necessary, you had GHC (`>=8 <9.29`) and Haskell packages Parsec (`>=3.1 <3.2`), Glob (`>=0.10 <0.11`), base (`>=4.9 <4.17`), text (`>=1.2 <2.1`) and containers (`>=0.5 <0.7`)
 installed. (The package boundaries given are identical to cabal bounderies.) Run then:

    make
    sudo make install
    
Now the lsql-csv is installed in `/usr/local/bin`. If you want, you can specify `INSTALL_DIR` like:

    sudo make INSTALL_DIR=/custom/install-folder install

This will install the package into `INSTALL_DIR`.

If you have installed `cabal`, you can alternatively run:

    cabal install
   
It will also install the dependencies for you.    

### Running the unit tests
If you want to verify, that the package has compiled correctly, it is possible to test it by running:

    make test

This will run all unit tests for you.


## lsql-csv - quick introduction 
LSQL, the language of `lsql-csv`, aims to be more lapidary language than SQL. The design purpose of it is to enable it's user to fast write simple queries directly to the terminal - it's design purpose is therefore different from SQL, where readability of queries is more taken in account than in LSQL.


### Examples
One of the way, how to learn the new programming language is by understanding many concrete examples of its usage. The following examples are written explicitly for the purpose - to teach a reader, how to use the tool `lsql-csv` by showing him many examples of its usage. 

The following examples might be not enough for reader, who don't know Unix/Linux scripting enough. If this is the case, please consider learning Unix/Linux scripting first before LSQL.

It is also advantageous to know SQL.

The following examples will be mainly about parsing of `/etc/passwd` and parsing of `/etc/group`. You may look at `man 5 passwd` and `man 5 group` to see what columns it contain.

#### Hello World

    lsql-csv '-, &1.2 &1.1'

This will print second (&1.2) and first column (&1.1) of csv file on stdin. If you know SQL, you can read it like `from stdio S select S.second, S.first`. 

So, as you can see, the first block is (*and always is*) the from block. There are file names or `-` (stdin) separated by space. The second block is the select block, also separated by space.

For example:

    lsql-csv '-, &1.2 &1.1' <<- EOF
    World;Hello
    EOF
    
It returns:
    
    Hello;World

#### Simple filtering 
    lsql-csv -d: '-, &1.*, if &1.3>=1000' < /etc/passwd

This will print lines of users whose UID >=1000. It can be also written as:
  
    lsql-csv -d: 'p=/etc/passwd, p.*, if p.3 >= 1000'
    
    lsql-csv -d: 'p=/etc/passwd, &1.*, if &1.3 >= 1000'

    lsql-csv -d: '/etc/passwd, &1.*, if &1.3 >= 1000'
    
In previous examples we used overnaming, which allows us to give a data source file `/etc/passwd` give a name `p`.

If you know SQL, you can read it as `from /etc/passwd P select * where P.UID >= 1000`. As you can see, lsql style is more compressed then standard SQL.
    
The output might be:

    nobody:x:65534:65534:nobody:/var/empty:/bin/false
    me:x:1000:1000::/home/me:/bin/bash

If you specify delimiter specifically for `/etc/passwd`, the output will be comma delimited.
    
    lsql-csv '/etc/passwd -d:, &1.*, if &1.3 >= 1000'

It might return:

    nobody;x;65534;65534;nobody;/var/empty;/bin/false
    me;x;1000;1000;;/home/me;/bin/bash


#### Named columns
Let's suppose a file people.csv:
   
    name;age
    Adam;21
    Petra;23
    Karel;25

Now, let's get all the names of people in people.csv:

    lsql-csv -n 'people.csv, &1.name'

The output will be:

    Adam
    Petra
    Karel

As you can see, we can reference named columns by a name. If named columns are enabled, each column have two names under &X - the number name &X.Y and actual name &X.NAME.

Now, we can select all columns with wildcard `&1.*`:
    lsql-csv -n 'people.csv, &1.*'

As the output, we get
    Adam;21;21;Adam
    Petra;23;23;Petra
    Karel;25;25;Karel

The output contains each column twice, because wildcard `&1.*` was evaluated to `&1.name, &1.age, &1.1, &1.2`.
How to fix it?

    lsql-csv -n 'people.csv, &1.[1-9]*'

The output is now:

    Adam;21
    Petra;23
    Karel;25

The command can be also written as

    lsql-csv -n 'people.csv, &1.{1,2}'
    lsql-csv -n 'people.csv, &1.{1..2}'

The output will be in the both cases still the same.


#### Simple join

Lets say, I am interested in the default group names of users. We need to join to tables: `/etc/passwd` and `/etc/group`. Let's do it.

    lsql-csv -d: '/etc/{passwd,group}, &1.1 &2.1, if &1.4 == &2.3'
    
What does `/etc/{passwd,group}` mean? Basically, there are three types of expressions. Select, from and arithmetic expression. In all select and from expressions, you can use expansion and wildcards just like in bash.
    
Finally, the output can be something like this:

    root:root
    bin:bin
    daemon:daemon
    me:me

First column is name of user and the second column is name of its default group.
    
#### Basic grouping
Let's say, I want do count users using the same shell. 

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
Lets say, you want to sort your users with UID greater than or equal to 1000 descendingly.

    lsql-csv -d: '/etc/passwd, &1.*, if &1.3 >= 1000, sort &1.3' | tac

The output might look like:
  
    nobody:x:65534:65534:nobody:/var/empty:/bin/false
    me3:x:1002:1002::/home/me3:/bin/bash
    me2:x:1001:1001::/home/me2:/bin/bash
    me1:x:1000:1000::/home/me1:/bin/bash

The sort block is the equivalent of `order by` in SQL. The `tac` command print the lines in reverse order.
    
#### About nice outputs
There is a trick, how to concat two values in select expression: Write them without space.

But how the interpreter knows the ends of the value name or value expression? You must use quotes for it - quotes itself can't be part of value name.
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

So far, we just met all kinds of blocks and only if block accepting arithmetic expression and the other accepting select expression. 
What if we needed to run arithmetic expression inside select expression. There is a special syntax `$(...)` for it.

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
    
This will print all pairs user <-> group excluding the default group. You can read it as `from /etc/passwd P, /etc/group G select P.1, G.1 where P.1 in G.4`.

How does `in` works? It's one the basic string level "consist".

And the output?

    root:root
    root:wheel
    root:floppy
    root:tape
    lp:lp
    halt:root
    halt:wheel

#### More complicated...

The previous example don't give much readable output. We can use `group by` to improve it (shortened as `g`).

    lsql-csv -d: 'p=/etc/passwd g=/etc/group, p.1 cat(g.1","), if p.1 in g.4, by p.1'

The output will be something like:
    
    adm:adm,disk,sys,
    bin:bin,daemon,sys,
    daemon:adm,bin,daemon,
    lp:lp,
    mythtv:audio,cdrom,tty,video,
    news:news,
    
This will cat all groups in one line delimeted by ",". 

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



## Usage
Now, if you understood the examples, is the time to move forward to more abstract description of the language and tool usage.

    lsql-csv [OPTIONS] COMMAND
    
    Approximate scatch of the grammar
    
      COMMAND -> FROM_BLOCK, REST
    
      REST -> SELECT_BLOCK, REST
      REST -> BY_BLOCK, REST
      REST -> SORT_BLOCK, REST
      REST -> IF_BLOCK, REST
      REST ->
    
      FROM_BLOCK -> FROM_SELECTOR FROM_BLOCK
      FROM_SELECTOR -> FROM ... FROM //Wildcard and brace expansion

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
      ARITHMETIC_EXPR -> ARITHMETIC_EXPR TWOARG_FUNCTION ARITHMETIC_EXPR
      ARITHMETIC_EXPR -> (ARITHMETIC_EXPR)
      
      SELECT_EXPR -> ATOM_SELECTOR SELECT_EXPR
      SELECT_EXPR ->
      
      ATOM_SELECTOR ~~> ATOM ... ATOM   // Wildcard and brace expansion
      
      ATOM -> CONSTANT
      ATOM -> COL_SYMBOL
      ATOM -> $(ARITHMETIC_EXPR)
      ATOM -> AGGREGATE_FUNCTION(SELECT_EXPR)
      ATOM -> ONEARG_FUNCTION(ARITHMETIC_EXPR)

      // # is not really char:
      // two atoms can be written without space and will be (string) appended, 
      // if they are separated using quote chars:
      //   left atom must end or right atom must begin with quote char
      ATOM -> ATOM#ATOM     

      
      AGGREGATE_FUNCTION -> cat
      AGGREGATE_FUNCTION -> sum
      AGGREGATE_FUNCTION -> count
      AGGREGATE_FUNCTION -> max
      AGGREGATE_FUNCTION -> min
      AGGREGATE_FUNCTION -> avg
      
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
      
      ONEARG_FUNCTION -> size
      ONEARG_FUNCTION -> to_string
      
      ONEARG_FUNCTION -> negate
      ONEARG_FUNCTION -> abs
      ONEARG_FUNCTION -> signum
      
      ONEARG_FUNCTION -> round
      ONEARG_FUNCTION -> truncate
      ONEARG_FUNCTION -> ceiling
      ONEARG_FUNCTION -> floor
      
      ONEARG_FUNCTION -> even
      ONEARG_FUNCTION -> odd
      
      // A in B means A is a substring of B
      TWOARG_FUNCTION -> in
      
      TWOARG_FUNCTION -> *
      TWOARG_FUNCTION -> **    //general power
      TWOARG_FUNCTION -> ^     //natural power
      TWOARG_FUNCTION -> /
      
      TWOARG_FUNCTION -> div
      TWOARG_FUNCTION -> quot
      TWOARG_FUNCTION -> rem
      TWOARG_FUNCTION -> mod
      TWOARG_FUNCTION -> gcd
      TWOARG_FUNCTION -> lcm
      
      TWOARG_FUNCTION -> ++    //append
      
      TWOARG_FUNCTION -> +
      TWOARG_FUNCTION -> -
      
      TWOARG_FUNCTION -> <=
      TWOARG_FUNCTION -> >=
      TWOARG_FUNCTION -> <
      TWOARG_FUNCTION -> >
      TWOARG_FUNCTION -> !=
      TWOARG_FUNCTION -> ==
      
      TWOARG_FUNCTION -> ||
      TWOARG_FUNCTION -> &&
      

### Options

    -h
    --help

Shows short command line help and exits before doing anything else.

    -n
    --named

Enables first line naming convension in csv files. This works only on input files. 
Output is always without first line column names.
    
    -dCHAR
    --delimiter=CHAR

Changes default primary delimiter. The default value is ';'.

    -sCHAR
    --secondary-delimiter=CHAR
    
Changes default quote char (secondary delimiter). The default value is '"'.

### Datatypes
There are 4 datatypes considered: Bool, Int, Double, String. 
Bool is either true/false, Int is at least 30 bit integer, Double double-precision floating point number and String is a ordinary char string.

### Joins
Joins have always the time complexity O(nm). There is no optimization made, when you put multiple files into from block.

### Documentation of language
Each command is made from blocks separated by comma. There are these types of blocks.
* From block
* Select block
* If block
* By block
* Sort block

First block is always from block. If block after first block is without specifier (`if`, `by` or `sort`), then it is select block. Otherwise it is block specified by the specifier.

From block accept specific grammar (as specified in the scratch), select, by and sort block select expression and if block arithmetic expression.

Every source file have a number and may have multiple names - assign name, the name given to the source file by `ASSIGN_NAME=FILE_PATH` syntax in from block, and 
default name, which is given path to the file or `-` in case of stdin in from block.

Each column of a source file have a number and may have name (if named option is enabled for the given source file). 

If the source file with index M (numbering input files from 1) have been given a name XXX, it is columns can be addressed by &M.N, XXX.N, where N is the index of column (numbering columns from 1). 
If named option is enabled and a column have name `NAME`, it can be also addressed by &M.NAME or XXX.NAME.

If there is collision in naming (two source file have same name or two columns under the same source file have same name), then the behavior is undefined.

If you want to write exotic identifiers/names, put them in \`EXOTIC NAME\`. Exotic names are names, which contains exotic characters.

#### Exotic chars
There are some chars which cannot be in symbol names (column names). For simplicity, we can suppose, they are everything but alphanumerical chars excluding `-`, `.`, `&` and `_`. 
Referencing names containing exotic chars without quotes is unsupported.

It is possible to reference columns with name with exotic chars using \` quote - like \`EXOTIC NAME\`. The source file name is always part of column name from the syntax perspective of language - it must be inside the quotes.

#### Quote chars
There are 3 quotes (\`, " and ') used in Lsql. " and ' are always quoting a string. The \` quote is used for quoting symbol names.

These chars can be used for fast appending. If two atoms are written without space and are separated using the quotes, they will be appended. For example `abc"abc"` means: append column abc to the string abc.

#### Select expression
They are similar to bash expressions. They are made by atom selector expressions separated by whitespaces. These expressions are expanded, evaluated and matched to column name, aggregate functions or arithmetic expressions.

Every atom selector expression can consist
* Wildcard (Each wildcard will be expanded to multiple statements during processing)
* Bash brace expansion (e.g. {22..25} -> 22 23 24 25)
* Arithmetic expression in `$(expr)` format
* Quotes \`anything\` to prevent wildcards and expansions
* Quotes " or ' to insert string
* Call of aggregate function `AGGREGATE_FUNCTION(next select block)` - there cannot be any space after FUNCTION
* Call of single arg function `ONEARG_FUNCTION(arithmetic expression)` - there cannot be any space after FUNCTION
* Reference to a column name

Please, keep in mind, that all integers, floats and booleans constants and nonaggregate functions must be put inside arithmetic expression, or they will be matched to a column name or aggregate function.

#### Arithmetic expression
The statement uses classical awk logic. You can use keywords >, <, <=, >=, ==, ||, &&, +, -, \*, /, div, mod,... You must also quote all string, or they can be behaved as numbers, booleans or matched to column names.

The only significant difference with awk are two args functions, which are called like `5 mod 2`. 

#### Select blocks
These blocks determine output. They accept select expression and are evaluated and printed in delimitered format. 

Every select block must contain at least one reference to column name, or behavior is undefined.

Examples of select blocks:

    &1.[3-6]

This will print column 3, 4, 5 and 6 from first file.

    ax*.{6..4} 
    
This will print 6th, 5th, 4th of all files which name begins with ax.

If you want to concatenate strings without cat, you can write `a.1","a.2`.

#### From blocks
There must be exactly one from block in the beginning of the command. The block can contain any files (and stdio in `-` format). You can use any syntax you would otherwise use in bash to select these files (wildcards, expansion,...). You can also overname the file using `NAME=stmt`. If there are more than 1 matching of stmt, the files will be named `(NAME, NAME1, NAME2,...)`.

Example:

    /etc/{passwd,group}
    
This will select `/etc/passwd` and `/etc/group` file. They can be addressed ether as `&1` or `/etc/passwd`, and `&2` or `/etc/group`.

If `filename` is put inside \` quotes, no wildcard or expansion logic will apply to it.

You can also add custom attributes to files in formats `FILE -a "xyz" --attribute="z" -a;`. The attributes will be applied to all files which will be matched using `FILE` bash expression.

##### Possible attributes
    
    -n
    --named

It means that csv file have first line with names of columns

    -N
    --not-named

You can also set the exact opposite. This can be useful, if you changed the default behavior.

    -dCHAR
    --delimiter=CHAR
    
This change the primary delimiter.

    -sCHAR
    --secondary-delimiter=CHAR
    
This change the secondary delimiter char.

Example:

    /etc/passwd -d:

Currently, CHARs, which are also quotes in Lsql, are not supported.
    
#### If block
This block always begins with if. They accept arithmetic expression, which should be convertable to bool - either string "false"/"true", int (0 false, anything else true) or bool. 

Filtering is done before the aggregation.

You can imagine if statement as where clause in SQL.

#### By block
This statement always begins with `by` and the rest of the block is select expression. There can be only one By block in the whole command.

You can imagine by block as the group by clause in SQL. 

There must be present at least one aggregate function in select block, if by block is present. Otherwise behavior is undefined.

If there is an aggregate function present without by block present, aggregation runs over all rows at once.

#### Sort block
This block can be at the end of the command. It begins with `sort` keyword and the rest is select expression.

You can imagine by sort as the order by clause in SQL.

There can be only one Sort block in the whole command.
