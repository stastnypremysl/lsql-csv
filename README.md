# lsql-csv
`lsql-csv` is a tool for CSV files data querying from shell with short queries. It makes possible to work with small CSV files like with read-only relational database.

The tool implements a new language LSQL similar to SQL, which is type-less, specifically designed for working with CSV files in shell. 

## Installation
It is necessary, you had GHC (`>=8 <9.29`), Parsec (`>=3.1 <3.2`) and Glob (`>=0.10, <0.11`) installed. Run then

    make
    sudo make install
    
Now the lsql-csv is installed in `/usr/local/bin`.

If you have installed `cabal`, you can alternatively run

    cabal install
   
It will also install the dependencies for you.    

### Running the unit tests
If you want to verify, that the package has compiled correctly, it is possible to test it by running

    make test

This will run all unit tests for you.


## lsql-csv - quick introduction 
LSQL, the language of `lsql-csv`, aims to be more lapidary language than SQL. The design purpose of it is to enable it's user to fast write simple queries directly to the terminal - it's design purpose is therefore different from SQL, where readability of queries is more taken in account than in LSQL.


### Examples
One of the way, how to learn the new programming language is by understanding many concrete examples of its usage. The following examples are written explicitly for the purpose - to learn a reader, how to use the tool `lsql-csv` by showing him many examples of its usage. 

The following examples might be not enough for reader, who don't know SQL or Unix/Linux scripting enough. If this is the case, please consider learning SQL and/or Unix/Linux scripting first before LSQL.

#### Hello World

    lsql-csv '-, &1.2 &1.1'

This will print second and first column of csv file on stdin. You can read it like `from stdio S select S.second, S.first`. 

So, as you can see, the first block is (*and always is*) the from block. There are file names or `-` (stdin) separated by space. The second block is the select block, also separated by space.

For example

    lsql-csv '-, &1.2 &1.1' <<- EOF
    World;Hello
    EOF
    
Returns
    
    Hello;World

#### Simple filtering 
    lsql-csv -d: '-, &1.*, if &1.3>=1000' < /etc/passwd

This will print lines of users whose UID >=1000. It can be also written as
  
    lsql-csv -d: 'p=/etc/passwd, p.*, if p.3 >= 1000'
    
    lsql-csv -d: 'p=/etc/passwd, &1.*, if &1.3 >= 1000'

    lsql-csv -d: '/etc/passwd, &1.*, if &1.3 >= 1000'
    

You can read it as `from /etc/passwd P select * where P.UID >= 1000`. As you can see, lsql style is much more compressed then standard SQL.
    
The output might be 

    nobody:x:65534:65534:nobody:/var/empty:/bin/false
    me:x:1000:1000::/home/me:/bin/bash

If you specify delimiter specifically for `/etc/passwd`, the output will be comma delimetered.
    
    lsql-csv '/etc/passwd -d:, &1.*, if &1.3 >= 1000'

Might return

    nobody;x;65534;65534;nobody;/var/empty;/bin/false
    me;x;1000;1000;;/home/me;/bin/bash


#### Simple join

Lets say, I am interested in the default group names of users. We need to join to tables: `/etc/passwd` and `/etc/group`. Let's do it.

    lsql-csv -d: '/etc/{passwd,group}, &1.1 &2.1, if &1.4 == &2.3'
    
What does `/etc/{passwd,group}` mean? Basically, there are two-three types of expressions. Select (and from) expression and arithmetic expression. In all select blocks, you can use expansion and wildcards just like you were in bash.
    
Finally, the output can be something like this

    root:root
    bin:bin
    daemon:daemon
    me:me

where first column is name of user and the second column is name of its default group.
    
#### Basic grouping
Let's say, I want do number of users using the same terminal. 

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
Let's say, you want to sort your users with UID greater than or equal to 1000 descendingly.

    lsql-csv -d: '/etc/passwd, &1.*, if &1.3 >= 1000, sort &1.3' | tac

The output might look like
  
    nobody:x:65534:65534:nobody:/var/empty:/bin/false
    me3:x:1002:1002::/home/me3:/bin/bash
    me2:x:1001:1001::/home/me2:/bin/bash
    me1:x:1000:1000::/home/me1:/bin/bash

The sort block is the equivalent of `order by` in SQL. The `tac` command print the lines in reverse order.
    
#### About nice outputs
There is a utterly sick trick, how to concat two values in select expression. Write them without space. No, we are not joking. Never.

But how the interpreter know the interpreter knows the ends of the value name or value expression? You must hint it!
There are many ways how to do it using the exotic chars, but the most easy one is just write "".

Let's try it!

    lsql-csv '/dev/null, "I did not steal it...""I just borrowed it"'
    
The output is, as you might expect
   
    I did not steal it...I just borrowed it

#### About the engineering way of theorem proving

Let's say you want to build a new bridge, but you aren't sure whether `sin(x)^2 + cos(x)^2` is really 1.
To verify this theorem, we need to make, let's say, 10 experiments. Then we compute the average and if it is 1 plus minus 0.1, the theorem is true.

The naive solution is just to write

    lsql-csv -d: '/etc/passwd, $(sin(&1.3)^2 + cos(&1.3)^2)'
    
Don't miss the new syntax here `$(...)`. This is the way, how you can insert arithmetic expression into the select expression. But you are too lazy to use your calculator, so you need to improve this program.

    lsql-csv -d: '/etc/passwd, "sin(x)^2 + cos(x)^2 = "avg($(sin(&1.3)^2 + cos(&1.3)^2))'
    
And the output is, suprisingly

    sin(x)^2 + cos(x)^2 = 1.0

#### More complicated join

Let's see more complicated examples.

    lsql-csv -d: 'p=/etc/passwd g=/etc/group, p.1 g.1, if p.1 in g.4'
    
This will print all all pairs user <-> group excluding the default group. You can read it as `from /etc/passwd P, /etc/group G select P.1, G.1 where P.1 in G.4`.

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

But this will give not much readable output. We can use `group by` to improve it (shortened as `g`).

    lsql-csv -d: 'p=/etc/passwd g=/etc/group, p.1 cat(g.1", "), if p.1 in g.4, by p.1'
    
This will cat all groups in one line delimeted by ", ". But I want there also default groups! How can it be done? Really easily.

    lsql-csv -d: 'p=/etc/passwd g=/etc/group, p.1 cat(g.1", "), if p.1 in g.4, by p.1' |
    lsql-csv -d: '- /etc/passwd /etc/group, &1.1 &1.2""&3.1, if &1.1 == &2.1 && &2.4 == &3.3'
    
What a nice oneliner (twoliner)! Try it yourself!



## Usage

    lsql-csv [OPTIONS] COMMAND
    
    Approximate scatch of the grammar
    
      COMMAND -> FROM_BLOCK, REST
    
      REST -> SELECT_BLOCK, REST
      REST -> BY_BLOCK, REST
      REST -> SORT_BLOCK, REST
      REST -> IF_BLOCK, REST
      REST ->
    
      FROM_BLOCK ~~> SELECT_EXPR  //not really, but similar princips
      
      SELECT_BLOCK -> SELECT_EXPR
      BY_BLOCK -> by SELECT_EXPR
      SORT_BLOCK -> sort SELECT_EXPR
      IF_BLOCK -> if ARITMETIC_EXPR
    
 
      ARITMETIC_EXPR -> ATOM
      ARITMETIC_EXPR -> ONEARG_FUNCTION(ARITMETIC_EXPR)
      ARITMETIC_EXPR -> ARITMETIC_EXPR TWOARG_FUNCTION ARITMETIC_FUNCTION
      ARITMETIC_EXPR -> (ARITMETIC_EXPR)
      
      SELECT_EXPR -> ATOM_SELECTOR SELECT_EXPR
      SELECT_EXPR ->
      
      ATOM_SELECTOR ~~> ATOM ... ATOM   //Wildcard and expansion magic
      
      ATOM -> CONSTANT
      ATOM -> COL_SYMBOL
      ATOM -> $(ARITMETIC_EXPR)
      ATOM -> AGGREGATE_FUNCTION(SELECT_EXPR)
      ATOM -> ATOM#ATOM     //# is not really char...two atoms can be written without space and will be appended, if they can be separated by compiler using exotic chars
      
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
      
      TWOARG_FUNCTION -> =>=   //left outer join - not working yet
      
      TWOARG_FUNCTION -> <=
      TWOARG_FUNCTION -> >=
      TWOARG_FUNCTION -> <
      TWOARG_FUNCTION -> >
      TWOARG_FUNCTION -> !=
      TWOARG_FUNCTION -> ==
      
      TWOARG_FUNCTION -> ||
      TWOARG_FUNCTION -> &&
      

### Options

    -n
    --named

Enables first line naming convension in csv files.
    
    -dCHAR
    --delimiter=CHAR

Changes default primary delimiter. The default value is ';'.

    -sCHAR
    --secondary-delimiter=CHAR
    
Changes default default quote char (secondary delimiter). The default value is '"'.


### Documantion of language
We suppose, you have already read everything before this section and you understood it.

Each column have number and may have name. If the source is file and have been given a name XXX by a user and can be addressed by XXX.N or XXX.NAME. It can be also addressed using &M.N syntax, where M is m-th input file or stdio.

Each command is made from blocks separated by comma. There are these types of blocks.

If you want to write exotic identifiers/names, put them in \`EXOTIC NAME\`. Exotic names are names, which contains exotic characters.

#### Exotic chars
There are some chars which can't be in symbol names (column names). For simplicity, you can suppose, they are everything but alphanumerical chars excluding `-` and `_`.

These chars can be used for fast appending. If two atoms are written without space and can be separated by compiler using the exotic chars, they will be appended. For example `abc"abc"` means: append column abc to the string abc.

#### Select blocks
These blocks determine output. They are similar to bash expressions. They are made by statements separeted by whitespaces. These statements are expanded, evaluated, matched to column name and printed in delimitered format.

Each statement can consist
* Wildcard (Each wildcard will be expanded to multiple statements during processing)
* Bash brace expansion (e.g. {22..25} -> 22 23 24 25)
* Aritmetic expression in `$(expr)` format
* Quotes "anything" to prevent wildcards, expansions and matching
* Overnaming (alias) in format `NAME=stmt` (NOT SUPPORTED YET)
* Call of aggregate function `AGGREGATE_FUNCTION(next select block)` - there can't be any space after FUNCTION

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

    -dCHAR
    --delimiterCHAR
    
This change the primary delimiter.

    -sCHAR
    --secondary-delimiter=CHAR
    
This change the secondary delimiter char.

Example:

    /etc/passwd -d:
    
### If block
This block always begins with if. The statement uses classical awk logic. You can use keywords >, <, <=, >=, ==, ||, &&, +, -, \*, /, div, mod,... You must also quote all string, or they can be behaved as numbers.

There are also new nonstandard keywords:
* `A in B` - means that A is substring of B
* `A.X =>= B.Y` - means that `A` is left outer joined on `B` with condition `A.X == B.Y` - *It can't be negated. Not supported yet.*

You can imagine if statement as where clausule in SQL.

### By block
This statement always begins with `by` and the rest of statement follows the same syntax as Select block. There can be only one By block in the whole command.

You can imagine by block as the group by clausule in SQL.

### Sort
This block can be at the end of the command. It begins with `sort` keyword and the rest is almost the same as the select block.


