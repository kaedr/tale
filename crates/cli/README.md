# Getting Started with TALE

## Running the application
TALE is a Command Line Interface (CLI) application, meaning it needs to be run at your system's terminal, whatever that may be. How to use a terminal is beyond the scope of this tutorial. Luckily, the internet has many resources available to help if you're not familiar.

Tale can be run on its own
```
./tale
```
Or preloaded with tables from files:
```
./tale some_tables.tale more_tables.tale
```

## TALE basics
The application relies on Tables, Table Groups, and Scripts. These currently **MUST** be made in a basic text editor (like Notepad/Gedit/vim/etc. NOT a word processor). By convention, they are saved in files with a `.tale` extension. When getting started, you will most likely find it easiest to just save these files in the same directory as the TALE executable, so that loading them is more straightforward.


### Defining Tables
For examples on how Tables can be defined, see [Table Basics](src/snippets/ex01_table_basics.tale), [List Tables](src/snippets/ex02_table_list.tale), [Table Probabilities](src/snippets/ex03_table_probabilities.tale), [Tables w/ comma separated numeric keys](src/snippets/ex04_table_csv_keys.tale), [Textually Keyed Lookup Tables](src/snippets/ex05_table_lookup.tale), [Table organization Tags](src/snippets/ex06_table_tags.tale), and [Tables w/ Statements](src/snippets/ex07_table_statements.tale).
These examples are also available directly in the application via built-in help.

**Table Naming:**
Table names may contains spaces (e.g. `Farm Animals`), and be alphanumeric (e.g. `10gp Gems`). They *MAY NOT* contain punctuation without being in quotes (e.g. `"Treasure Hoard: Challenge 0-4"`).


### Defining Table groups
For examples on how to define Table Groups, see [Table Group Basics](src/snippets/ex11_table_group_basics.tale), and [Table Groups w/ Statements](src/snippets/ex12_table_group_statements.tale).


### Defining Scripts
For examples of how to define Scripts, see [Script Basics](src/snippets/ex21_script_basics.tale).


### Using TALE

Once the application is running, you should see the welcome prompt:
```
-----+- Welcome to TALE! ---->
     |   Type a command to get started, 'help' if you're unsure what to do, or CTRL+C to exit.
TALE +->
```
From here, the most common action is rolling. This can be either just rolling dice:
```
TALE +-> Roll 3d6
```
Or rolling on a defined table:
```
TALE +-> Roll on Magic Item Table A
```

### Basic Interaction
TALE evaluates (or tries to) whatever statements you enter into the prompt, then shows the output of that evaluation. These could be simple [Arithmetic Expressions](src/snippets/ex31_expressions.tale), [Roll Expressions](src/snippets/ex32_rolls.tale), [Lookup Expressions](src/snippets/ex33_lookup.tale), [Load Statements](src/snippets/ex41_load.tale) or one of TALE's more [Advanced Features](Advanced_Features.md). Any of these expressions or statements can also be included in `.tale` files. They can either be directly in the body of the file, and executed when it is loaded, or within Tables or Scripts, and executed when rolled/invoked.
