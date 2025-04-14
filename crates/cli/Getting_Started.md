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
The application relies on Tables, Table Groups, and Scripts. These currently must be made in a basic text editor (like Notepad/Gedit/vim/etc. NOT a word processor).

For examples on how Tables can be defined, see [Table Basics](src/snippets/ex01_table_basics.tale), [List Tables](src/snippets/ex02_table_list.tale), [Table Probabilities](src/snippets/ex03_table_probabilities.tale), [Tables w/ comma separated numeric keys](src/snippets/ex04_table_csv_keys.tale), [Textually Keyed Lookup Tables](src/snippets/ex05_table_lookup.tale), and [Table organization Tags](src/snippets/ex06_table_tags.tale).
These examples are also available directly in the application via built-in help.

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
