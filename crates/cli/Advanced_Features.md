# Advanced Features

## Displaying information with `show`

### Built-ins & Defined Identifiers
There are a number of built in terms that the `show` command can operate on.

You can use it to see what Table are currently defined:
```
TALE +-> Show Tables
     |       Defined Tables:
     |       `farm animals`, 1d4, 4 Rows
     |       `cardinal directions`, 1d4, 4 Items
     |       `ordinal directions`, 1d4, 4 Items
```
The same is true for Scripts:
```
TALE +-> Show Scripts
     |       Defined Scripts:
     |       `starting gear`, 4 Statements
```
Or generally get a list of all defined symbols:
```
TALE +-> Show names
TALE +-> Show Identifiers
     |       Defined Identifiers:
     |       currentduchy
     |       duchy
     |       duchy npc
     |       foreign nations
```

From there, once you've gotten the name of a stored value, you can use `show` to see what value is stored:
```
TALE +-> show currentduchy
     |     norlands
```

### Tags
The `show` command can also be used to show can Tables that match a tag or set of tags:
```
TALE +-> show tag desert
     |       `cr0 desert monsters`, 1d7, 7 Items
TALE +-> show tags desert monsters
     |       `cr0 desert monsters`, 1d7, 7 Items
```


## Adding and clearing modifiers to Table rolls with `modify` and `clear`
`Modify` statements can either apply to all of the rolls on a given Table, or just a certain number of the rolls. To modify all rolls, you can use:
```
TALE +-> Modify All Quality Rolls +2
```
Or, to modify the next 3 rolls, you would use:
```
TALE +-> Modify Next 3 Quality Quality Rolls -4
```
There's also an alternative form where the modifier is in the leading position, followed by the keyword `To`:
```
TALE +-> +3 To Next 5 Quality Rolls
```
When using the leading modifier form, if you don't specify `Next`/`All`, it is implied to be `All`. So the following two statements are equivalent:
```
TALE +-> -1 To All Quality Rolls
TALE +-> -1 To Quality Rolls
```
---
`Clear` statements are used to remove modifiers that have been applied to the given table. To clear all the modifiers from a table:
```
TALE +-> Clear All Quality Rolls
```
Or to clear just a certain number of modifiers (as if you had rolled that many times):
```
TALE +-> Clear Next 2 Quality Rolls
```


## Output statements and Interpolation expressions
`Output` statements are useful for displaying additional information or context within tale. They aren't particularly useful when just used at the prompt, but really shine within Scripts.

For example, say you wanted to provide a name to a group of rolls, or divide them from another group, you could so something such as:
```
Script: Example
    Output: Doing mischief
    Roll on Some Random Table
    Roll on Something Else
    Invoke The Names of Elder Gods
    Output: Mischief Managed!
End Script
```
And Invoke it to yield:
```
TALE +-> Invoke Example
     |       Doing mischief
     |       1 => Wonder!
     |       1 => Excitement!
     |         ^(;,;)^
     |       Mischief Managed!
```

But the real power of output statements comes out when combined with interpolation expressions, which use square brackets `[]` to evaluate expressions within a section of text. For example:
```
Output: The house is [Roll on Color], with [Color] trim, and has [1d4] stories.
```
Notice that above, you can either use full roll syntax, or an abbreviated syntax where you only put the Table name within the `[]`.

## Assigning values to names
Or Assigning names to values, depending on how you want to think about it. Either way, Assignment statements allow you created named values that can be used elsewhere in Tables and Scripts.

The first first two forms rely on the `=` sign to indicate assignment, and differ only in whether they include the leading `Set` keyword.
```
Set the_answer = 42
the_word = Bird
```
Notice that assigned values can be either numbers or textual/strings.

The third form uses the keyword `To` in place of the `=` sign, and must always include the leading keyword `Set`
```
Set throttle To "100 %"
```
You can enclose values in quotes to force them to be interpreted as strings in situations where it might be ambiguous otherwise.

### The Scope of Assignments
Most programming languages, including TALE, implement [Scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)) for their named values/variables. In TALE, two syntax constructs currently introduce a new Scope: `Script` executions, and `Load` statements. Here is a [Practical Example](src/snippets/ex22_script_scopes.tale) of how that might apply to a script. The average TALE user, is unlikely to run into situations where this is relevent, but it's documented here so that advanced users aren't caught off guard by the behavior.
