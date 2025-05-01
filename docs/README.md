# DOCS
This folder contains documentation related to the grammar and implementation of TALE. For user documentation, see the [README](../crates/cli/README.md) for the Command Line Interface Application.

## Formal Language Definition
A formal definition of the grammar (In [**E**xtended **B**ackus-**N**aur **F**orm](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form)) of TALE can be found in [grammar.ebnf](grammar.ebnf). Assuming it's been kept up to date, and mistakes haven't been made, it lays out what constitues valid TALE syntax from the perspective of the Lexer + Parser. Due to its nature as a [Context-Free Grammar](https://en.wikipedia.org/wiki/Context-free_grammar), it cannot cover semantics, which may be rejected by the Analyzer or Evaluator.

## Syntax Highlighting
A fairly basic (and incomplete) syntax highlighting definition for TALE exists in [tale.tmLanguage.json](tale.tmLanguage.json). A [VSCode Extension](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide) could be created and published once this grammar is more complete. A more complete solution, handling syntax highlighting, semantic analysis and possibly other error detection, will eventually be accomplished via an LSP implementation [Tracked Here](https://github.com/kaedr/tale/issues/22).
