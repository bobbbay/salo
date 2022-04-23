# Syntax

Ready to write some Salo? Let's go through some basic language features.

## Comments

There are two forms of comments in Salo: regular comments and documentation comments. A regular comment, denoted by two dashes (`--`) or a curly brace and a dash (`{-`, `-}`) are comments that Salo will completely ignore.

```salo
-- Hello, world!
```

Three-dash comments (`---`), or a curly brace and two dashes (`{--`, `--}`) denote documentation comments. These comments are seen by Salo, and included when you run `salo doc`. Unlike regular comments, these comments must be tied to some expression in your code, like so:

```
--- This is some value x.
x = 0
```

Doc comments that are not "attached" to a piece of code are called tangling comments. These generate warnings in Salo, but pass compilation. Tangling comments are not recommended.

## Variables

> Variables are immutable by default in Salo.

TODO: To be continued!
