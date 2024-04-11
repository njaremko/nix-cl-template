This aims to provide a template to run common lisp under nix

ALIVE should "just work" in VS Code if you have `nix` and `direnv` installed, and you install the VS Code recommended extensions.

# How do I start the server?

`run-dev` or `repl` + `(cave:start :server :woo :port 8080)`

# CSS?

Tailwindcss is available, and running `watch-css` or `build-css` will build the relevant CSS file

# How do I run tests?

`run-tests` or `nix flake check`

# How do I hot-reload my changes to a file in VS Code?

Right click, and select `Alive: Load file into REPL` (or press the shortcut beside it)

# How do I hot-reload my changes in the repl?

The following reloads the whole project:

```lisp
(asdf:load-system :cave :force t)
```
