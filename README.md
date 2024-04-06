This aims to provide a template to run common lisp under nix

`run_dev` to start

ALIVE should "just work" in VS Code if you have `nix` and `direnv` installed, and you install the VS Code recommended extensions.

# How do I hot-reload my changes in the repl?

```lisp
(asdf:load-system :cave :force t)
```
