This aims to provide a template to run common lisp under nix

ALIVE should "just work" in VS Code if you have `nix` and `direnv` installed, and you install the VS Code recommended extensions.

# How do I start the server?

`run-dev` or `repl` + `(cave:start :server :woo :port 8080)`

# CSS?

Tailwindcss is available, and running `watch-css` or `build-css` will build the relevant CSS file

# How do I run tests?

`run-tests` or `nix flake check`

# How do I hot-reload my changes to a file in VS Code?

### Note:

Starting the server directly in the VS Code breaks the VS Code REPL...for some reason (regular shell repl is fine). To get around that, we do the following:

- When you open VS Code, Alive boots a repl
- That repl starts a swank server
- Open a bash shell, and run `start-server-in-alive`
  - This connects to the running repl in VS Code, and starts the server
- Right any file you want to reload, and select `Alive: Load file into REPL` (or type the shortcut)

# How do I hot-reload my changes in the repl?

The following reloads the whole project:

```lisp
(asdf:load-system :cave :force t)
```
