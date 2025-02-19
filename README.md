# Cave - A Modern Common Lisp Web Application

Cave is a robust Common Lisp web application framework built with modern libraries and best practices.

## Features

- Modern web server with both production (Woo) and development (Hunchentoot) support
- Built-in OAuth authentication support
- Stripe payment integration
- Database connectivity with Postmodern
- Thread-safe session management
- Custom reader macros for Clojure-like syntax
- Environment-based configuration

## Prerequisites

- SBCL or another modern Common Lisp implementation
- Quicklisp package manager
- PostgreSQL (for database functionality)
- Redis (optional, for production session storage)

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/cave.git
   cd cave
   ```

2. Load the system through ASDF:
   ```lisp
   (ql:quickload :cave)
   ```

## Configuration

Cave uses environment variables for configuration. Set these before starting the server:

Required:
- `DATABASE_URL`: PostgreSQL connection string
- `AUTH0_DOMAIN`: Your Auth0 domain
- `AUTH0_CLIENT_ID`: Your Auth0 client ID
- `AUTH0_CLIENT_SECRET`: Your Auth0 client secret
- `STRIPE_SECRET_KEY`: Your Stripe secret key
- `STRIPE_WEBHOOK_SECRET`: Your Stripe webhook secret

Optional:
- `REDIS_URL`: Redis connection string (for production session storage)
- `PORT`: Server port (defaults to 3000)
- `DEBUG`: Enable debug mode (defaults to false)

## Usage

### Development Mode

Start the development server with:
```lisp
(cave:start-dev)
```

This will:
- Start Hunchentoot on port 3000
- Enable debug mode
- Start a Swank server on port 4005 for SLIME/SLY connection

### Production Mode

Start the production server with:
```lisp
(cave:start)
```

This will:
- Start Woo server on port 3000
- Disable debug mode
- Use production-optimized settings

### Custom Configuration

Start with custom settings using:
```lisp
(cave:start-custom
  :server :woo
  :port 8080
  :debug t
  :swank-port 4006)
```

### Stopping the Server

Stop the server with:
```lisp
(cave:stop)
```

## Development

### Reader Macros

Cave includes custom reader macros for Clojure-like syntax:
- `[]` for vectors
- `{}` for maps
- `#{}` for sets

Example:
```lisp
[1 2 3]              ; => #(1 2 3)
{"key" "value"}      ; => hash-table
#{1 2 3}             ; => set
```

### Protected Routes

Define protected routes (requiring authentication) using the `defprotected` macro:
```lisp
(defprotected "/api/protected" ()
  (render-json '(:message "This is a protected endpoint")))
```

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -am 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

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
