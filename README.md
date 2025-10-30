# pengine.el - SWI-Prolog Pengines Client for Emacs

A comprehensive Emacs Lisp client for [SWI-Prolog Pengines](https://www.swi-prolog.org/pldoc/man?section=pengine-overview), enabling seamless interaction with remote Prolog engines from Emacs.

## Features

- **Full Pengines API Support**: Create, query, and manage remote Prolog engines
- **Asynchronous Operations**: Non-blocking HTTP requests using Emacs' `url` library
- **Event-Driven Architecture**: Callback-based handling for all pengine events
- **JSON Support**: Automatic conversion between Prolog terms and JSON
- **Interactive Commands**: Query pengines interactively from Emacs
- **Multiple Pengine Management**: Track and manage multiple concurrent pengines
- **Compatible**: Works with Emacs 27.1, 28.2, and later versions

## Installation

### Manual Installation

1. Download the files:
   - `pengine.el` - Main library
   - `pengine-examples.el` - Usage examples
   - `pengine-test.el` - Test suite

2. Place in your load path:
```bash
   mkdir -p ~/.emacs.d/pengine
   cp pengine.el pengine-examples.el pengine-test.el ~/.emacs.d/pengine/
```

3. Add to your `init.el`:
```elisp
   (add-to-list 'load-path "~/.emacs.d/pengine")
   (require 'pengine)
```

4. Restart Emacs or evaluate:
```elisp
   M-x eval-buffer RET
```

### Using `use-package`
```elisp
(use-package pengine
  :load-path "~/.emacs.d/pengine"
  :config
  (setq pengine-default-server "http://localhost:3030"))
```

## Server Setup

### Quick Start: Local Server

The easiest way to get started is to run a local Pengine server.

#### 1. Install SWI-Prolog
```bash
# Ubuntu/Debian
sudo apt-get install swi-prolog

# macOS (Homebrew)
brew install swi-prolog

# Arch Linux
sudo pacman -S swi-prolog

# Or download from: https://www.swi-prolog.org/Download.html
```

#### 2. Create Server File

Copy the included `swipl_pengine_server.pl` to your project directory, or create it with this content:
```prolog
% swipl_pengine_server.pl - Local SWI-Prolog Pengine Server
% Usage: swipl swipl_pengine_server.pl

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_cors)).
:- use_module(library(pengines)).
:- use_module(library(http/http_json)).

% Enable CORS for local development (allows requests from Emacs)
:- set_setting(http:cors, [*]).

% Make standard Prolog libraries available in the sandbox
% Add more as needed for your application
:- use_module(pengine_sandbox:library(lists)).
:- use_module(pengine_sandbox:library(apply)).
:- use_module(pengine_sandbox:library(aggregate)).

% Optional: Enable debugging to see request logs
% :- debug(http(request)).
% :- debug(pengine(transition)).

% Start the HTTP server on port 3030
:- http_server(http_dispatch, [port(3030)]).

% Print startup message
:- format('~n'),
   format('PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP~n'),
   format('  SWI-Prolog Pengine Server~n'),
   format('  Port: 3030~n'),
   format('  URL:  http://localhost:3030~n'),
   format('~n'),
   format('  Available libraries in sandbox:~n'),
   format('    - lists (member/2, append/3, etc.)~n'),
   format('    - apply (maplist/2, etc.)~n'),
   format('    - aggregate (aggregate/3, etc.)~n'),
   format('~n'),
   format('  Server is ready to accept queries from Emacs!~n'),
   format('PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP~n'),
   format('~n').

% Keep the server running (prevents immediate exit)
:- thread_get_message(_).
```

#### 3. Start the Server
```bash
# Navigate to your project directory
cd ~/your-project

# Start the server
swipl swipl_pengine_server.pl
```

You should see:
```
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
  SWI-Prolog Pengine Server
  Port: 3030
  URL:  http://localhost:3030

  Available libraries in sandbox:
    - lists (member/2, append/3, etc.)
    - apply (maplist/2, etc.)
    - aggregate (aggregate/3, etc.)

  Server is ready to accept queries from Emacs!
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
```

#### 4. Test from Emacs
```elisp
(require 'pengine)

;; Set the local server as default
(setq pengine-default-server "http://localhost:3030")

;; Test query
(pengine-query 
 "http://localhost:3030"
 "member(X, [hello, world])"
 (lambda (solutions)
   (message "Results: %S" solutions)))
```

You should see: `Results: ((:X "hello") (:X "world"))`

### Adding Custom Libraries

To make additional Prolog libraries available in your pengine server, add them to `swipl_pengine_server.pl`:
```prolog
% Add more libraries as needed
:- use_module(pengine_sandbox:library(clpfd)).     % Constraint logic programming
:- use_module(pengine_sandbox:library(dcg/basics)). % DCG utilities
:- use_module(pengine_sandbox:library(assoc)).     % Association lists

% Or load your own modules
:- use_module(pengine_sandbox:my_custom_module).
```

### Adding Custom Predicates

You can add your own predicates directly in the server file:
```prolog
% Define custom predicates in the pengine_sandbox module
:- module(pengine_sandbox, []).

% Your custom predicates
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.

% Make them safe (if they have no dangerous side effects)
:- use_module(library(sandbox)).
:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(pengine_sandbox:fibonacci(_,_)).
```

### Production Server Setup

For production use, consider these additional settings:
```prolog
% Limit concurrent pengines per client
:- set_setting_default(pengines:max_session_pengines, 10).

% Set time limits (in seconds)
:- set_setting_default(pengines:time_limit, 60).

% Restrict access by IP
:- set_setting_default(pengines:allow_from, ['127.0.0.1', 'your.server.ip']).

% Enable HTTPS (requires SSL certificate)
:- use_module(library(http/http_ssl_plugin)).
:- http_server(http_dispatch, 
               [port(3030),
                ssl([certificate_file('cert.pem'),
                     key_file('key.pem')])]).
```

### Remote Server

To use a remote Pengine server (like the official SWI-Prolog demo server):
```elisp
(setq pengine-default-server "https://pengines.swi-prolog.org")

(pengine-query 
 "https://pengines.swi-prolog.org"
 "between(1, 5, X)"
 (lambda (solutions)
   (message "Numbers: %S" solutions)))
```

### Troubleshooting Server Issues

#### Port Already in Use

If port 3030 is already in use, change it in the server file:
```prolog
:- http_server(http_dispatch, [port(8080)]).  % Use different port
```

Then update Emacs:
```elisp
(setq pengine-default-server "http://localhost:8080")
```

#### Connection Refused

1. Check if the server is running: `ps aux | grep swipl`
2. Test with curl: `curl http://localhost:3030/pengine/create -X POST -H "Content-Type: application/json" -d '{"format":"json"}'`
3. Check firewall settings

#### Predicate Not Available

If you get "Unknown procedure" errors, the library isn't loaded:
```prolog
% Add to swipl_pengine_server.pl
:- use_module(pengine_sandbox:library(the_library_you_need)).
```

#### CORS Errors

If you see CORS errors in logs, ensure CORS is enabled:
```prolog
:- use_module(library(http/http_cors)).
:- set_setting(http:cors, [*]).
```

## Quick Start

### Simple Query
```elisp
(pengine-query 
 "http://localhost:3030"
 "member(X, [1,2,3])"
 (lambda (solutions)
   (message "Solutions: %S" solutions)))
```

### Interactive Query
```
M-x pengine-query-interactive RET
Server: http://localhost:3030 RET
Query: member(X, [a,b,c]) RET
```

### Complete Example
```elisp
(pengine-create
 :server "http://localhost:3030"
 :src-text "
   fibonacci(0, 0).
   fibonacci(1, 1).
   fibonacci(N, F) :-
       N > 1,
       N1 is N - 1,
       N2 is N - 2,
       fibonacci(N1, F1),
       fibonacci(N2, F2),
       F is F1 + F2.
 "
 :ask "fibonacci(10, X)"
 :onsuccess (lambda (pengine event)
              (message "Result: %S" (pengine-event-data event))))
```

## API Reference

### Creating Pengines

#### `pengine-create`

Create a new pengine with extensive options.
```elisp
(pengine-create &key
  server               ; Server URL (default: pengine-default-server)
  application          ; Application name (default: "pengine_sandbox")
  destroy              ; Auto-destroy after query (default: t)
  format               ; Response format (default: "json")
  ask                  ; Initial query
  template             ; Variable template for bindings
  chunk                ; Solutions per chunk (default: 1)
  src-text             ; Prolog source code as string
  src-url              ; URL to load Prolog source
  oncreate             ; Callback when pengine is created
  onsuccess            ; Callback for successful results
  onfailure            ; Callback when query fails
  onerror              ; Callback for errors
  onprompt             ; Callback for pengine_input/2
  onoutput             ; Callback for pengine_output/1
  onstop               ; Callback when query is stopped
  onabort              ; Callback when query is aborted
  ondestroy)           ; Callback when pengine is destroyed
```

**Returns**: A pengine object

**Example**:
```elisp
(defvar my-pengine nil)

(setq my-pengine
      (pengine-create
       :server "http://localhost:3030"
       :src-text "p(a). p(b). p(c)."
       :oncreate (lambda (pengine event)
                   (message "Created pengine: %s" (pengine-id pengine))
                   (pengine-ask pengine "p(X)"))
       :onsuccess (lambda (pengine event)
                    (message "Solutions: %S" (pengine-event-data event))
                    (when (pengine-event-more event)
                      (pengine-next pengine)))
       :onfailure (lambda (pengine event)
                    (message "Query failed"))))
```

### Querying Pengines

#### `pengine-ask`

Ask a query to an existing pengine.
```elisp
(pengine-ask pengine query &optional template chunk)
```

**Arguments**:
- `pengine`: Pengine object
- `query`: Prolog query string
- `template`: Optional variable template
- `chunk`: Optional chunk size for solutions

**Example**:
```elisp
(pengine-ask my-pengine "member(X, [1,2,3])")
```

#### `pengine-next`

Request the next solution(s) from a pengine.
```elisp
(pengine-next pengine &optional chunk)
```

**Example**:
```elisp
;; In onsuccess callback:
(lambda (pengine event)
  (message "Solution: %S" (pengine-event-data event))
  (when (pengine-event-more event)
    (pengine-next pengine)))
```

#### `pengine-query`

Simplified query interface that collects all solutions.
```elisp
(pengine-query server query &optional callback)
```

**Example**:
```elisp
(pengine-query
 "http://localhost:3030"
 "between(1, 5, X)"
 (lambda (solutions)
   (message "All solutions: %S" solutions)))
```

### Controlling Pengines

#### `pengine-stop`

Stop the current query gracefully.
```elisp
(pengine-stop pengine)
```

#### `pengine-abort`

Abort the current query forcefully.
```elisp
(pengine-abort pengine)
```

#### `pengine-destroy`

Destroy a pengine.
```elisp
(pengine-destroy pengine &optional force)
```

**Example**:
```elisp
;; Graceful destruction
(pengine-destroy my-pengine)

;; Forceful destruction
(pengine-destroy my-pengine t)
```

#### `pengine-respond`

Send input in response to a prompt from `pengine_input/2`.
```elisp
(pengine-respond pengine input)
```

**Example**:
```elisp
(pengine-create
 :src-text "
   ask_name :-
       pengine_input('What is your name?', Name),
       format('Hello, ~w!~n', [Name]),
       pengine_output(Name).
 "
 :onprompt (lambda (pengine event)
             (let ((name (read-string (pengine-event-data event))))
               (pengine-respond pengine name)))
 :onoutput (lambda (pengine event)
             (message "Output: %s" (pengine-event-data event))))
```

### Management Commands

#### `pengine-list-active`

List all active pengines.
```elisp
M-x pengine-list-active
```

#### `pengine-destroy-all`

Destroy all active pengines.
```elisp
M-x pengine-destroy-all
```

## Event Callbacks

All callbacks receive two arguments: the `pengine` object and an `event` object.

### Event Structure

Access event properties using:

- `(pengine-event-event event)` - Event type (symbol)
- `(pengine-event-id event)` - Pengine ID
- `(pengine-event-data event)` - Event data (varies by event)
- `(pengine-event-more event)` - More solutions available (success events)
- `(pengine-event-time event)` - Query execution time (success events)
- `(pengine-event-code event)` - Error code (error events)

### Event Types

#### `oncreate`

Called when the pengine is successfully created.
```elisp
:oncreate (lambda (pengine event)
            (message "Pengine ID: %s" (pengine-id pengine)))
```

#### `onsuccess`

Called when a query succeeds with solution(s).
```elisp
:onsuccess (lambda (pengine event)
             (let ((solutions (pengine-event-data event))
                   (more (pengine-event-more event)))
               (message "Found %d solution(s)" (length solutions))
               (when more
                 (pengine-next pengine))))
```

The `data` field contains a list of solution bindings, where each binding is a plist of variable-value pairs.

**Example data**:
```elisp
((:X "a") (:X "b") (:X "c"))
```

#### `onfailure`

Called when a query fails to find solutions.
```elisp
:onfailure (lambda (pengine event)
             (message "No solutions found"))
```

#### `onerror`

Called when an error occurs.
```elisp
:onerror (lambda (pengine event)
           (message "Error: %s" (pengine-event-data event)))
```

#### `onprompt`

Called when the Prolog code calls `pengine_input/2`.
```elisp
:onprompt (lambda (pengine event)
            (let* ((prompt (pengine-event-data event))
                   (answer (read-string prompt)))
              (pengine-respond pengine answer)))
```

#### `onoutput`

Called when the Prolog code calls `pengine_output/1`.
```elisp
:onoutput (lambda (pengine event)
            (message "Output: %s" (pengine-event-data event)))
```

## Advanced Examples

### Running Multiple Queries
```elisp
(defun run-multiple-queries ()
  "Run multiple pengines concurrently."
  (let ((queries '("member(X, [1,2,3])"
                  "between(5, 10, X)"
                  "append([a], [b,c], X)")))
    (dolist (query queries)
      (pengine-query
       "http://localhost:3030"
       query
       (lambda (solutions)
         (message "Query '%s' -> %S" query solutions))))))
```

### Error Handling
```elisp
(pengine-create
 :server "http://localhost:3030"
 :ask "nonexistent_predicate(X)"
 :onsuccess (lambda (pengine event)
              (message "Success: %S" (pengine-event-data event)))
 :onfailure (lambda (pengine event)
              (message "Query failed"))
 :onerror (lambda (pengine event)
            (message "Error occurred: %s" (pengine-event-data event))))
```

### Loading External Code
```elisp
(pengine-create
 :server "http://localhost:3030"
 :src-url "https://example.com/my-prolog-code.pl"
 :ask "my_predicate(X)"
 :onsuccess (lambda (pengine event)
              (message "Results: %S" (pengine-event-data event))))
```

### Working with Prolog I/O
```elisp
(pengine-create
 :server "http://localhost:3030"
 :src-text "
   dialog :-
       pengine_input('Enter your age: ', Age),
       (   Age >= 18
       ->  pengine_output('You are an adult')
       ;   pengine_output('You are a minor')
       ).
 "
 :ask "dialog"
 :onprompt (lambda (pengine event)
             (let ((age (read-number (pengine-event-data event))))
               (pengine-respond pengine (number-to-string age))))
 :onoutput (lambda (pengine event)
             (message "%s" (pengine-event-data event))))
```

## Configuration

### Customization Variables
```elisp
;; Default pengine server
(setq pengine-default-server "http://localhost:3030")

;; Default application name
(setq pengine-default-application "pengine_sandbox")

;; HTTP request timeout (seconds)
(setq pengine-request-timeout 300)

;; Enable debug messages (optional)
(setq pengine-debug t)
```

## Prolog Term Representation

### JSON to Prolog Term Mapping

The client automatically converts between JSON and Prolog-like Lisp representations:

| Prolog | JSON | Emacs Lisp |
|--------|------|------------|
| `a` | `"a"` | `"a"` |
| `123` | `123` | `123` |
| `[1,2,3]` | `[1,2,3]` | `(1 2 3)` |
| `f(a,b)` | `{"functor":"f","args":["a","b"]}` | `(compound f "a" "b")` |
| `@(true)` | `true` | `t` |
| `@(false)` | `false` | `nil` |
| `@(null)` | `null` | `:null` |

## Troubleshooting

### Common Issues

1. **Pengine not created**: Check that the server URL is correct and accessible.

2. **JSON parse errors**: Ensure the server is returning valid JSON. Check server logs.

3. **Timeout errors**: Increase `pengine-request-timeout` for long-running queries.

4. **Permission errors**: Some predicates may be restricted in the sandbox. Check server configuration and add libraries as needed.

5. **"Unknown procedure" errors**: The predicate isn't available in the sandbox. Add the required library to your server file:
```prolog
   :- use_module(pengine_sandbox:library(the_library)).
```

### Debugging

Enable debug messages:
```elisp
(setq pengine-debug t)
```

Enable URL library debugging:
```elisp
(setq url-debug t)
```

Check active pengines:
```elisp
M-x pengine-list-active
```

Check server logs in the terminal where `swipl` is running.

## Compatibility

- **Emacs**: 27.1, 28.2, and later
- **SWI-Prolog**: 7.x, 8.x, 9.x with pengines package
- **Operating Systems**: Linux, macOS, Windows
- **Servers**: Any SWI-Prolog server with pengines enabled

## Examples

The package includes `pengine-examples.el` with 20+ examples:
```elisp
(require 'pengine-examples)

;; Try these examples
M-x pengine-example-factorial RET
M-x pengine-example-fibonacci RET
M-x pengine-example-zebra-puzzle RET
M-x pengine-example-calculator RET
```

## Testing

Run the complete test suite:
```elisp
(require 'pengine-test)
M-x ert RET pengine-test- RET
```

All tests should pass with a working internet connection and accessible pengine server.

## Contributing

Contributions are welcome! Please submit issues and pull requests to the repository.

Areas where help is appreciated:
- Additional examples and use cases
- Integration with existing Prolog modes
- Performance optimizations
- Bug reports and fixes
- Documentation improvements

## License

This package is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3, or (at your option) any later version.

## See Also

- [SWI-Prolog Pengines Documentation](https://www.swi-prolog.org/pldoc/man?section=pengine-overview)
- [Pengines JavaScript Client](https://github.com/SWI-Prolog/packages-pengines)
- [SWISH: SWI-Prolog for Sharing](https://swish.swi-prolog.org/)

## Author

Created for the Emacs and Prolog communities.

## Changelog

### Version 1.0.0 (2025)

- Initial release
- Full Pengines API support
- Async HTTP with url.el
- Event-driven callbacks
- Interactive commands
- Emacs 27.1+ compatibility
- Comprehensive documentation and examples