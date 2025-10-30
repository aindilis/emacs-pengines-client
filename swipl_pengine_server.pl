% swipl_pengine_server.pl - Local SWI-Prolog Pengine Server for Emacs
%
% This file provides a simple, ready-to-use Pengine server for local development
% with the pengine.el Emacs client.
%
% USAGE:
%   swipl swipl_pengine_server.pl
%
% The server will start on http://localhost:3030
%
% REQUIREMENTS:
%   - SWI-Prolog 7.x or later with pengines package
%   - No additional configuration needed
%
% CUSTOMIZATION:
%   - Change port: Edit the http_server/2 call below
%   - Add libraries: Add use_module directives in the "Libraries" section
%   - Add custom predicates: Define them in the "Custom Predicates" section
%
% For more information, see:
%   https://www.swi-prolog.org/pldoc/man?section=pengine-overview

% ============================================================================
% Required Modules
% ============================================================================

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_cors)).
:- use_module(library(pengines)).
:- use_module(library(http/http_json)).

% ============================================================================
% Server Configuration
% ============================================================================

% Enable CORS (Cross-Origin Resource Sharing) for local development
% This allows Emacs to make requests to the server
:- set_setting(http:cors, [*]).

% Optional: Set maximum number of pengines per session
% Uncomment to limit concurrent pengines (default is unlimited)
% :- set_setting_default(pengines:max_session_pengines, 10).

% Optional: Set time limit for queries (in seconds)
% Uncomment to prevent runaway queries (default is 300 seconds)
% :- set_setting_default(pengines:time_limit, 60).

% Optional: Restrict access by IP address
% Uncomment and modify to restrict access in production
% :- set_setting_default(pengines:allow_from, ['127.0.0.1']).

% ============================================================================
% Standard Libraries Available in Sandbox
% ============================================================================

% These libraries are made available to all pengine queries.
% Add or remove libraries based on your needs.

% List manipulation (member/2, append/3, reverse/2, etc.)
:- use_module(pengine_sandbox:library(lists)).

% Higher-order predicates (maplist/2, include/3, etc.)
:- use_module(pengine_sandbox:library(apply)).

% Aggregation (aggregate/3, aggregate_all/3)
:- use_module(pengine_sandbox:library(aggregate)).

% Sorting predicates (sort/2, msort/2, etc.)
% :- use_module(pengine_sandbox:library(sort)).

% Constraint Logic Programming over Finite Domains
% Uncomment if you need CLP(FD) for constraint solving
% :- use_module(pengine_sandbox:library(clpfd)).

% String manipulation
% :- use_module(pengine_sandbox:library(strings)).

% DCG (Definite Clause Grammar) utilities
% :- use_module(pengine_sandbox:library(dcg/basics)).

% Association lists (efficient key-value storage)
% :- use_module(pengine_sandbox:library(assoc)).

% Random number generation
% :- use_module(pengine_sandbox:library(random)).

% ============================================================================
% Custom Predicates (Optional)
% ============================================================================

% You can define custom predicates here that will be available to all pengines.
% Make sure to declare them as safe if they don't have dangerous side effects.

% Example: Fibonacci sequence
%
% fibonacci(0, 0).
% fibonacci(1, 1).
% fibonacci(N, F) :-
%     N > 1,
%     N1 is N - 1,
%     N2 is N - 2,
%     fibonacci(N1, F1),
%     fibonacci(N2, F2),
%     F is F1 + F2.
%
% % Make it safe for sandboxing
% :- use_module(library(sandbox)).
% :- multifile sandbox:safe_primitive/1.
% sandbox:safe_primitive(pengine_sandbox:fibonacci(_,_)).

% Example: Factorial
%
% factorial(0, 1).
% factorial(N, F) :-
%     N > 0,
%     N1 is N - 1,
%     factorial(N1, F1),
%     F is N * F1.
%
% :- multifile sandbox:safe_primitive/1.
% sandbox:safe_primitive(pengine_sandbox:factorial(_,_)).

% ============================================================================
% Debugging (Optional)
% ============================================================================

% Uncomment these lines to see detailed request logs in the console.
% Useful for debugging but verbose in production.

% :- debug(http(request)).        % Log all HTTP requests
% :- debug(pengine(transition)).  % Log pengine state transitions
% :- debug(pengine(debug)).       % Log pengine debug messages

% ============================================================================
% Start HTTP Server
% ============================================================================

% Start the server on port 3030
% Change the port number if 3030 is already in use
:- http_server(http_dispatch, [port(3030)]).

% ============================================================================
% Startup Messages
% ============================================================================

:- format('~n'),
   format('PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP~n'),
   format('  SWI-Prolog Pengine Server for Emacs~n'),
   format('PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP~n'),
   format('~n'),
   format('  Server Status:   RUNNING~n'),
   format('  Port:            3030~n'),
   format('  URL:             http://localhost:3030~n'),
   format('~n'),
   format('  Available Libraries in Sandbox:~n'),
   format('    " lists      - List manipulation (member/2, append/3, ...)~n'),
   format('    " apply      - Higher-order predicates (maplist/2, ...)~n'),
   format('    " aggregate  - Aggregation (aggregate/3, ...)~n'),
   format('~n'),
   format('  Test from command line:~n'),
   format('    curl -X POST http://localhost:3030/pengine/create \\~n'),
   format('      -H "Content-Type: application/json" \\~n'),
   format('      -d \'{"format":"json","ask":"member(X,[a,b,c])"}\'~n'),
   format('~n'),
   format('  Test from Emacs:~n'),
   format('    (require \'pengine)~n'),
   format('    (pengine-query "http://localhost:3030" \\~n'),
   format('                   "member(X, [hello, world])" \\~n'),
   format('                   (lambda (s) (message "%%S" s)))~n'),
   format('~n'),
   format('  Press Ctrl-C to stop the server~n'),
   format('PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP~n'),
   format('~n').

% ============================================================================
% Keep Server Running
% ============================================================================

% This prevents the server from exiting immediately after startup.
% The server will run until you press Ctrl-C or kill the process.
:- thread_get_message(_).

% ============================================================================
% Advanced Configuration Examples (Commented Out)
% ============================================================================

% HTTPS Configuration:
% If you want to use HTTPS, uncomment and configure these lines.
% You will need SSL certificate files.
%
% :- use_module(library(http/http_ssl_plugin)).
% :- http_server(http_dispatch,
%                [port(3030),
%                 ssl([certificate_file('cert.pem'),
%                      key_file('key.pem'),
%                      password('your_key_password')])]).

% Custom Application Configuration:
% You can define custom pengine applications with specific settings.
%
% :- pengine_application(my_app).
% :- use_module(my_app:library(lists)).
% :- use_module(my_app:my_custom_module).

% Static File Serving:
% If you want to serve static files (HTML, JS, etc.):
%
% :- use_module(library(http/http_files)).
% :- http_handler(root(.), http_reply_from_files('.', []), [prefix]).

% Session Management:
% For persistent sessions across requests:
%
% :- use_module(library(http/http_session)).
% :- set_setting_default(http:session_timeout, 3600).

% ============================================================================
% Security Notes
% ============================================================================

% IMPORTANT: This server configuration is suitable for LOCAL DEVELOPMENT ONLY!
%
% For production use, consider:
%   1. Restricting CORS to specific origins (not [*])
%   2. Setting pengines:allow_from to specific IPs
%   3. Enabling HTTPS with proper certificates
%   4. Setting time_limit and max_session_pengines
%   5. Running behind a reverse proxy (nginx, apache)
%   6. Implementing authentication/authorization
%   7. Regular security updates to SWI-Prolog
%
% The pengine sandbox provides security by restricting predicates,
% but always review what libraries and predicates you expose.

% ============================================================================
% Troubleshooting
% ============================================================================

% Problem: Port 3030 already in use
% Solution: Change the port number in http_server/2 above
%
% Problem: "Unknown procedure" errors from Emacs
% Solution: Add the required library with use_module above
%
% Problem: Connection refused
% Solution: Check firewall settings, verify server is running
%
% Problem: CORS errors
% Solution: Verify http:cors setting is set to [*] or appropriate origins
%
% Problem: Queries timing out
% Solution: Increase pengines:time_limit or optimize your queries

% ============================================================================
% End of Configuration
% ============================================================================
