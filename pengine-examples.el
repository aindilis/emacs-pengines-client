;;; pengine-examples.el --- Examples for pengine.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains practical examples of using pengine.el for various
;; Prolog querying tasks from Emacs.

;;; Code:

(require 'pengine)

;;; Basic Examples

(defun pengine-example-simple-query ()
  "Example: Simple query with immediate results."
  (interactive)
  (pengine-query
   "https://pengines.swi-prolog.org"
   "member(X, [apple, banana, cherry])"
   (lambda (solutions)
     (message "Fruits: %S" solutions))))

(defun pengine-example-member ()
  "Example: Query list membership."
  (interactive)
  (pengine-create
   :ask "member(X, [1,2,3,4,5])"
   :onsuccess (lambda (pengine event)
                (message "Member: %S" (pengine-event-data event))
                (when (pengine-event-more event)
                  (message "Getting next...")
                  (pengine-next pengine)))
   :onfailure (lambda (pengine event)
                (message "No more solutions"))))

(defun pengine-example-append ()
  "Example: List append operations."
  (interactive)
  (pengine-query
   "https://pengines.swi-prolog.org"
   "append([1,2], [3,4], X)"
   (lambda (solutions)
     (message "Result of append: %S" solutions))))

;;; Arithmetic Examples

(defun pengine-example-factorial ()
  "Example: Calculate factorial using Prolog."
  (interactive)
  (let ((n (read-number "Calculate factorial of: ")))
    (pengine-create
     :src-text "
       factorial(0, 1).
       factorial(N, F) :-
           N > 0,
           N1 is N - 1,
           factorial(N1, F1),
           F is N * F1.
     "
     :ask (format "factorial(%d, F)" n)
     :onsuccess (lambda (pengine event)
                  (let* ((solutions (pengine-event-data event))
                         (result (plist-get (car solutions) :F)))
                    (message "Factorial of %d is %s" n result)))
     :onerror (lambda (pengine event)
                (message "Error: %s" (pengine-event-data event))))))

(defun pengine-example-fibonacci ()
  "Example: Calculate Fibonacci numbers."
  (interactive)
  (let ((n (read-number "Calculate Fibonacci number: ")))
    (pengine-create
     :src-text "
       fib(0, 0).
       fib(1, 1).
       fib(N, F) :-
           N > 1,
           N1 is N - 1,
           N2 is N - 2,
           fib(N1, F1),
           fib(N2, F2),
           F is F1 + F2.
     "
     :ask (format "fib(%d, X)" n)
     :onsuccess (lambda (pengine event)
                  (let* ((solutions (pengine-event-data event))
                         (result (plist-get (car solutions) :X)))
                    (message "Fibonacci(%d) = %s" n result))))))

;;; List Processing Examples

(defun pengine-example-list-sum ()
  "Example: Calculate sum of a list."
  (interactive)
  (let ((list (read-string "Enter list (e.g., [1,2,3,4,5]): ")))
    (pengine-create
     :src-text "
       sum_list([], 0).
       sum_list([H|T], Sum) :-
           sum_list(T, SumT),
           Sum is H + SumT.
     "
     :ask (format "sum_list(%s, Sum)" list)
     :onsuccess (lambda (pengine event)
                  (let* ((solutions (pengine-event-data event))
                         (sum (plist-get (car solutions) :Sum)))
                    (message "Sum of %s is %s" list sum))))))

(defun pengine-example-list-reverse ()
  "Example: Reverse a list."
  (interactive)
  (let ((list (read-string "Enter list to reverse: " "[1,2,3,4,5]")))
    (pengine-query
     "https://pengines.swi-prolog.org"
     (format "reverse(%s, X)" list)
     (lambda (solutions)
       (let ((reversed (plist-get (car solutions) :X)))
         (message "Reversed: %S" reversed))))))

(defun pengine-example-list-sort ()
  "Example: Sort a list."
  (interactive)
  (let ((list (read-string "Enter list to sort: " "[5,2,8,1,9,3]")))
    (pengine-query
     "https://pengines.swi-prolog.org"
     (format "sort(%s, Sorted)" list)
     (lambda (solutions)
       (let ((sorted (plist-get (car solutions) :Sorted)))
         (message "Sorted: %S" sorted))))))

;;; Logic Puzzle Examples

(defun pengine-example-zebra-puzzle ()
  "Example: Solve the Zebra puzzle (Einstein's riddle)."
  (interactive)
  (message "Solving Zebra puzzle... (this may take a moment)")
  (pengine-create
   :src-text "
     member(X, [X|_]).
     member(X, [_|T]) :- member(X, T).
     
     nextto(X, Y, List) :- append(_, [X,Y|_], List).
     nextto(X, Y, List) :- append(_, [Y,X|_], List).
     
     zebra(Owns, Drinks) :-
         Houses = [_, _, _, _, _],
         member([red, english, _, _, _], Houses),
         member([_, spanish, _, _, dog], Houses),
         member([green, _, coffee, _, _], Houses),
         member([_, ukrainian, tea, _, _], Houses),
         nextto([green, _, _, _, _], [white, _, _, _, _], Houses),
         member([_, _, _, oldgold, snails], Houses),
         member([yellow, _, _, kools, _], Houses),
         Houses = [_, _, [_, _, milk, _, _], _, _],
         Houses = [[_, norwegian, _, _, _]|_],
         nextto([_, _, _, chesterfield, _], [_, _, _, _, fox], Houses),
         nextto([_, _, _, kools, _], [_, _, _, _, horse], Houses),
         member([_, _, orangejuice, luckystrike, _], Houses),
         member([_, japanese, _, parliament, _], Houses),
         nextto([_, norwegian, _, _, _], [blue, _, _, _, _], Houses),
         member([_, Owns, _, _, zebra], Houses),
         member([_, Drinks, water, _, _], Houses).
   "
   :ask "zebra(Owns, Drinks)"
   :onsuccess (lambda (pengine event)
                (let* ((solutions (pengine-event-data event))
                       (owns (plist-get (car solutions) :Owns))
                       (drinks (plist-get (car solutions) :Drinks)))
                  (message "The %s owns the zebra, and the %s drinks water!"
                          owns drinks)))
   :onfailure (lambda (pengine event)
                (message "Failed to solve puzzle"))
   :onerror (lambda (pengine event)
              (message "Error: %s" (pengine-event-data event)))))

;;; Interactive Examples

(defun pengine-example-input-output ()
  "Example: Interactive I/O with pengine_input and pengine_output."
  (interactive)
  (pengine-create
   :src-text "
     greet :-
         pengine_input('What is your name? ', Name),
         pengine_input('What is your age? ', Age),
         format(atom(Message), 'Hello, ~w! You are ~w years old.', [Name, Age]),
         pengine_output(Message).
   "
   :ask "greet"
   :onprompt (lambda (pengine event)
               (let* ((prompt (pengine-event-data event))
                      (response (read-string prompt)))
                 (pengine-respond pengine response)))
   :onoutput (lambda (pengine event)
               (message ">>> %s" (pengine-event-data event)))))

(defun pengine-example-calculator ()
  "Example: Simple calculator using Prolog."
  (interactive)
  (pengine-create
   :src-text "
     calculate :-
         pengine_input('Enter first number: ', A),
         pengine_input('Enter operator (+,-,*,/): ', Op),
         pengine_input('Enter second number: ', B),
         (   Op = '+' -> Result is A + B
         ;   Op = '-' -> Result is A - B
         ;   Op = '*' -> Result is A * B
         ;   Op = '/' -> Result is A / B
         ;   Result = error
         ),
         format(atom(Output), '~w ~w ~w = ~w', [A, Op, B, Result]),
         pengine_output(Output).
   "
   :ask "calculate"
   :onprompt (lambda (pengine event)
               (let* ((prompt (pengine-event-data event))
                      (response (read-string prompt)))
                 (pengine-respond pengine response)))
   :onoutput (lambda (pengine event)
               (message "Result: %s" (pengine-event-data event)))))

;;; Advanced Examples

(defun pengine-example-concurrent-queries ()
  "Example: Run multiple queries concurrently."
  (interactive)
  (let ((queries '(("member(X, [a,b,c])" . "List membership")
                  ("between(1, 5, X)" . "Range generation")
                  ("factorial(5, F)" . "Factorial"))))
    (dolist (query-info queries)
      (let ((query (car query-info))
            (desc (cdr query-info)))
        (pengine-query
         "https://pengines.swi-prolog.org"
         query
         (lambda (solutions)
           (message "%s: %S" desc solutions)))))))

(defun pengine-example-error-handling ()
  "Example: Demonstrate error handling."
  (interactive)
  (pengine-create
   :ask "undefined_predicate(X)"
   :onsuccess (lambda (pengine event)
                (message "Success: %S" (pengine-event-data event)))
   :onfailure (lambda (pengine event)
                (message "Query failed (no solutions)"))
   :onerror (lambda (pengine event)
              (let ((error-msg (pengine-event-data event)))
                (message "ERROR: %s" error-msg)))))

(defun pengine-example-chunked-results ()
  "Example: Get results in chunks."
  (interactive)
  (let ((chunk-size (read-number "Chunk size: " 3)))
    (pengine-create
     :ask "between(1, 20, X)"
     :chunk chunk-size
     :onsuccess (lambda (pengine event)
                  (let ((solutions (pengine-event-data event))
                        (more (pengine-event-more event)))
                    (message "Chunk of %d: %S" (length solutions) solutions)
                    (when more
                      (when (y-or-n-p "Get next chunk? ")
                        (pengine-next pengine chunk-size)))))
     :onfailure (lambda (pengine event)
                  (message "No more solutions")))))

;;; Graph Theory Examples

(defun pengine-example-graph-path ()
  "Example: Find paths in a graph."
  (interactive)
  (pengine-create
   :src-text "
     % Graph edges
     edge(a, b).
     edge(a, c).
     edge(b, d).
     edge(c, d).
     edge(c, e).
     edge(d, f).
     edge(e, f).
     
     % Find path from Start to End
     path(Start, End, Path) :-
         path(Start, End, [Start], Path).
     
     path(End, End, Visited, Path) :-
         reverse(Visited, Path).
     path(Current, End, Visited, Path) :-
         edge(Current, Next),
         \\+ member(Next, Visited),
         path(Next, End, [Next|Visited], Path).
   "
   :ask "path(a, f, Path)"
   :onsuccess (lambda (pengine event)
                (let ((solutions (pengine-event-data event)))
                  (message "Paths from a to f:")
                  (dolist (sol solutions)
                    (message "  %S" (plist-get sol :Path)))
                  (when (pengine-event-more event)
                    (when (y-or-n-p "Find more paths? ")
                      (pengine-next pengine)))))
   :onfailure (lambda (pengine event)
                (message "No more paths found"))))

;;; Natural Language Processing Examples

(defun pengine-example-sentence-parser ()
  "Example: Simple sentence parsing with DCG."
  (interactive)
  (pengine-create
   :src-text "
     % Simple DCG grammar
     sentence --> noun_phrase, verb_phrase.
     noun_phrase --> determiner, noun.
     verb_phrase --> verb, noun_phrase.
     
     determiner --> [the].
     determiner --> [a].
     noun --> [cat].
     noun --> [dog].
     noun --> [mouse].
     verb --> [chases].
     verb --> [sees].
     
     parse_sentence(Words) :-
         sentence(Words, []).
   "
   :ask "parse_sentence([the, cat, chases, a, mouse])"
   :onsuccess (lambda (pengine event)
                (if (pengine-event-data event)
                    (message "Valid sentence!")
                  (message "Invalid sentence")))
   :onfailure (lambda (pengine event)
                (message "Invalid sentence"))))

;;; Database Query Examples

(defun pengine-example-family-tree ()
  "Example: Query a family tree database."
  (interactive)
  (pengine-create
   :src-text "
     % Family relationships
     parent(tom, bob).
     parent(tom, liz).
     parent(bob, ann).
     parent(bob, pat).
     parent(pat, jim).
     
     grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
     sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \\= Y.
     ancestor(X, Y) :- parent(X, Y).
     ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
   "
   :ask "grandparent(tom, X)"
   :onsuccess (lambda (pengine event)
                (let ((solutions (pengine-event-data event)))
                  (message "Tom's grandchildren:")
                  (dolist (sol solutions)
                    (message "  %s" (plist-get sol :X)))
                  (when (pengine-event-more event)
                    (pengine-next pengine))))
   :onfailure (lambda (pengine event)
                (message "Tom has no grandchildren"))))

;;; Utility Functions

(defun pengine-example-all-solutions ()
  "Example: Collect all solutions at once."
  (interactive)
  (let ((query (read-string "Enter query: " "between(1, 10, X)"))
        (all-solutions nil))
    (pengine-create
     :ask query
     :chunk 100  ; Get many at once
     :onsuccess (lambda (pengine event)
                  (setq all-solutions
                        (append all-solutions (pengine-event-data event)))
                  (if (pengine-event-more event)
                      (pengine-next pengine)
                    (message "All solutions (%d total): %S"
                            (length all-solutions)
                            all-solutions)))
     :onfailure (lambda (pengine event)
                  (message "All solutions (%d total): %S"
                          (length all-solutions)
                          all-solutions)))))

;;; Testing and Benchmarking

(defun pengine-example-benchmark ()
  "Example: Benchmark query performance."
  (interactive)
  (let ((start-time (current-time))
        (query (read-string "Enter query to benchmark: " "between(1, 1000, _)")))
    (pengine-create
     :ask query
     :chunk 1000
     :onsuccess (lambda (pengine event)
                  (let ((elapsed (float-time (time-subtract (current-time)
                                                           start-time)))
                        (count (length (pengine-event-data event))))
                    (if (pengine-event-more event)
                        (pengine-next pengine)
                      (message "Query completed in %.3fs (%d solutions)"
                              elapsed count))))
     :onfailure (lambda (pengine event)
                  (let ((elapsed (float-time (time-subtract (current-time)
                                                           start-time))))
                    (message "Query completed in %.3fs (no solutions)" elapsed))))))

(provide 'pengine-examples)

;;; pengine-examples.el ends here
