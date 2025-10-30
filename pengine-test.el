;;; pengine-test.el --- Tests for pengine.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the pengine.el package.
;; These tests require a running pengine server.

;;; Code:

(require 'ert)
(require 'pengine)

;;; Test Utilities

(defvar pengine-test-server "https://pengines.swi-prolog.org"
  "Test server URL.")

(defvar pengine-test-timeout 10
  "Timeout for tests in seconds.")

(defmacro pengine-test-with-timeout (timeout &rest body)
  "Execute BODY with TIMEOUT."
  (declare (indent 1))
  `(with-timeout (,timeout (error "Test timed out"))
     ,@body))

;;; Basic Tests

(ert-deftest pengine-test-create ()
  "Test basic pengine creation."
  (let ((created nil))
    (pengine-create
     :server pengine-test-server
     :oncreate (lambda (pengine event)
                 (should (pengine-p pengine))
                 (should (stringp (pengine-id pengine)))
                 (setq created t)
                 (pengine-destroy pengine)))
    (pengine-test-with-timeout 5
      (while (not created)
        (sleep-for 0.1)))
    (should created)))

(ert-deftest pengine-test-simple-query ()
  "Test simple query execution."
  (let ((result nil))
    (pengine-query
     pengine-test-server
     "member(X, [a,b,c])"
     (lambda (solutions)
       (setq result solutions)))
    (pengine-test-with-timeout 5
      (while (not result)
        (sleep-for 0.1)))
    (should (listp result))
    (should (= (length result) 3))))

(ert-deftest pengine-test-success-callback ()
  "Test success callback."
  (let ((success-called nil))
    (pengine-create
     :server pengine-test-server
     :ask "member(X, [1,2,3])"
     :onsuccess (lambda (pengine event)
                  (should (pengine-event-p event))
                  (should (eq (pengine-event-event event) 'success))
                  (should (pengine-event-data event))
                  (setq success-called t)))
    (pengine-test-with-timeout 5
      (while (not success-called)
        (sleep-for 0.1)))
    (should success-called)))

(ert-deftest pengine-test-failure-callback ()
  "Test failure callback."
  (let ((failure-called nil))
    (pengine-create
     :server pengine-test-server
     :ask "fail"
     :onfailure (lambda (pengine event)
                  (should (pengine-event-p event))
                  (should (eq (pengine-event-event event) 'failure))
                  (setq failure-called t)))
    (pengine-test-with-timeout 5
      (while (not failure-called)
        (sleep-for 0.1)))
    (should failure-called)))

;;; Query Tests

(ert-deftest pengine-test-arithmetic ()
  "Test arithmetic queries."
  (let ((result nil))
    (pengine-query
     pengine-test-server
     "X is 2 + 2"
     (lambda (solutions)
       (setq result (plist-get (car solutions) :X))))
    (pengine-test-with-timeout 5
      (while (not result)
        (sleep-for 0.1)))
    (should (equal result 4))))

(ert-deftest pengine-test-list-operations ()
  "Test list operations."
  (let ((result nil))
    (pengine-query
     pengine-test-server
     "append([1,2], [3,4], X)"
     (lambda (solutions)
       (setq result (plist-get (car solutions) :X))))
    (pengine-test-with-timeout 5
      (while (not result)
        (sleep-for 0.1)))
    (should (equal result '(1 2 3 4)))))

(ert-deftest pengine-test-multiple-solutions ()
  "Test retrieving multiple solutions."
  (let ((solutions nil)
        (done nil))
    (pengine-create
     :server pengine-test-server
     :ask "member(X, [a,b,c])"
     :onsuccess (lambda (pengine event)
                  (setq solutions (append solutions (pengine-event-data event)))
                  (if (pengine-event-more event)
                      (pengine-next pengine)
                    (setq done t)))
     :onfailure (lambda (pengine event)
                  (setq done t)))
    (pengine-test-with-timeout 5
      (while (not done)
        (sleep-for 0.1)))
    (should (= (length solutions) 3))))

;;; Source Code Tests

(ert-deftest pengine-test-src-text ()
  "Test loading source code via src-text."
  (let ((result nil))
    (pengine-query
     pengine-test-server
     "test_predicate(X)"
     (lambda (solutions)
       (setq result solutions)))
    ;; First try - should fail since predicate doesn't exist
    (sleep-for 1)
    (should (null result))
    
    ;; Now create with source
    (setq result nil)
    (pengine-create
     :server pengine-test-server
     :src-text "test_predicate(hello). test_predicate(world)."
     :ask "test_predicate(X)"
     :onsuccess (lambda (pengine event)
                  (setq result (pengine-event-data event))))
    (pengine-test-with-timeout 5
      (while (not result)
        (sleep-for 0.1)))
    (should (= (length result) 2))))

;;; Control Flow Tests

(ert-deftest pengine-test-next ()
  "Test pengine-next for getting additional solutions."
  (let ((solution-count 0)
        (done nil))
    (pengine-create
     :server pengine-test-server
     :ask "between(1, 5, X)"
     :chunk 1
     :onsuccess (lambda (pengine event)
                  (setq solution-count (+ solution-count
                                        (length (pengine-event-data event))))
                  (if (and (pengine-event-more event) (< solution-count 3))
                      (pengine-next pengine)
                    (setq done t)))
     :onfailure (lambda (pengine event)
                  (setq done t)))
    (pengine-test-with-timeout 5
      (while (not done)
        (sleep-for 0.1)))
    (should (= solution-count 3))))

(ert-deftest pengine-test-stop ()
  "Test stopping a query."
  (let ((stopped nil))
    (pengine-create
     :server pengine-test-server
     :ask "between(1, 1000, X)"
     :oncreate (lambda (pengine event)
                 ;; Stop immediately after creation
                 (run-with-timer 0.5 nil
                               (lambda () (pengine-stop pengine))))
     :onstop (lambda (pengine event)
               (setq stopped t)))
    (pengine-test-with-timeout 5
      (while (not stopped)
        (sleep-for 0.1)))
    (should stopped)))

;;; Chunking Tests

(ert-deftest pengine-test-chunked-results ()
  "Test chunked result retrieval."
  (let ((result nil)
        (done nil))
    (pengine-create
     :server pengine-test-server
     :ask "between(1, 10, X)"
     :chunk 5
     :onsuccess (lambda (pengine event)
                  (setq result (pengine-event-data event))
                  (setq done t)))
    (pengine-test-with-timeout 5
      (while (not done)
        (sleep-for 0.1)))
    (should (= (length result) 5))))

;;; Error Handling Tests

(ert-deftest pengine-test-error-callback ()
  "Test error callback with invalid query."
  (let ((error-called nil))
    (pengine-create
     :server pengine-test-server
     :ask "X is 1/0"  ; Division by zero
     :onerror (lambda (pengine event)
                (should (pengine-event-p event))
                (should (stringp (pengine-event-data event)))
                (setq error-called t)))
    (pengine-test-with-timeout 5
      (while (not error-called)
        (sleep-for 0.1)))
    (should error-called)))

;;; Lifecycle Tests

(ert-deftest pengine-test-destroy ()
  "Test pengine destruction."
  (let ((destroyed nil))
    (pengine-create
     :server pengine-test-server
     :destroy nil  ; Don't auto-destroy
     :oncreate (lambda (pengine event)
                 (run-with-timer 0.5 nil
                               (lambda () (pengine-destroy pengine))))
     :ondestroy (lambda (pengine event)
                  (setq destroyed t)))
    (pengine-test-with-timeout 5
      (while (not destroyed)
        (sleep-for 0.1)))
    (should destroyed)))

(ert-deftest pengine-test-auto-destroy ()
  "Test automatic destruction after query completion."
  (let ((destroyed nil))
    (pengine-create
     :server pengine-test-server
     :destroy t  ; Auto-destroy
     :ask "member(X, [a])"
     :ondestroy (lambda (pengine event)
                  (setq destroyed t)))
    (pengine-test-with-timeout 5
      (while (not destroyed)
        (sleep-for 0.1)))
    (should destroyed)))

;;; JSON Conversion Tests

(ert-deftest pengine-test-json-to-prolog ()
  "Test JSON to Prolog term conversion."
  ;; Atom
  (should (equal (pengine--json-to-prolog-term "atom") "atom"))
  
  ;; Number
  (should (equal (pengine--json-to-prolog-term 42) 42))
  
  ;; List
  (should (equal (pengine--json-to-prolog-term '(1 2 3)) '(1 2 3)))
  
  ;; Null
  (should (equal (pengine--json-to-prolog-term :null) nil))
  
  ;; Compound term
  (let ((json-term '(:functor "f" :args ("a" "b"))))
    (should (equal (pengine--json-to-prolog-term json-term)
                  '(compound f "a" "b")))))

(ert-deftest pengine-test-prolog-to-json ()
  "Test Prolog term to JSON conversion."
  ;; String
  (should (equal (pengine--prolog-term-to-json "atom") "atom"))
  
  ;; Number
  (should (equal (pengine--prolog-term-to-json 42) 42))
  
  ;; List
  (should (equal (pengine--prolog-term-to-json '(1 2 3)) '(1 2 3)))
  
  ;; Nil
  (should (equal (pengine--prolog-term-to-json nil) :null))
  
  ;; Symbol
  (should (equal (pengine--prolog-term-to-json 'atom) "atom")))

;;; Integration Tests

(ert-deftest pengine-test-full-workflow ()
  "Test complete workflow: create, query, get all solutions, destroy."
  (let ((pengine nil)
        (created nil)
        (solutions nil)
        (destroyed nil))
    
    ;; Create
    (setq pengine
          (pengine-create
           :server pengine-test-server
           :destroy nil
           :oncreate (lambda (p event)
                       (setq created t)
                       (setq pengine p))
           :ondestroy (lambda (p event)
                        (setq destroyed t))))
    
    (pengine-test-with-timeout 5
      (while (not created)
        (sleep-for 0.1)))
    
    ;; Query
    (pengine-ask pengine "member(X, [a,b,c])")
    
    ;; Collect solutions
    (setf (pengine-onsuccess pengine)
          (lambda (p event)
            (setq solutions (append solutions (pengine-event-data event)))
            (if (pengine-event-more event)
                (pengine-next p)
              (pengine-destroy p))))
    
    (pengine-test-with-timeout 5
      (while (not destroyed)
        (sleep-for 0.1)))
    
    (should (= (length solutions) 3))
    (should destroyed)))

;;; Run all tests

(defun pengine-run-all-tests ()
  "Run all pengine tests."
  (interactive)
  (ert-run-tests-batch-and-exit "pengine-test-"))

(provide 'pengine-test)

;;; pengine-test.el ends here
