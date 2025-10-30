;;; pengine.el --- SWI-Prolog Pengines client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Andrew <adougher9@yahoo.com>
;; Maintainer: Andrew <adougher9@yahoo.com>
;; Created with assistance from: Claude (Anthropic AI)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: prolog, pengines, swi-prolog
;; URL: https://github.com/example/pengine.el

;;; Commentary:

;; This package provides an Emacs client for SWI-Prolog Pengines,
;; allowing Emacs to interact with remote Prolog engines over HTTP.
;;
;; Pengines (Prolog Engines) is a SWI-Prolog library that enables
;; creating and querying Prolog engines remotely via HTTP/JSON.
;;
;; Basic usage:
;;
;;   (require 'pengine)
;;
;;   ;; Create a pengine and run a query
;;   (pengine-create
;;    :server "https://pengines.swi-prolog.org"
;;    :src-text "p(a). p(b). p(c)."
;;    :ask "p(X)"
;;    :oncreate (lambda (pengine event)
;;                (message "Pengine created: %s" (pengine-id pengine)))
;;    :onsuccess (lambda (pengine event)
;;                 (message "Solutions: %S" (pengine-event-data event))
;;                 (when (pengine-event-more event)
;;                   (pengine-next pengine)))
;;    :onfailure (lambda (pengine event)
;;                 (message "Query failed")))
;;
;; See README.md for full documentation.

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)
(require 'cl-lib)

;;; Customization

(defgroup pengine nil
  "SWI-Prolog Pengines client."
  :group 'external
  :prefix "pengine-")

(defcustom pengine-default-server "https://pengines.swi-prolog.org"
  "Default Pengines server URL."
  :type 'string
  :group 'pengine)

(defcustom pengine-default-application "pengine_sandbox"
  "Default application name for pengines."
  :type 'string
  :group 'pengine)

(defcustom pengine-request-timeout 300
  "Timeout for pengine HTTP requests in seconds."
  :type 'integer
  :group 'pengine)

(defcustom pengine-debug nil
  "Enable debug messages for pengine operations.
When non-nil, pengine will output detailed debug information."
  :type 'boolean
  :group 'pengine)

;;; Data structures

(cl-defstruct (pengine (:constructor pengine--create)
                       (:copier nil))
  "A pengine object representing a remote Prolog engine."
  id                    ; Pengine ID (string)
  server                ; Server URL
  application           ; Application name
  destroy-on-success    ; Whether to auto-destroy
  format                ; Response format (json, prolog, etc.)
  oncreate              ; Callback for create event
  onsuccess             ; Callback for success event
  onfailure             ; Callback for failure event
  onerror               ; Callback for error event
  onprompt              ; Callback for prompt event
  onoutput              ; Callback for output event
  onstop                ; Callback for stop event
  onabort               ; Callback for abort event
  ondestroy             ; Callback for destroy event
  state)                ; Current state

(cl-defstruct (pengine-event (:constructor pengine-event--create)
                              (:copier nil))
  "An event received from a pengine."
  event                 ; Event type (symbol)
  id                    ; Pengine ID
  data                  ; Event data
  more                  ; Whether more solutions exist (for success)
  code                  ; Error code (for error)
  time                  ; Time taken (for success)
  projection)           ; Variable projection (for success)

;;; Internal variables

(defvar pengine--active-pengines (make-hash-table :test 'equal)
  "Hash table of active pengines, keyed by pengine ID.")

(defvar pengine--http-buffer-pengine-id nil
  "Buffer-local variable storing pengine ID for HTTP response buffers.")
(make-variable-buffer-local 'pengine--http-buffer-pengine-id)

;;; HTTP utilities

(defun pengine--url-encode-params (params)
  "Encode PARAMS as URL query parameters.
PARAMS is an alist of (KEY . VALUE) pairs."
  (mapconcat
   (lambda (pair)
     (concat (url-hexify-string (format "%s" (car pair)))
             "="
             (url-hexify-string (format "%s" (cdr pair)))))
   params
   "&"))

(defun pengine--make-url (server path &optional params)
  "Construct a URL for SERVER with PATH and optional query PARAMS."
  (let ((base-url (concat server path)))
    (if params
        (concat base-url "?" (pengine--url-encode-params params))
      base-url)))

(defun pengine--http-post-json (url data callback &optional error-callback)
  "POST JSON DATA to URL and call CALLBACK with parsed response.
On error, call ERROR-CALLBACK if provided, or signal an error."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json; charset=utf-8")))
        (url-request-data (encode-coding-string
                          (json-encode data)
                          'utf-8))
        (url-http-attempt-keepalives nil))
    (url-retrieve
     url
     (lambda (status)
       (pengine--handle-http-response status callback error-callback))
     nil
     t)))

(defun pengine--http-get-json (url callback &optional error-callback)
  "GET from URL and call CALLBACK with parsed JSON response.
On error, call ERROR-CALLBACK if provided, or signal an error."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Accept" . "application/json")))
        (url-http-attempt-keepalives nil))
    (url-retrieve
     url
     (lambda (status)
       (pengine--handle-http-response status callback error-callback))
     nil
     t)))

(defun pengine--handle-http-response (status callback error-callback)
  "Handle HTTP response with STATUS, calling CALLBACK or ERROR-CALLBACK."
  (let ((pengine-id pengine--http-buffer-pengine-id))
    (unwind-protect
        (cond
         ;; Check for errors in status
         ((plist-get status :error)
          (if error-callback
              (funcall error-callback (plist-get status :error))
            (error "HTTP request failed: %S" (plist-get status :error))))
         
         ;; Success - parse response
         (t
          (goto-char (point-min))
          ;; Skip headers
          (re-search-forward "^\r?$" nil t)
          (let* ((json-object-type 'plist)
                 (json-array-type 'list)
                 (json-key-type 'keyword)
                 (json-false nil)
                 (json-null nil)
                 (response (condition-case err
                               (json-read)
                             (error
                              (if error-callback
                                  (funcall error-callback
                                          (format "JSON parse error: %S" err))
                                (error "Failed to parse JSON: %S" err))
                              nil))))
            (when response
              (funcall callback response)))))
      (kill-buffer (current-buffer)))))

;;; JSON/Prolog term conversion

(defun pengine--prolog-term-to-json (term)
  "Convert a Prolog-like TERM to JSON-compatible structure.
Supports atoms, numbers, lists, and compound terms."
  (cond
   ;; Nil
   ((null term) :null)
   
   ;; Strings become JSON strings
   ((stringp term) term)
   
   ;; Numbers
   ((numberp term) term)
   
   ;; Lists
   ((and (listp term) (not (eq (car-safe term) 'compound)))
    (mapcar #'pengine--prolog-term-to-json term))
   
   ;; Compound terms: (compound functor . args)
   ((and (listp term) (eq (car term) 'compound))
    (let ((functor (cadr term))
          (args (cddr term)))
      `(:functor ,(format "%s" functor)
        :args ,(mapcar #'pengine--prolog-term-to-json args))))
   
   ;; Symbols/atoms
   ((symbolp term) (symbol-name term))
   
   ;; Default
   (t (format "%s" term))))

(defun pengine--json-to-prolog-term (json)
  "Convert JSON structure to a Prolog-like term representation.
Returns native Emacs Lisp structures that represent Prolog terms."
  (cond
   ;; Null
   ((eq json :null) nil)
   
   ;; JSON objects representing compound terms
   ((and (listp json) (plist-get json :functor))
    (let ((functor (intern (plist-get json :functor)))
          (args (mapcar #'pengine--json-to-prolog-term
                       (plist-get json :args))))
      `(compound ,functor ,@args)))
   
   ;; JSON arrays (lists)
   ((and (listp json) (not (keywordp (car-safe json))))
    (mapcar #'pengine--json-to-prolog-term json))
   
   ;; Plain values
   (t json)))

;;; Event handling

(defun pengine--dispatch-event (pengine response)
  "Dispatch an event from RESPONSE to the appropriate PENGINE callback."
  (when pengine-debug
    (message "Pengine dispatch: %S" response))
  
  (let* ((event-type-str (plist-get response :event))
         (event-type (intern event-type-str))
         (event-id (plist-get response :id))
         (event-data (plist-get response :data))
         (event-more (plist-get response :more))
         (event-time (plist-get response :time))
         (event-projection (plist-get response :projection))
         (event-code (plist-get response :code))
         (event (pengine-event--create
                :event event-type
                :id event-id
                :data event-data
                :more event-more
                :time event-time
                :projection event-projection
                :code event-code)))
    
    (pcase event-type
      ('create
       (setf (pengine-id pengine) event-id)
       (puthash event-id pengine pengine--active-pengines)
       (when (pengine-oncreate pengine)
         (funcall (pengine-oncreate pengine) pengine event))
       ;; Check for embedded answer in create response
       (let ((answer (plist-get response :answer)))
         (when answer
           ;; If answer is a destroy event with nested data, extract the success event
           (if (equal (plist-get answer :event) "destroy")
               (let ((nested-data (plist-get answer :data)))
                 (when nested-data
                   (pengine--dispatch-event pengine nested-data)))
             ;; Otherwise dispatch the answer directly
             (pengine--dispatch-event pengine answer)))))
      
      ('success
       (when (pengine-onsuccess pengine)
         (funcall (pengine-onsuccess pengine) pengine event))
       (when (and (pengine-destroy-on-success pengine)
                  (not event-more))
         (pengine--destroy-internal pengine)))
      
      ('failure
       (when (pengine-onfailure pengine)
         (funcall (pengine-onfailure pengine) pengine event))
       (when (pengine-destroy-on-success pengine)
         (pengine--destroy-internal pengine)))
      
      ('error
       (when (pengine-onerror pengine)
         (funcall (pengine-onerror pengine) pengine event)))
      
      ('stop
       (when (pengine-onstop pengine)
         (funcall (pengine-onstop pengine) pengine event)))
      
      ('abort
       (when (pengine-onabort pengine)
         (funcall (pengine-onabort pengine) pengine event)))
      
      ('prompt
       (when (pengine-onprompt pengine)
         (funcall (pengine-onprompt pengine) pengine event)))
      
      ('output
       (when (pengine-onoutput pengine)
         (funcall (pengine-onoutput pengine) pengine event)))
      
      ('destroy
       (remhash (pengine-id pengine) pengine--active-pengines)
       (when (pengine-ondestroy pengine)
         (funcall (pengine-ondestroy pengine) pengine event)))
      
      (_ (message "Unknown pengine event type: %s" event-type)))))

;;; Internal API functions

(defun pengine--destroy-internal (pengine)
  "Internal function to destroy PENGINE."
  (when (pengine-id pengine)
    (remhash (pengine-id pengine) pengine--active-pengines)))

;;; Public API

;;;###autoload
(cl-defun pengine-create (&key
                          (server pengine-default-server)
                          (application pengine-default-application)
                          (destroy t)
                          (format "json")
                          ask
                          template
                          chunk
                          src-text
                          src-url
                          (oncreate nil)
                          (onsuccess nil)
                          (onfailure nil)
                          (onerror nil)
                          (onprompt nil)
                          (onoutput nil)
                          (onstop nil)
                          (onabort nil)
                          (ondestroy nil))
  "Create a new pengine on SERVER.

Required arguments (with defaults):
  :server SERVER        - URL of pengine server (default: pengine-default-server)
  :application APP      - Application name (default: pengine-default-application)
  :destroy BOOL         - Auto-destroy on completion (default: t)
  :format FORMAT        - Response format (default: \"json\")

Optional query arguments:
  :ask QUERY            - Initial query to run
  :template TEMPLATE    - Template for variable bindings
  :chunk INTEGER        - Number of solutions per chunk (default: 1)

Optional source code:
  :src-text TEXT        - Prolog source code as string
  :src-url URL          - URL to load Prolog source from

Callbacks (each receives PENGINE and EVENT arguments):
  :oncreate FUNC        - Called when pengine is created
  :onsuccess FUNC       - Called on successful query result
  :onfailure FUNC       - Called when query fails
  :onerror FUNC         - Called on error
  :onprompt FUNC        - Called when pengine_input/2 prompts
  :onoutput FUNC        - Called when pengine_output/1 outputs
  :onstop FUNC          - Called when query is stopped
  :onabort FUNC         - Called when query is aborted
  :ondestroy FUNC       - Called when pengine is destroyed

Returns a pengine object."
  (let ((pengine (pengine--create
                 :server server
                 :application application
                 :destroy-on-success destroy
                 :format format
                 :oncreate oncreate
                 :onsuccess onsuccess
                 :onfailure onfailure
                 :onerror onerror
                 :onprompt onprompt
                 :onoutput onoutput
                 :onstop onstop
                 :onabort onabort
                 :ondestroy ondestroy
                 :state 'creating)))
    
    ;; Build request data
    (let ((data (list :format format
                     :application application
                     :destroy destroy)))
      
      ;; Add optional parameters
      (when ask
        (setq data (plist-put data :ask ask)))
      (when template
        (setq data (plist-put data :template template)))
      (when chunk
        (setq data (plist-put data :chunk chunk)))
      (when src-text
        (setq data (plist-put data :src_text src-text)))
      (when src-url
        (setq data (plist-put data :src_url src-url)))
      
      ;; Send create request
      (pengine--http-post-json
       (pengine--make-url server "/pengine/create")
       data
       (lambda (response)
         (pengine--dispatch-event pengine response))
       (lambda (error)
         (when onerror
           (let ((event (pengine-event--create
                        :event 'error
                        :data (format "HTTP error: %S" error))))
             (funcall onerror pengine event))))))
    
    pengine))

;;;###autoload
(defun pengine-ask (pengine query &optional template chunk)
  "Ask PENGINE a QUERY.
Optional TEMPLATE specifies the variable template.
Optional CHUNK specifies the chunk size for solutions."
  (unless (pengine-id pengine)
    (error "Pengine not yet created"))
  
  (let ((url (pengine--make-url
              (pengine-server pengine)
              "/pengine/send"
              `((id . ,(pengine-id pengine))
                (format . ,(pengine-format pengine)))))
        (data (concat "ask(" query 
                     (if template 
                         (format ", [template(%s)]" template)
                       "")
                     (if chunk
                         (format ", [chunk(%d)]" chunk)
                       "")
                     ").\n")))
    
    ;; Send as Prolog term, not JSON
    (let ((url-request-method "POST")
          (url-request-extra-headers
           '(("Content-Type" . "application/x-prolog; charset=UTF-8")))
          (url-request-data (encode-coding-string data 'utf-8))
          (url-http-attempt-keepalives nil))
      (url-retrieve
       url
       (lambda (status)
         (pengine--handle-http-response 
          status
          (lambda (response)
            (pengine--dispatch-event pengine response))
          (lambda (error)
            (when (pengine-onerror pengine)
              (let ((event (pengine-event--create
                           :event 'error
                           :data (format "HTTP error: %S" error))))
                (funcall (pengine-onerror pengine) pengine event))))))
       nil
       t))))

;;;###autoload
(defun pengine-next (pengine &optional chunk)
  "Request next solution from PENGINE.
Optional CHUNK specifies the chunk size."
  (unless (pengine-id pengine)
    (error "Pengine not yet created"))
  
  (let ((params `((id . ,(pengine-id pengine))
                 (format . ,(pengine-format pengine)))))
    (when chunk
      (push `(chunk . ,chunk) params))
    
    (pengine--http-get-json
     (pengine--make-url
      (pengine-server pengine)
      "/pengine/pull_response"
      params)
     (lambda (response)
       (pengine--dispatch-event pengine response))
     (lambda (error)
       (when (pengine-onerror pengine)
         (let ((event (pengine-event--create
                      :event 'error
                      :data (format "HTTP error: %S" error))))
           (funcall (pengine-onerror pengine) pengine event)))))))

;;;###autoload
(defun pengine-stop (pengine)
  "Stop the current query in PENGINE."
  (unless (pengine-id pengine)
    (error "Pengine not yet created"))
  
  (pengine--http-get-json
   (pengine--make-url
    (pengine-server pengine)
    "/pengine/stop"
    `((id . ,(pengine-id pengine))))
   (lambda (response)
     (pengine--dispatch-event pengine response))
   (lambda (error)
     (when (pengine-onerror pengine)
       (let ((event (pengine-event--create
                    :event 'error
                    :data (format "HTTP error: %S" error))))
         (funcall (pengine-onerror pengine) pengine event))))))

;;;###autoload
(defun pengine-abort (pengine)
  "Abort the current query in PENGINE by force."
  (unless (pengine-id pengine)
    (error "Pengine not yet created"))
  
  (pengine--http-get-json
   (pengine--make-url
    (pengine-server pengine)
    "/pengine/abort"
    `((id . ,(pengine-id pengine))))
   (lambda (response)
     (pengine--dispatch-event pengine response))
   (lambda (error)
     (when (pengine-onerror pengine)
       (let ((event (pengine-event--create
                    :event 'error
                    :data (format "HTTP error: %S" error))))
         (funcall (pengine-onerror pengine) pengine event))))))

;;;###autoload
(defun pengine-destroy (pengine &optional force)
  "Destroy PENGINE.
If FORCE is non-nil, kill the pengine forcefully."
  (when (pengine-id pengine)
    (let ((params `((id . ,(pengine-id pengine)))))
      (when force
        (push '(force . true) params))
      
      (pengine--http-get-json
       (pengine--make-url
        (pengine-server pengine)
        "/pengine/destroy"
        params)
       (lambda (response)
         (pengine--dispatch-event pengine response))
       (lambda (error)
         ;; Even on error, remove from active pengines
         (pengine--destroy-internal pengine)
         (when (pengine-onerror pengine)
           (let ((event (pengine-event--create
                        :event 'error
                        :data (format "HTTP error: %S" error))))
             (funcall (pengine-onerror pengine) pengine event))))))))

;;;###autoload
(defun pengine-respond (pengine input)
  "Send INPUT in response to a prompt from PENGINE.
INPUT should be a string containing a Prolog term."
  (unless (pengine-id pengine)
    (error "Pengine not yet created"))
  
  (let ((data (if (stringp input)
                  input
                (format "%S" input))))
    (pengine--http-post-json
     (pengine--make-url
      (pengine-server pengine)
      "/pengine/send"
      `((id . ,(pengine-id pengine))
        (format . ,(pengine-format pengine))))
     data
     (lambda (response)
       (pengine--dispatch-event pengine response))
     (lambda (error)
       (when (pengine-onerror pengine)
         (let ((event (pengine-event--create
                      :event 'error
                      :data (format "HTTP error: %S" error))))
           (funcall (pengine-onerror pengine) pengine event)))))))

;;; Convenience functions

;;;###autoload
(defun pengine-query (server query &optional callback)
  "Simple query interface: run QUERY on SERVER and call CALLBACK with results.
CALLBACK is called with a list of solution bindings.
Returns the pengine object."
  (pengine-create
   :server server
   :ask query
   :chunk 1000
   :destroy t
   :onsuccess (lambda (pengine event)
                (when callback
                  (funcall callback (pengine-event-data event))))
   :onfailure (lambda (pengine event)
                (when callback
                  (funcall callback nil)))
   :onerror (lambda (pengine event)
              (message "Pengine error: %s" (pengine-event-data event))
              (when callback
                (funcall callback nil)))))

;;; Interactive functions

;;;###autoload
(defun pengine-query-interactive (server query)
  "Interactively query SERVER with QUERY and display results.
Prompts for SERVER and QUERY."
  (interactive
   (list (read-string "Server: " pengine-default-server)
         (read-string "Query: ")))
  
  (pengine-query
   server
   query
   (lambda (solutions)
     (if solutions
         (with-current-buffer (get-buffer-create "*Pengine Results*")
           (erase-buffer)
           (insert (format "Query: %s\n\n" query))
           (insert (format "Found %d solution(s):\n\n" (length solutions)))
           (dolist (solution solutions)
             (insert (format "%S\n" solution)))
           (goto-char (point-min))
           (display-buffer (current-buffer)))
       (message "Query failed or returned no solutions")))))

;;; Utility functions

;;;###autoload
(defun pengine-list-active ()
  "List all active pengines."
  (interactive)
  (let ((pengines (hash-table-values pengine--active-pengines)))
    (if pengines
        (with-current-buffer (get-buffer-create "*Active Pengines*")
          (erase-buffer)
          (insert "Active Pengines:\n\n")
          (dolist (p pengines)
            (insert (format "ID: %s\nServer: %s\nApplication: %s\n\n"
                          (pengine-id p)
                          (pengine-server p)
                          (pengine-application p))))
          (goto-char (point-min))
          (display-buffer (current-buffer)))
      (message "No active pengines"))))

;;;###autoload
(defun pengine-destroy-all ()
  "Destroy all active pengines."
  (interactive)
  (let ((count 0))
    (maphash (lambda (id pengine)
              (pengine-destroy pengine)
              (setq count (1+ count)))
            pengine--active-pengines)
    (message "Destroyed %d pengine(s)" count)))

(provide 'pengine)

;;; pengine.el ends here
