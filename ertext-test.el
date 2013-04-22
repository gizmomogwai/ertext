;; TODO beispiel navigate-to erst load-model dann navigate-to und nochmal das mapping pattern + .rtext-file -> process richtig machen
;;; -*- lexical-binding: t -*-
;;(setq load-path (cons (file-name-directory (buffer-file-name)) load-path))
;;(require 'ertext)
(require 'ert-expectations)
(require 'json)
(require 'completion-ui)

(defvar ertext/service-regex "RText service, listening on port \\(.*\\)\n"
  "Regex used for finding the communication port of a RText service.")
(defvar ertext/glob-command-regex "\\(.*\\):\n\\(.*\\)\n"
  "Regex used to process .rtext files.")
(defvar ertext/config-filename ".rtext"
  "Filename of RText configuration files.")
(defvar ertext/buffer-basename "ertext"
  "Basename for buffers associated with ertext-processes.")
(defvar ertext/connection-basename "ertext-connection"
  "Basename for buffers for network connections to ertext-processes.")
(defvar ertext/invocation-id 1
  "The global invocation id shared for all RText connections.")
(defvar ertext/json-header "^\\([0-9]+\\){"
  "Regex to parse answers from the server.")

(defvar ertext/rtextconfig-2-processes-map (make-hash-table :test 'equal)
  "Maps from rtext config files to created processes. The key is pattern@full-config-file-path, the values are a hash with keys :socket and :process to process objects.")

(defconst ertext/process-data-key "data")
(defconst ertext/process-callback-key "callback")

(defun ertext/parent-directory(directory)
  "Return the parent directory of DIRECTORY."
  (file-name-directory (directory-file-name directory)))

(defun ertext/find-matching-rtext-file
  (filename exists-p matches &optional path)
  "Return the rtextfile, the command and the pattern from the first matching .rtext file for FILENAME or nil.
The first matching file is an existing .rtext file in FILENAME's directory hierarchie, that has a command associated with FILENAME. EXISTS-P and MATCHES are used to analyze this. EXISTS-P is usually just `file-exists-p', MATCHES has to open the file and check if it contains a matching pattern and return the associated command."
  (let* ((current-dir (expand-file-name (if path path (file-name-directory filename))))
         (rtext-file-name (expand-file-name ertext/config-filename current-dir))
         (rtext-exists (funcall exists-p rtext-file-name))
         (finished (and rtext-exists (funcall matches rtext-file-name filename))))
    (if finished (list rtext-file-name (first finished) (second finished))
      (let* ((parent-dir (ertext/parent-directory current-dir))
             (new-dir (not (string= parent-dir current-dir))))
        (if new-dir (ertext/find-matching-rtext-file filename exists-p matches (ertext/parent-directory current-dir)) nil)))))

;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman <zzbba...@aol.com>”. 2010-09-02
(defun ertext/get-string-from-file(filename)
  "Return FILENAME's file content."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun ertext/map-regex (text regex fn)
  "Map the REGEX over the FILENAME executing FN.
   FN is called for each successful `string-match' for the content of FILENAME.
   Returns the results of the FN as a list."
  (let* ((search-idx 0)
         (res))
    (while (string-match regex text search-idx)
      (setq res (append res (list (funcall fn text))))
      (setq search-idx (match-end 0)))
    res))

(defun ertext/map-regex-with-file (filename regex fn)
  "Use `ertext/map-regex' with the contents of FILENAME."
  (ertext/map-regex (ertext/get-string-from-file filename) regex fn))

(defun ertext/extract-first-and-second-from-match (text)
  "Return list with the first and second match-group.
See `string-match' and `match-string'."
  (list (match-string 1 text) (match-string 2 text)))

(defun ertext/get-rtext-command(rtext-filename filename)
  "Return the associated command from the file RTEXT-FILENAME for FILENAME or nil."
  (if (file-exists-p rtext-filename)
      ()
    nil))

(defun ertext/glob-pattern-to-regexp(pattern)
  "Return regexp matching on the given PATTERN."
  (let ((res ""))
    (mapc (lambda (c) (cond
                       ((char-equal c ?.) (setq res (concat res (list ?\\ ?.))))
                       ((char-equal c ?*) (setq res (concat res (list ?. ?*))))
                       (t (setq res (concat res (list c)))))) pattern)
    res))

(defun ertext/string-match-fully-p (pattern text)
  "Return t if PATTERN matches TEXT fully."
  (let ((match (string-match pattern text)))
    (if match (eq (match-end 0) (length text)) nil)))

(defun ertext/glob-pattern-command-matcher (pairs text)
  "Return the matching pattern and command for TEXT.
Pairs is a list of regexp strings and commands."
  (dolist (head pairs)
    (if (ertext/string-match-fully-p (ertext/glob-pattern-to-regexp (car head)) text)
        (return head))))

(defun ertext/glob-pattern-command-matcher-with-file (pattern-command-file filename)
  "Return a matching command from PATTERN-COMMAND-FILE for FILENAME or nil."
  (let* ((pairs (ertext/map-regex-with-file ertext/config-filename ertext/glob-command-regex (function ertext/extract-first-and-second-from-match))))
    (ertext/glob-pattern-command-matcher pairs filename)))

(defun ertext/get-rtext-and-pattern-and-command (filename)
  "Return rtext-file, pattern and command for FILENAME or nil."
  (ertext/find-matching-rtext-file filename (function file-exists-p) (function ertext/glob-pattern-command-matcher-with-file)))

(defun ertext/start-rtext-process (command)
  "Return the process and the port of the launched rText process given by COMMAND."
  (let* ((process-connection-type nil)
         (buffer-name (generate-new-buffer "ertext"))
         (process (apply 'start-process (append (list "ertext" buffer-name) (split-string command)))))
    (accept-process-output process 1 nil t)
    (list process
          (let* ((string (with-current-buffer buffer-name (buffer-string)))
                 (bummer (string-match ertext/service-regex string))
                 (match (match-string 1 string))
                 (res (if match (string-to-number match) nil)))
            res))))

(defun ertext/protocol-get-json-data (string)
  "Return the pure json data or nil if not enough data available."
  (let* ((match (string-match ertext/json-header string))
         (json-length (if match (string-to-number (match-string 1 string))))
         (header-length (if match (match-end 1)))
         (minimum-length (+ json-length header-length)))
    (if (>= (length string) minimum-length)
        (list (substring string header-length minimum-length) (substring string minimum-length))
      nil)))

(defun ertext/protocol-get-json (string)
  "Like `ertext/protocol-get-json-data' but the json-data is parsed."
  (let* ((help (ertext/protocol-get-json-data string))
         (json (if help (json-read-from-string (car help))))
         (res (if json (list json (second help)))))
    res))

(defun ertext/protocol-collect-and-process-output (process content)
  "Gets invoked whenever the server sends data to the client.
It collects the data and checks if enough data for a package of the
RText-Protocol is received. Then it calls the callback, that is
associated with process."
  (process-put process ertext/process-data-key (concat (process-get process ertext/process-data-key) content))
  (let ((json-and-data (ertext/protocol-get-json (process-get process ertext/process-data-key))))
    (if json-and-data
        (progn
          (process-put process ertext/process-data-key (second json-and-data))
          (funcall (process-get process ertext/process-callback-key) process (car json-and-data))))))

(defun ertext/protocol-json-response-received (process json)
  "Called whenever a json response is received."
  (message "%s got %s" process (json-read-from-string json)))

(defun ertext/connect-to-service (port)
  "Connect to a RText service that is listening on PORT."
  (message "connecting to %S" port)
  (let* ((buffer-name (generate-new-buffer "ertext-connection"))
         (process (open-network-stream "ertext-connection" nil "localhost" port)))
    (set-process-filter-multibyte process t)
    (set-process-coding-system process 'utf-8 'utf-8)
    (process-put process ertext/process-callback-key 'ertext/protocol-json-response-received)
    (set-process-filter process 'ertext/protocol-collect-and-process-output)
;;    (ACCEPT-process-output process 1 0 t)
    process))

(defun ertext/calc-rtext-and-pattern-key (rtext pattern)
  ""
  (format "%s@%s" pattern rtext))

(defun ertext/connect-to-rtext-process (filename)
  "Launch the defined command for FILENAME."
  (let* ((found (ertext/get-rtext-and-pattern-and-command filename))
        (rtext (first found))
        (pattern (second found))
        (command (third found)))
    (if found
        (let* ((process-and-port (ertext/start-rtext-process command))
               (process (first process-and-port))
               (port (second process-and-port)))
          (if port
              (list (ertext/calc-rtext-and-pattern-key rtext pattern)
                    process
                    (ertext/connect-to-service port)))))))

(defun my-rtext-exists-p (filename) (string= (expand-file-name "/my/very/long/path/.rtext") filename))
(defun my-rtext-match-p (rtext-config filename) (list "*.test" "command"))

(expectations
  (desc "directory-file-name gets the filename of a directory")
  (expect "/abc/def" (directory-file-name "/abc/def/"))

  (desc "file-name-directory gets the parent directory for a non directory file")
  (expect "/abc/" (file-name-directory "/abc/test.txt"))

  (desc "file-name-directory gets itself for a directory")
  (expect "/abc/test/" (file-name-directory "/abc/test/"))
  (desc "ertext/parent-directory")
  (expect "/abc/" (ertext/parent-directory "/abc/def/"))

  (desc "ertext/parent-directory")
  (expect "/" (ertext/parent-directory "/"))

  (desc "ertext/find-matching-rtext-file")
  (expect (list (expand-file-name "/my/very/long/path/.rtext") "*.test" "command") (ertext/find-matching-rtext-file "/my/very/long/path/123.txt" (function my-rtext-exists-p) (function my-rtext-match-p)))

  (desc "ertext/map-regex")
  (expect '(("abc" "def")) (ertext/map-regex "abc:\ndef\n" ertext/glob-command-regex (function ertext/extract-first-and-second-from-match)))

  (desc "ertext/map-regex-with-file")
  (expect
      '(
        ("*.test" "command1")
        ("*.test2" "command2")
        ("*.rtest" "ruby ./test.rb")
        ("*.rtext-process" "ruby -I../rgen/lib -I../rtext/lib ../rtext/test/integration/ecore_editor.rb \"*.ect\"")
        )
    (ertext/map-regex-with-file ".rtext" ertext/glob-command-regex (function ertext/extract-first-and-second-from-match)))

  (desc "transform glob-pattern to emacs regexp")
  (expect "abc" (ertext/glob-pattern-to-regexp "abc"))

  (desc "transform glob-pattern with . to emacs regexp")
  (expect "a\\.b" (ertext/glob-pattern-to-regexp "a.b"))

  (desc "transform glob-pattern with * to emacs regexp")
  (expect "a.*b" (ertext/glob-pattern-to-regexp "a*b"))

  (desc "glob-pattern-command-matcher")
  (expect (list "*.text" "command1") (ertext/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text"))

  (desc "glob-pattern-command-matcher")
  (expect (list "*.text2" "command2") (ertext/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text2"))

  (desc "glob-pattern-command-matcher")
  (expect nil (ertext/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text3"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect (list "*.test" "command1") (ertext/glob-pattern-command-matcher-with-file "./.rtext" "test.test"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect (list "*.test2" "command2") (ertext/glob-pattern-command-matcher-with-file "./.rtext" "test.test2"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect nil (ertext/glob-pattern-command-matcher-with-file "./.rtext" "test.test3"))

  (desc "get rtextfile and command for filename")
  (expect (list (expand-file-name "./.rtext") "*.test2" "command2") (ertext/find-matching-rtext-file "./blub.test2" (function file-exists-p) (function ertext/glob-pattern-command-matcher-with-file)))

  (desc "convinient get rtextfile and command")
  (expect (list (expand-file-name "./.rtext") "*.test2" "command2") (ertext/get-rtext-and-pattern-and-command "./blub.test2"))

  (desc "convinient get rtextfile and command -> nil")
  (expect nil (ertext/get-rtext-and-pattern-and-command "./blub.test3"))

  (desc "string-match-fully-p matches")
  (expect t (ertext/string-match-fully-p "abc" "abc"))

  (desc "string-match-fully-p matches not")
  (expect nil (ertext/string-match-fully-p "abc" "abcd"))

  (desc "test json protocol stuff")
  (expect 0 (string-match ertext/json-header "15{"))

  (desc "test json protocol stuff")
  (expect nil (string-match ertext/json-header "{"))

  (desc "connection-get-json-data - not enough data")
  (expect nil (ertext/protocol-get-json-data "3{1"))

  (desc "connection-get-json-data - enough data")
  (expect '("{\"1\":2}" "") (ertext/protocol-get-json-data "7{\"1\":2}"))

  (desc "connection-get-json-data - too much data")
  (expect '("{1}" "123") (ertext/protocol-get-json-data "3{1}123"))
;;  TODO use protocol-get-json-data in protocol-collect-and-process-output

  (desc "connection-get-json - not enough data")
  (expect nil (ertext/protocol-get-json "3{1"))

  (desc "connection-get-json-data - enough data")
  (expect '(((\1 . 2)) "") (ertext/protocol-get-json "7{\"1\":2}"))

  (desc "connection-get-json-data - too much data")
  (expect '(((\1 . 2)) "123") (ertext/protocol-get-json "7{\"1\":2}123"))

  (desc "key calc for rtext and pattern")
  (expect "pattern@file" (ertext/calc-rtext-and-pattern-key "file" "pattern"))

  (desc "get completion candidates from answer")
  (expect (list "EAnnotation" "EClass" "EClassifier")
    (ertext/response-to-completion-candidates
     '((invocation_id . 7)
       (type . "response")
       (options . [((display . "EAnnotation ") (insert . "EAnnotation")) ((display . "EClass <name>") (insert . "EClass")) ((display . "EClassifier <name>") (insert . "EClassifier"))]))))

  )

(defun ertext/response-to-completion-candidates (response)
  "Convert the response to a list of strings used for completion."
  (let* ((options (cdr (assoc 'options response)))
         (type (cdr (assoc 'type response)))
         (invocation (cdr (assoc 'invocation_id response))))
    (setq o options)
    (if (string-equal type "response")
        (mapcar 'car (mapcar 'car options)))))
      (mapcar 'cdr ((mapcar 'car (mapcar 'cdr options)))

(defun ertext/get-process (filename)
  "Return the process responsible for rText for FILENAME."
  (let* ((found (ertext/get-rtext-and-pattern-and-command filename))
         (rtext (first found))
         (pattern (second found))
         (key (ertext/calc-rtext-and-pattern-key rtext pattern))
         (already-launched (gethash key ertext/rtextconfig-2-processes-map)))
    (if already-launched (gethash :socket already-launched)
      (let* ((launch-info (ertext/connect-to-rtext-process buffer))
             (process (second launch-info))
             (socket (third launch-info))
             (res (make-hash-table :test 'equal))
             (new-entry (make-hash-table :test 'equal)))
        (puthash :process process new-entry)
        (puthash :socket socket new-entry)
        (puthash key new-entry ertext/rtextconfig-2-processes-map)
        (ertext/load-model socket)
        socket))))

(defun ertext/load-model(protocol)
  "Sends a load_model request on communication channel PROTOCOL."
  (ertext/send-request protocol
   (list (cons "type" "request")
         (cons "invocation_id" 6)
         (cons "command" "load_model"))))

(defun ertext/send-request(protocol request)
  "Sends the REQUEST with PROTOCOL."
  (process-send-string
   protocol
   (let* ((json (json-encode request))
          (length (number-to-string (length json)))
          (data (concat length json)))
     (message "Sending: %s" data)
     data)))

(defun ertext/send-request-for-current-cursor(request)
  "Gets context for current cursor position."
  (let* ((buffer (expand-file-name  (buffer-name)))
         (line (count-lines 1 (point)))
         (column (current-column))
         (process (ertext/get-process buffer))
         (context (ertext/context/get-from-current-buffer))
         (json (list
                (cons "type" "request")
                (cons "invocation_id"  7)
                (cons "command" request)
                (cons "context" context)
                (cons "column" column))))
    (message "Context for %S:%d.%d -> %S\nContext:\n%S\njson:\n%S" buffer line column process context json)
    (ertext/send-request process json)))


(defun ertext/navigate-to ()
  "Navigate to the file rText says."
  (interactive)
  (ertext/send-request-for-current-cursor "link_targets"))
(defun ertext/context()
  "Show context."
  (interactive)
  (ertext/send-request-for-current-cursor "context_info"))
(defun ertext/content-complete()
  "Show content complete."
  (interactive)
  (ertext/send-request-for-current-cursor "content_complete"))

;(let* ((test "abc") (json (json-encode '(("type" . "request") ("test" . 'test))))) json)
;
;;(setq h (ertext/connect-to-rtext-process "./test.rtext-process"))
;(process-send-string h (let* ((json (json-encode (list (cons "type" "request") (cons "invocation_id" 6) (cons "command" "load_model"))))
;                              (length (number-to-string (length json))))
;                         (concat length json)))
;
;(accept-process-output h 1 0 t)
;(sit-for 1)
;(waiting-for-user-input-p)
;

(defun ertext/reset()
  "Clears ertext data."
  (interactive)
  (mapc #'delete-process (process-list))
  (clrhash ertext/rtextconfig-2-processes-map))
(ertext/reset)
;; json tests
;;(require 'json)
;;(setq h (json-read-from-string "{\"type\": [\"1\", 2, 3],\"command\": \"load_model\"}"))
;(setq blub "a")
;(setq h (list (cons "type" blub)))
;(json-encode  h)
;(json-read-from-string)
;;;(cdr (assoc 'command h))
;;;(setq blub "abc")
;;;(json-encode '((1 . 2) ("type" . ("blub1" "blub2"))))
;(apply 'vector (list 1 2 3))
(defun ertext/process-output-filter (process string)
  "Process output from all rtext processes."
  (message "%S -> %S" process string))



;; closure tests
;;(defun my-adder (a b)
;;  (+ a b))
;;(my-adder 1 2)
;;
;;(defun create-adder (a)
;;  (lambda (x) (my-adder a x)))
;;
;;(setq add5 (create-adder 5))
;;(setq add6 (create-adder 6))
