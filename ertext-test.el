;;; -*- lexical-binding: t -*-
;;(setq load-path (cons (file-name-directory (buffer-file-name)) load-path))
;;(require 'ertext)
(require 'ert-expectations)
(require 'json)

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
(defvar ertext/process-to-output '()
  "Maps processes to output from processed.")

(defun ertext/parent-directory(directory)
  "Return the parent directory of DIRECTORY."
  (file-name-directory (directory-file-name directory)))

(defun ertext/find-matching-rtext-file
  (filename exists-p matches &optional path)
  "Return the rtextfile and the command from the first matching .rtext file for FILENAME or nil.
The first matching file is an existing .rtext file in FILENAME's directory hierarchie, that has a command associated with FILENAME. EXISTS-P and MATCHES are used to analyze this. EXISTS-P is usually just `file-exists-p', MATCHES has to open the file and check if it contains a matching pattern and return the associated command."
  (let* ((current-dir (expand-file-name (if path path (file-name-directory filename))))
         (rtext-file-name (expand-file-name ertext/config-filename current-dir))
         (rtext-exists (funcall exists-p rtext-file-name))
         (finished (and rtext-exists (funcall matches rtext-file-name filename))))
    (if finished (list rtext-file-name finished)
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
  "Return the matching command for TEXT.
Pairs is a list of regexp strings and commands."
  (dolist (head pairs)
    (if (ertext/string-match-fully-p (ertext/glob-pattern-to-regexp (car head)) text)
        (return (second head)))))

(defun ertext/glob-pattern-command-matcher-with-file (pattern-command-file filename)
  "Return a matching command from PATTERN-COMMAND-FILE for FILENAME or nil."
  (let* ((pairs (ertext/map-regex-with-file ertext/config-filename ertext/glob-command-regex (function ertext/extract-first-and-second-from-match))))
    (ertext/glob-pattern-command-matcher pairs filename)))

(defun ertext/get-rtext-and-command (filename)
  "Return rtext-file and command for FILENAME or nil."
  (ertext/find-matching-rtext-file filename (function file-exists-p) (function ertext/glob-pattern-command-matcher-with-file)))

(defun ertext/start-rtext-process (command)
  "Return the port of the launched rText process given by COMMAND."
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
         (minimum-length (+ json-length header-length))
         (json (if (>= (length string) minimum-length)
                   (substring string header-length minimum-length))))
    (condition-case nil
        (json-read-from-string json)
      (error nil))))

(expectations
  (defun my-rtext-exists-p (filename) (string= "/my/.rtext" filename))
  (defun my-rtext-match-p (rtext-config filename) 1)

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

  (desc "mock")
  (expect 1 (my-rtext-match-p "123" "123"))

  (desc "ertext/find-matching-rtext-file")
  (expect '("/my/.rtext" 1) (ertext/find-matching-rtext-file "/my/very/long/path/123.txt" (function my-rtext-exists-p) (function my-rtext-match-p)))

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
  (expect "command1" (ertext/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text"))

  (desc "glob-pattern-command-matcher")
  (expect "command2" (ertext/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text2"))

  (desc "glob-pattern-command-matcher")
  (expect nil (ertext/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text3"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect "command1" (ertext/glob-pattern-command-matcher-with-file "./.rtext" "test.test"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect "command2" (ertext/glob-pattern-command-matcher-with-file "./.rtext" "test.test2"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect nil (ertext/glob-pattern-command-matcher-with-file "./.rtext" "test.test3"))

  (desc "get rtextfile and command for filename")
  (expect (list (expand-file-name "./.rtext") "command2") (ertext/find-matching-rtext-file "./blub.test2" (function file-exists-p) (function ertext/glob-pattern-command-matcher-with-file)))

  (desc "convinient get rtextfile and command")
  (expect (list (expand-file-name "./.rtext") "command2") (ertext/get-rtext-and-command "./blub.test2"))

  (desc "convinient get rtextfile and command -> nil")
  (expect nil (ertext/get-rtext-and-command "./blub.test3"))

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

  (desc "connection-get-json-data - not enough data")
  (expect '((\1 . 2)) (ertext/protocol-get-json-data "7{\"1\":2}"))

  
  )



(defconst ertext/process-data-key "data")

(defun ertext/protocol-collect-and-process-output (process content)
  "Gets invoked whenever the server sends data to the client.
It collects the data and checks if enough data for a package of the 
RText-Protocol is received. Then it calls the callback, that is 
associated with process."
  (process-put process ertext/process-data-key (concat (process-get process ertext/process-data-key) content))
  (let ((json-data (ertext/get-json-data (process-get process ertext/process-data-key))))
    (funcall (process-get process ertext/process-callback-key) json-data)))

(defun rtext/protocol-collect-and-process-output (callback process content)
   "Gets invoked whenever the server sends data to the client."
   (message "%S -> %S" process content)
   (rtext/protocol-collect-output process content)
   (setq ertext/process-to-output
         (let* ((list ertext/process-to-output)
                (current (plist-get list process))
                (new (if current (concat current content) content))
                (new-list (plist-put list process new)))
           new-list))
   (let* ((content (plist-get ertext/process-to-output process))
          (json-and-header-length (ertext/get-json-length content))
          (needed-length (apply '+ json-and-header-length))
          (enough-data (>= (length content) needed-length)))
     enough-data))
(setq ertext/process-to-output '())

(defun ertext/connect-to-service (port)
  "Connect to a RText service that is listening on PORT."
  (message "connecting to %S" port)
  (let* ((buffer-name (generate-new-buffer "ertext-connection"))
         (process (open-network-stream "ertext-connection" nil "localhost" port '(:nowait t))))
    (set-process-filter-multibyte process t)
    (set-process-coding-system process 'utf-8 'utf-8)
    (set-process-filter process 'handle-server-reply)
;;    (accept-process-output process 1 0 t)
    process))

(defun ertext/connect-to-rtext-process (filename)
  "Launch the defined command for FILENAME."
  (let ((rtext-and-command (ertext/get-rtext-and-command filename)))
    (if rtext-and-command
        (let* ((process-and-port (ertext/start-rtext-process (second rtext-and-command)))
               (port (second process-and-port)))
          (if port (ertext/connect-to-service port))))))

(setq h (ertext/connect-to-rtext-process "./test.rtext-process"))
(process-send-string h (let* ((json (json-encode '(("type" . "request") ("invocation_id" . 1) ("command" . "load_model"))))
                               (length (number-to-string (length json))))
                          (concat length json)))
(accept-process-output h 1 0 t)
(sit-for 1)
(waiting-for-user-input-p)
(mapc #'delete-process (process-list))

;; json tests
;;(require 'json)
;;(setq h (json-read-from-string "{\"type\": \"request\",\"command\": \"load_model\"}"))
;;(cdr (assoc 'command h))
;;(setq blub "abc")
;;(json-encode '((1 . 2) ("type" . ("blub1" "blub2"))))

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
