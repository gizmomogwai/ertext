;;; -*- lexical-binding: t -*-
;;(setq load-path (cons (file-name-directory (buffer-file-name)) load-path))
;;(require 'ertext)
(require 'ert-expectations)

(defun ertext/parent-directory(directory)
  "Return the parent directory of DIRECTORY."
  (file-name-directory (directory-file-name directory)))

(defun ertext/find-matching-rtext-file
  (filename exists-p matches &optional path)
  "Return the rtextfile and the command from the first matching .rtext file for FILENAME or nil.

The first matching file is an existing .rtext file in FILENAME's directory hierarchie, that has a command associated with FILENAME. EXISTS-P and MATCHES are used to analyze this. EXISTS-P is usually just `file-exists-p', MATCHES has to open the file and check if it contains a matching pattern and return the associated command."
  (message "%S %S %S %S" filename exists-p matches path)
  (let* ((current-dir (if path path (file-name-directory filename)))
         (rtext-file-name (expand-file-name ".rtext" current-dir))
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

(setq ertext/glob-command-regex "\\(.*\\):\n\\(.*\\)\n")

(defun ertext/glob-pattern-command-matcher-with-file (pattern-command-file filename)
  "Return a matching command from PATTERN-COMMAND-FILE for FILENAME or nil."
  (let* ((pairs (ertext/map-regex-with-file "./.rtext" ertext/glob-command-regex (function ertext/extract-first-and-second-from-match))))
    (ertext/glob-pattern-command-matcher pairs filename)))

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
      '(("*.test" "command1") ("*.test2" "command2"))
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

  (desc "string-match-fully-p matches")
  (expect t (ertext/string-match-fully-p "abc" "abc"))

  (desc "string-match-fully-p matches not")
  (expect nil (ertext/string-match-fully-p "abc" "abcd"))

  )
