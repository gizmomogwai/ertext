;;; -*- lexical-binding: t -*-
(require 'ert-expectations)
(require 'cl)
(require 's) ; need s library by magnars located at https://github.com/magnars/s.el/blob/master/s.el or just el-get-install s

(defun ertext/context/ignore-line-p (string)
  "Calculates if the string should be ignored. Ignores strings are empty strings an comment lines."
  (or (eq 0 (length string))
      (eq (elt string 0) ?#)))

(defun ertext/context/remove-ignored-lines (lines)
  "Remove all ignored lines from the list of lines."
  (remove-if 'ertext/context/ignore-line-p lines))

(defun ertext/context/trim (string)
  "Remove whitespace from the start and end of a string."
  (s-trim string))

(defun ertext/context/split-for-context (string)
  "Split STRING into lines. Use `ertext/context/remove-ignored-lines' to get only real lines."
  (ertext/context/remove-ignored-lines (split-string string)))

(defun ertext/context/extract-context (lines)
  "Extract the RText specific context from lines."
  (let* (
         (non-ignored-lines 0)
         (array-nesting 0)
         (block-nesting 0)
         (last-element-line 0)
         (res '())
         (lines (reverse lines))
         (count 0)
         )
    (while lines
      (let ((current (car lines))
            (new-lines (cdr lines))
            (new-count (1+ count))
            )
        (if
            (eq count 0)
            (setq res (append (list current) res))
          (progn
            (setq current (ertext/context/trim current))
            (if (not  (ertext/context/ignore-line-p current))
                (progn
                  (setq non-ignored-lines (1+ non-ignored-lines))
                  (cond ((s-ends-with-p "{" current)
                         (if (> block-nesting 0)
                             (setq block-nesting (1- block-nesting))
                           (if (eq block-nesting 0)
                               (progn
                                 (setq res (append (list current) res))
                                 (setq last-element-line non-ignored-lines))
                             )
                           ))
                        ((s-ends-with-p "}" current) (setq block-nesting (1+ block-nesting)))
                        ((s-ends-with-p "[" current) (if (> array-nesting 0) (setq array-nesting (1- array-nesting))
                                                       (if (eq array-nesting 0) (setq res (append  (list current) res)))))
                        ((s-ends-with-p "]" current) (setq array-nesting (1+ array-nesting)))
                        ((s-ends-with-p ":" current) (if (eq non-ignored-lines (1+ last-element-line))
                                                         (setq res (append (list current) res)))))))))
        (setq lines new-lines)
        (setq count new-count)))
    res))


;; testhelper
(defun ertext/context/get-test-context (n)
  (ertext/context/extract-context
   (ertext/context/get-first-n-lines (split-string (ertext/get-string-from-file "test/data/context.txt") "\n+") n)))

(defun ertext/context/get-test-result (n)
  (split-string (ertext/get-string-from-file (format "test/data/context_result_%d.txt" n)) "\n+"))

(defun ertext/context/get-first-n-lines (list n)
  "Return just the first N elements of list."
  (nbutlast list (- (length list) n)))


(expectations
  (desc "ertext/context/ignore-line-p ignore comment")
  (expect '(nil t t nil) (mapcar 'ertext/context/ignore-line-p '("abc" "#abc" "" "abc#")))

  (desc "ertext/context/remove ignored lines")
  (expect '("abc" "def") (ertext/context/remove-ignored-lines '("abc" "#abc" "" "def")))

  (desc "trim string - nothing todo")
  (expect "abc" (ertext/context/trim "abc"))

  (desc "trim string - trim at end")
  (expect "abc" (ertext/context/trim "abc   \t\r\n"))

  (desc "trim string - trim at start")
  (expect "abc" (ertext/context/trim "\t   abc"))

  (desc "split to interesting lines")
  (expect '("abc" "def") (ertext/context/split-for-context "  abc  \n  #test \n  def\t"))

  (desc "extract context trivial")
  (expect '("abc {" "def") (ertext/context/extract-context '("abc {" "def")))

  (desc "extract context with sibblings")
  (expect '("abc {" "def {") (ertext/context/extract-context '("abc {" "def1 {" "}" "def {")))

  (desc "get first 1 lines")
  (expect (list "Command 1 {")
    (ertext/context/get-first-n-lines (split-string (ertext/get-string-from-file "test/data/context.txt") "\n+") 1))

  (desc "get first 3 lines")
  (expect (list "Command 1 {" "  Command 2 {" "  }")
    (ertext/context/get-first-n-lines (split-string (ertext/get-string-from-file "test/data/context.txt") "\n+") 3))

  (desc "test/data/context.txt 1")
  (expect (ertext/context/get-test-result 1)
    (ertext/context/get-test-context 1))

  (desc "test/data/context.txt 2")
  (expect (ertext/context/get-test-result 2)
    (ertext/context/get-test-context 2))


  (desc "test/data/context.txt 3")
  (expect (ertext/context/get-test-result 3)
    (ertext/context/get-test-context 3))


  (desc "test/data/context.txt 4")
  (expect (ertext/context/get-test-result 4)
    (ertext/context/get-test-context 4))


  (desc "test/data/context.txt 5")
  (expect (ertext/context/get-test-result 5)
    (ertext/context/get-test-context 5))

  (desc "test/data/context.txt 6")
  (expect (ertext/context/get-test-result 6)
    (ertext/context/get-test-context 6))

  (desc "test/data/context.txt 7")
  (expect (ertext/context/get-test-result 7)
    (ertext/context/get-test-context 7))

  (desc "test/data/context.txt 8")
  (expect (ertext/context/get-test-result 8)
    (ertext/context/get-test-context 8))

  (desc "test/data/context.txt 9")
  (expect (ertext/context/get-test-result 9)
    (ertext/context/get-test-context 9))

  (desc "test/data/context.txt 10")
  (expect (ertext/context/get-test-result 10)
    (ertext/context/get-test-context 10))

  (desc "test/data/context.txt 11")
  (expect (ertext/context/get-test-result 11)
    (ertext/context/get-test-context 11))

  (desc "test/data/context.txt 12")
  (expect (ertext/context/get-test-result 12)
    (ertext/context/get-test-context 12))

  (desc "test/data/context.txt 13")
  (expect (ertext/context/get-test-result 13)
    (ertext/context/get-test-context 13))

  (desc "test/data/context.txt 14")
  (expect (ertext/context/get-test-result 14)
    (ertext/context/get-test-context 14))

  (desc "test/data/context.txt 15")
  (expect (ertext/context/get-test-result 15)
    (ertext/context/get-test-context 15))

  (desc "test/data/context.txt 16")
  (expect (ertext/context/get-test-result 16)
    (ertext/context/get-test-context 16))

  (desc "test/data/context.txt 17")
  (expect (ertext/context/get-test-result 17)
    (ertext/context/get-test-context 17))

  (desc "test/data/context.txt 18")
  (expect (ertext/context/get-test-result 18)
    (ertext/context/get-test-context 18))

  (desc "test/data/context.txt 19")
  (expect (ertext/context/get-test-result 19)
    (ertext/context/get-test-context 19))
  )
