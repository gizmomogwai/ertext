;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(require 'completion-ui)

(defun complete-test ()
  "Minimal test."
  (list "abc" "def" "ghi"))

(defun complete-test-empty()
  "Returns just an empty prefix"
  "")
(completion-ui-register-source complete-test :name blub :completion-args 0 :prefix-function complete-test-empty)


abc
totomate
to
tes

tes

test

test
test


test(completion--semantic-wrapper "test")
(rng-complete-qname-function "test")
test

