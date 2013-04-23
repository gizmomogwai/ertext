(require 'completion-ui)


(require 'completion-ui)

(defun ertext/complete-ui-plugin ()
  "Minimal test."
  ertext/last-completion)
  (list "abc" "def" "ghi"))

(defun ertext/complete-ui-empty-test()
  "Returns just an empty prefix"
  "")
(completion-ui-register-source
 ertext/complete-ui-plugin
 :name ertext
 :completion-args 0
 :prefix-function ertext/complete-ui-empty-test)


ghi

