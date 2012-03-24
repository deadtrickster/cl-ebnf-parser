(defun c++-lex-phase3 (string &optional (start 0) (end (length string)))
  "approximate phase 3"
  ())

;; 2.2 -- impl
;; approximate phase 3
(defrule whitespace
  (or (c-comment) (c++-comment) slash-t slash-n slash-v slash-f slash-r " "))


(defrule c++-lex-phase3
  (repeat 0 nil (and (preprocessing-token) (repeat 0 nil (whitespace)))))

(c++-lex-phase3 "#define a
//this is a 123456 test
/* test */
void f(int *x);
")
