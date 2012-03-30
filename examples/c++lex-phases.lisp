(defun c++-lex-phase3 (string &optional (start 0) (end (length string)))
  "approximate phase 3"
  ())

;; 2.2 -- impl
;; approximate phase 3
(defrule whitespace
  (or (c-comment) (c++-comment) slash-t slash-n slash-v slash-f slash-r " "))


(defrule c++-lex-phase3
  ;; need to detect "#" "include" and bind *enable-header* to t
  ;; need to make sure whitespace exists between certain tokens?
  (repeat 0 nil (or (preprocessing-token) (whitespace))))

(c++-lex-phase3 "#define a
//this is a 123456 test
/* test */
void f(int *x);
")

(defun parse-file (filename)
  (let (str
        (*filename* filename))
    (with-open-file (file filename)
      (setf str (make-sequence 'string (file-length file)))
      (read-sequence str file))
    ;; eventually return the AST, the comment list, and the preproc list
    ;; also return a list of newlines (so line/col can be quickly calculated)
    ;; also return an indication if the parse didn't consume the whole file
    (c++-lex-phase3 str)))
