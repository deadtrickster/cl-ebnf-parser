;; A simple top-down, backtracking parser
;; Modelled after EBNF notation

(defun starts-with (string prefix &key (start 0))
  "Does 'string' begin with 'prefix'?"
  (let ((end (+ start (length prefix)))
        (l (length string)))
    (if (> end l)
        nil
        (string= prefix string :start2 start :end2 end))))

;; return a list of the children
;; return the end of the last child
(defun kleene* (f string &key (start 0))
  (multiple-value-bind (flag value end) (funcall f string :start start)
    (if flag
        (multiple-value-bind (f v e) (kleene* f string :start end)
          (values f (cons value v) e))
        (values t nil start))))

(defun kleene+ (f string &key (start 0))
  "<rule>+ == <rule> <rule>*"
  (multiple-value-bind (flag value end) (funcall f string :start start)
    (if flag
        (multiple-value-bind (f v e) (kleene* f string :start end)
          (values f (cons value v) e))
        nil)))

;; Construction macros
(defmacro grammar-string (str)
  `(if (starts-with string ,str :start start)
    (values t ,str (+ start ,(length str)))))

(defmacro grammar-and (first &rest rest)
  (if (null rest)
      first
      `(multiple-value-bind (flag value end) ,first
        (if flag
            (multiple-value-bind (f v e) (grammar-and ))))))


(defmacro grammar-or (first &rest rest)
  (if (null rest)
      first
      `(multiple-value-bind (flag value end) ,first
        (if flag
            (values flag value end)
            (grammar-or ,@rest)))))


(defun parse-test (string &key (start 0))
  "token := 'a' | 'b'"
;  (grammar-string "a"))
  (grammar-or
   (grammar-string "a")
   (grammar-string "b")))


;; Simple grammar
; token := "a" | "b"
; list := "(" token+ ")"

(defun parse-token (string &key (start 0))
  "token := 'a' | 'b'"
  (grammar-or
   (grammar-string "a")
   (grammar-string "b")))

; First writing
(defun parse-list (string &key (start 0))
  "list := '(' token* ')'"
  (if (starts-with string "(" :start start)
      (multiple-value-bind (flag value end) (kleene* 'parse-token string :start (+ start (length "(")))
        (if flag
            (when (starts-with string ")" :start end)
              (values t (list "(" value ")") (+ end (length ")"))))))))

; Refactored
(defun parse-list (string &key (start 0))
  "list := '(' token* ')'"
  (multiple-value-bind (f0 v0 e0) (grammar-string "(")
    (if f0
      (multiple-value-bind (flag value end) (kleene* 'parse-token string :start (+ start (length "(")))
        (if flag
            (when (starts-with string ")" :start end)
              (values t (list "(" value ")") (+ end (length ")")))))))))



;;;; Special conditions

;; Flag whether to ignore whitespace between tokens
(defparameter *grammar-no-white* nil)

;; List of restricted keywords
(defparameter *grammar-keywords* (make-hash-table :test #'equal))