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
  (multiple-value-bind (end value) (funcall f string :start start)
    (if end
        (multiple-value-bind (e v) (kleene* f string :start end)
          (values e (cons value v)))
        (values start nil))))

(defun kleene+ (f string &key (start 0))
  "<rule>+ == <rule> <rule>*"
  (multiple-value-bind (end value) (funcall f string :start start)
    (if end
        (multiple-value-bind (e v) (kleene* f string :start end)
          (values e (cons value v)))
        nil)))

;; Construction macros
(defmacro grammar-string (str)
  `(when (starts-with string ,str :start start)
    (values (+ start ,(length str)) ,str)))

(defmacro grammar-and (first &rest rest)
  (if (null rest)
      first
      `(multiple-value-bind (end value) ,first
        (when end
          (let ((start end))
          (multiple-value-bind (e v) (grammar-and ,@rest)
            (when e
              (if (listp v)
                  (values e (cons value v))
                  (values e (list value v))))))))))


(defmacro grammar-or (first &rest rest)
  (if (null rest)
      first
      `(multiple-value-bind (end value) ,first
        (if end
            (values end value)
            (grammar-or ,@rest)))))


(defmacro grammar-* (x)
  (cond
    ((null x) nil)
    ((symbolp x) `(kleene* ',x string :start start))
    ((listp x)
     (let ((f (gensym)))
       `(let ((,f (lambda (string &key (start 0))
                   ,x)))
         (kleene* ,f string :start start))))
    (t (error (format nil "grammar-* cannot process ~S" x)))))

(defun parse-test (string &key (start 0))
  "match := 'a' 'b'"
  (grammar-and
   (grammar-string "a")
   (grammar-string "b")))

;; This is ambiguous; for now, 'a'* always matches...
(defun parse-test (string &key (start 0))
  "match := 'a'* | 'b'"
  (grammar-or
   (grammar-* (grammar-string "a"))
   (grammar-string "b")))

(defun parse-test (string &key (start 0))
  "match := 'a', 'b'"
  (grammar-and
   (grammar-string "a")
   (grammar-string "b")))

;; Simple grammar
(defun parse-token (string &key (start 0))
  "token := 'a' | 'b'"
  (grammar-or
   (grammar-string "a")
   (grammar-string "b")))

(defun parse-list (string &key (start 0))
  "list := '(', {token}, ')'"
  (grammar-and
   (grammar-string "(")
   (grammar-* parse-token)
   (grammar-string ")")))


;;;; Special conditions

;; Flag whether to ignore whitespace between tokens
(defparameter *grammar-no-white* nil)

;; List of restricted keywords
(defparameter *grammar-keywords* (make-hash-table :test #'equal))