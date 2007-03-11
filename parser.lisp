;; A simple top-down, backtracking parser
;; Modelled after EBNF notation

(defpackage "EBNF-PARSER"
    (:nicknames "EBNF" "PARSER")
    (:use "COMMON-LISP")
    (:export "GRAMMAR-CHAR"
             "GRAMMAR-CHARTABLE"
             "GRAMMAR-STRING"
             "GRAMMAR-OPTIONAL"
             "GRAMMAR-AND"
             "GRAMMAR-OR"
             "GRAMMAR-N"
             "GRAMMAR-*"
             "GRAMMAR-EXCEPTION"
             "GRAMMAR-FUNC"
             "GRAMMAR-RULE"
             "START"
             "STRING"))

(in-package "EBNF-PARSER")


;;; Internal utilities


(defmacro grammar-call (x)
  "Call function or macro x"
  (cond ((null x) (error "Cannot execute nil."))
        ((symbolp x) `(,x string :start start))
        ((listp x) x)
        (t (error "Cannot call ~S" x))))

(defmacro grammar-wrap (x)
  "Wrap function or macro x as a callback"
  (cond ((null x) (error "Cannot execute nil."))
        ((symbolp x) (list 'quote x))
        ((listp x) `(lambda (string &key (start 0)) ,x))
        (t (error "Cannot call ~S" x))))


;;; Parser construction


(defun starts-with (string prefix &key (start 0))
  "Does 'string' begin with 'prefix'?"
  (let ((end (+ start (length prefix)))
        (l (length string)))
    (unless (> end l)
      (string= prefix string :start2 start :end2 end))))

(defmacro grammar-char (c)
  "match = 'c'"
  `(when (and
          (< start (length string))
          (eq ,c (char string start)))
    (values (1+ start) ,(string c))))

(defmacro grammar-chartable (&rest ctable)
  "match = '(first ctable)' | '(second ctable)' | ..."
  `(when (< start (length string))
    (let ((c (char string start)))
      (when (find c ,(format nil "~{~C~}" ctable))
        (values (1+ start) (string c))))))

(defmacro grammar-string (str)
  "match = 'str'"
  (let ((l (length str)))
    (cond ((= l 0) '(values start ""))
          ((= l 1) `(when (and
                           (< start (length string))
                           (eq ,(char str 0) (char string start)))
                     (values (1+ start) ,str)))
          (t 
           `(when (starts-with string ,str :start start)
             (values (+ start ,(length str)) ,str))))))

(defmacro grammar-optional (x)
  "match = [x]"
  `(multiple-value-bind (end value) (grammar-call ,x)
    (if end
        (values end value)
        start)))

(defmacro grammar-and (first &rest rest)
  "match = first, (grammar-and rest)"
  (if (null rest)
      `(grammar-call ,first)
      `(multiple-value-bind (end value) (grammar-call ,first)
        (when end
          (let ((start end))
          (multiple-value-bind (e v) (grammar-and ,@rest)
            (when e
              (if (listp v)
                  (values e (cons value v))
                  (values e (list value v))))))))))

(defmacro grammar-or (first &rest rest)
  "match = first | (grammar-or rest)"
  (if (null rest)
      `(grammar-call ,first)
      `(multiple-value-bind (end value) (grammar-call ,first)
        (if end
            (values end value)
            (grammar-or ,@rest)))))

(defmacro grammar-n (n x)
  "match = n * x"
  (if (> n 0)
      (let ((n1 (1- n)))
      `(multiple-value-bind (end value) (grammar-call ,x)
        (when end
          (let ((start end))
            (multiple-value-bind (e v) (grammar-n ,n1 ,x)
              (when e
                (if (car v)
                    (values e (cons value v))
                    (values e (list value)))))))))
      'start))

(defun kleene* (f string &key (start 0))
  "match f 0 or more times"
  (multiple-value-bind (end value) (funcall f string :start start)
    (if end
        (multiple-value-bind (e v) (kleene* f string :start end)
          (values e (cons value v)))
        start)))

(defmacro grammar-* (x)
  "match = {x}"
  `(kleene* (grammar-wrap ,x) string :start start))

(defmacro grammar-exception (x y)
  "match = x - y"
  `(multiple-value-bind (end value) (grammar-call ,x)
    (when end
      (multiple-value-bind (e v) (grammar-call ,y)
        (declare (ignore v))
        (when (not e)
          (values end value))))))


;;; Output control


(defmacro grammar-func (x f)
  "Apply f to the value of x"
  `(multiple-value-bind (end value) (grammar-call ,x)
    (when end
      (values end (,f value)))))


;;; Helper macros


(defmacro grammar-rule (name &body body)
  "defun wrapper to simplify rule production"
  `(defun ,name (string &key (start 0))
    ,@body))
