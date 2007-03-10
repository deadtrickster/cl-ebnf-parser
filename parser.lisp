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
(defun match-n (n f string &key (start 0))
  (if (> n 0)
      (multiple-value-bind (end value) (funcall f string :start start)
        (when end
          (multiple-value-bind (e v) (match-n (1- n) f string :start end)
            (when e
              (if (car v)
                  (values e (cons value v))
                  (values e (list value)))))))
      start))

(defun kleene* (f string &key (start 0))
  (multiple-value-bind (end value) (funcall f string :start start)
    (if end
        (multiple-value-bind (e v) (kleene* f string :start end)
          (values e (cons value v)))
        start)))

(defun kleene+ (f string &key (start 0))
  "<rule>+ == <rule> <rule>*"
  (multiple-value-bind (end value) (funcall f string :start start)
    (if end
        (multiple-value-bind (e v) (kleene* f string :start end)
          (values e (cons value v)))
        nil)))

;;
;; Construction macros
;;

;; Provide a clean interface to rules expressed as either function names or macros
(defmacro grammar-call (x)
  (cond ((null x) (error "Cannot execute nil."))
        ((symbolp x) `(,x string :start start))
        ((listp x) x)
        (t (error "Cannot call ~S" x))))

(defmacro grammar-wrap (x)
  (cond ((null x) (error "Cannot execute nil."))
        ((symbolp x) (list 'quote x))
        ((listp x) `(lambda (string &key (start 0)) ,x))
        (t (error "Cannot call ~S" x))))

(defmacro grammar-string (str)
  (let ((l (length str)))
    (cond ((= l 0) '(values start ""))
          ((= l 1) `(when (eq ,(char str 0) (char string start))
                     (values (1+ start) ,str)))
          (t 
           `(when (starts-with string ,str :start start)
             (values (+ start ,(length str)) ,str))))))

(defmacro grammar-optional (x)
  `(multiple-value-bind (end value) (grammar-call ,x)
    (if end
        (values end value)
        start)))

(defmacro grammar-and (first &rest rest)
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
  (if (null rest)
      `(grammar-call ,first)
      `(multiple-value-bind (end value) (grammar-call ,first)
        (if end
            (values end value)
            (grammar-or ,@rest)))))

(defmacro grammar-n* (n x)
  `(match-n ,n (grammar-wrap ,x) string :start start))

(defmacro grammar-* (x)
  `(kleene* (grammar-wrap ,x) string :start start))

(defmacro grammar-exception (x y)
  "a syntactic-exception; x but not also y"
  `(multiple-value-bind (end value) (grammar-call ,x)
    (when end
      (multiple-value-bind (e v) (grammar-call ,y)
        (declare (ignore v))
        (when (not e)
          (values end value))))))

;;
;; Value-changing mechanism
;;
(defmacro grammar-func (x f)
  "Apply f to the value of x"
  `(multiple-value-bind (end value) (grammar-call ,x)
    (when end
      (values end (,f value)))))


(defmacro grammar-rule (name &rest body)
  `(defun ,name (string &key (start 0))
    ,@body))


;;;; Special conditions

;; Flag whether to ignore whitespace between tokens
;(defparameter *grammar-no-white* nil)

;; List of restricted keywords
;(defparameter *grammar-keywords* (make-hash-table :test #'equal))