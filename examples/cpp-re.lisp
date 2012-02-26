#|
parser protocol
args: (sequence start end)
return: nil or (next-start value)

transform protocol similar to Boost::Spirit
see http://www.boost.org/doc/libs/1_48_0/libs/spirit/doc/html/spirit/qi/reference/action.html
args: (sequence start next-start end value)
return: nil or (next-start value)
note that the fancy return-value isn't necessary; just wrap the parser...

old protocol had start
|#

#|
definition: A ``parse-form'' is a shorthand notation for specifying parsers; it leaves out the (seq start end) parameters used to pass stream context.

the parse-form (x y z) is expanded to (x seq start end y z)
a parse-form atom is taken to be a literal token
|#
(defmacro parse-form (form seq start end)
  "expand the parse-form f"

 )

(defmacro grammar-call (form seq start end)
  "how to invoke a form in a grammar rule"
  (cond
    ;; an empty list never matches
    ((null form) nil)
    ;; t always matches, without advancing the stream or producing a value
    ((eql form t) (list 'values end))
    ;; match some common literals
    ((stringp form) (list 'match-string seq start end))
    ((characterp form) (list 'match-char seq start end))
    ;; if form is a list
    ))

(defmacro transform (seq start end parse-form modifier-form)
  `(multiple-value-bind (e v)
       ,parse-form
     (when e
       (values e ,(push v (cdr modifier-form))))))

;; parser compiler similar to "On Lisp" section 19.5
;; use generic functions to register actions

(defgeneric cpf (form context)
  (:documentation "CPF = compile parse form.  Given a parse expression and a parsing context, return (values expansion expanded-p)."))

(defun cpf-cl-all (form context)
  "recurse through a normal CL source form, processing sublists as possible parse forms"
  (if (listp form)
    (let ((modified nil))
      (values 
       (loop for x in form
          collecting
            (if (listp x)
                (multiple-value-bind (exp exp-p) (cpf x context)
                  (when exp-p
                    (setf modified t))
                  exp)
                x))
       modified))
    form))

(define-condition warn-not-parse-function (style-warning)
  ((form :initarg :form :reader warn-not-parse-function-form))
  (:report
   (lambda (c s)
     (format s "form was not a parse form: ~A" (warn-not-parse-function-form c)))))

;; needs a way to control context chaining when recursing through normal expressions
(defun cfp-cl-key (form context &optional (key 'parse))
  "recurse through a normal CL form, processing sublists that begin with KEY as parse forms"
  (if (listp form)
      (let ((modified nil))
        (values
         (loop for x in form
            collecting
              (if (and (listp x) (eql (car x) key))
                  (multiple-value-bind (exp exp-p) (cpf (cdr x) context)
                    (if exp-p
                        (setf modified t)
                        (warn 'warn-not-parse-function :form exp))
                    exp)
                  x))
         modified))
      form))
                        

(defgeneric cpf-list (car form context)
  (:documentation "CPF can dispatch list forms to this function."))

(defclass context nil nil)

(defclass string-context (context)
  (string start end))

(defclass list-context (context)
  (top here end))

(defun match-atom (seq start end atom)
  (declare (ignore end))
  (when (equal (elt seq start) atom)
    (values (1+ start) atom)))

(defmethod cpf (form seq start end)
  "try to match the atom form in the seq"
  `(match-atom ,seq ,start ,end ,form))

(defmethod cpf-list (car form seq start end)
  ;; default method: don't modify the form...
  form)

(defun starts-with (string start end prefix)
  "Does 'string' begin with 'prefix'?"
  (let ((stop (+ start (length prefix))))
    (when (and (< stop (or end (length string)))
               (string= prefix string :start2 start :end2 stop))
      (values stop prefix))))

(defmethod cpf ((form string) seq start end)
  `(typecase ,seq
     (string (starts-with ,seq ,start ,end ,form))
     (t (match-atom ,seq ,start ,end ,form))))
  

(defmethod cpf ((form list) seq start end)
  (cpf-list (car form) form seq start end))

(defmethod cpf-list ((car (eql 'and)) form seq start end)
  "return nil if any term failed or (values last-end (list val1 ... valn)) if all passed"
  
  (print 43))

(defmacro defrule (name &body body)
  `(progn
     (defun ,name (seq start end)
       (cpf ,body seq start end))
     (demethod cpf-list ((car (eql ',name)) seq start end)
             (cpf ,body seq start end)))


;; translation of Boost's cpp.re

(defconstant slash-t #\Tab "C escape: \t; (code-char 9)")
(defconstant slash-n #\Linefeed "C escape: \n; (code-char 10)")
(defconstant slash-v (code-char 11) "C escape: \v; (code-char 11)")
(defconstant slash-f #\Page "C escape: \f; (code-char 12)")
(defconstant slash-r #\Return "C escape: \r; (code-char 13)")

(defparameter *c++-mode* nil "indicate that we are parsing C++ code")


;; any                = [\t\v\f\r\n\040-\377];
(defrule any
  (or
   slash-t
   slash-v
   slash-f
   slash-r
   slash-n
   (ascii-range 32 255)))

;; anyctrl            = [\001-\037];
(defrule anyctrl
    (ascii-range 1 31))

;; OctalDigit         = [0-7];
(defrule octal-digit
    (ascii-range #\0 #\7))

;; Digit              = [0-9];
(defrule digit
    (ascii-range #\0 #\9))

;; HexDigit           = [a-fA-F0-9];
(defrule hex-digit
    (or
     (ascii-range #\a #\f)
     (ascii-range #\A #\F)
     (ascii-range #\0 #\9)))

;; Integer            = (("0" [xX] HexDigit+) | ("0" OctalDigit*) | ([1-9] Digit*));
(defrule integer
    (or
     (and #\0 (or #\x #\X) (range 1 * hex-digit))
     (and #\0 (range 0 * octal-digit))
     (and (ascii-range #\1 #\9) (range 0 * digit))))

;; ExponentStart      = [Ee] [+-];
(defrule exponent-start
    (and (or #\E #\e) (or #\+ #\-)))

;; ExponentPart       = [Ee] [+-]? Digit+;
(defrule exponent-part
    (and (or #\E #\e)
         (range 0 1 (or #\+ #\-))
         (range 1 * digit)))

;; FractionalConstant = (Digit* "." Digit+) | (Digit+ ".");
(defrule fractional-constant
    (or (and (range 0 * digit) #\. (range 1 * digit))
        (and (range 1 * digit) #\.)))

;; FloatingSuffix     = [fF] [lL]? | [lL] [fF]?;
(defrule floating-suffix
    (or (and (or #\f #\F) (range 0 1 (or #\l #\L)))
        (and (or #\l #\L) (range 0 1 (or #\f #\F)))))

;; IntegerSuffix      = [uU] [lL]? | [lL] [uU]?;
(defrule integer-suffix
    (or (and (or #\u #\U) (range 0 1 (or #\l #\L)))
        (and (or #\l #\L) (range 0 1 (or #\u #\U)))))

;; LongIntegerSuffix  = [uU] ([lL] [lL]) | ([lL] [lL]) [uU]?;
(defrule long-integer-suffix
Backslash          = [\\] | "??/";
EscapeSequence     = Backslash ([abfnrtv?'"] | Backslash | "x" HexDigit+ | OctalDigit OctalDigit? OctalDigit?);
HexQuad            = HexDigit HexDigit HexDigit HexDigit;
UniversalChar      = Backslash ("u" HexQuad | "U" HexQuad HexQuad);
Newline            = "\r\n" | "\n" | "\r";
PPSpace            = ([ \t\f\v]|("/*"(any\[*]|Newline|("*"+(any\[*/]|Newline)))*"*"+"/"))*;
Pound              = "#" | "??=" | "%:";
NonDigit           = [a-zA-Z_$] | UniversalChar;


(defrule newline
    (or
     (and #\Return #\Linefeed)
     #\Linefeed
     #\Return))

;; 2.7
(defrule token
    (or identifier
        keyword
        literal
        operator
        punctuator))

;; 2.8
(defrule c-comment
    (and "/*" (match-until "*/")))

;; 2.8
(defrule c++-comment
    (cl:and *c++-mode*
            (and "//"
                 (repeat 0 * (exception any (or slash-v slash-f slash-n)))
                 (repeat 0 * (or slash-v slash-f (exception whitespace slash-n)))
                 slash-n)))

(defun string-value (x)
  "parser transform, return the matched string instead of its pieces"
  (declare (ignore x))
  (subseq string start end))

(defun string-matcher (s)
  "return a grammar lambda that matches the string s"
  (let ((l (length s)))
    (lambda (seq start end)
      ;; dispatch on type of seq? For example, also match '(s) or #(s) ?
      (let ((stop (+ start l)))
        (when (and (< stop (or end (length seq)))
                   (string= s seq :start2 start :end2 stop))
          (values stop s))))))
           

;; if body is a single form, execute it
;; if body has two forms, the second is a filter for the first
(defmacro defrule (name &body body)
  `(defun ,name (seq start end)
     ,@
     ))

;; 2.9
(defrule header-name
    (multiple-value-bind (e v)
        (or (and #\< h-char-sequence #\>)
            (and #\" q-char-sequence #\"))
      (when e
        (values e (list :include (second v))))))

(defrule h-char-sequence
    (repeat 1 * h-char)
  #'string-value)

(defrule h-char
    (exception any (or slash-n #\>)))

(defrule q-char-sequence
    (repeat 1 * q-char)
  #'string-value)

(defrule q-char
    (exception any (or slash-n #\")))

#|
([ \t\f\v]|
 ("/*"
  (any\[*]|
  Newline|
  ("*"+(any\[*/]|Newline)))*
 "*"+"/"))*
|#
(defrule ppspace
(repeat *
 (or
  (chartable #\Space #\Tab #\Page vtab)
  (and
   "/*"
   (repeat *
    (or
     (exception any #\*)
     newline
     (and
      (one-or-more "*")
      (or
       (exception
        any (or #\* #\/))
       #\Linefeed))))
   (one-or-more "*")
   "/")))