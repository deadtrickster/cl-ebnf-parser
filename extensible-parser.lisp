#| extensive redesign of the ebnf parser
- allow matching in strings, lists, and other source types (extensible)
- allow writing expressions using normal CL grammar but evaluating in a parse-specific way
- allow easily embedding custom parse forms (including plain CL)
|#

;; parser compiler similar to "On Lisp" section 19.5
;; use generic functions to register actions

;;;; PROTOCOL

(defgeneric cpf (form context)
  (:documentation "Main entry point for expanding parse forms.
 CPF = compile parse form.
 Given a parse expression and a parsing context, return (values expansion expanded-p)."))

(defgeneric cpf-list (car form context)
  (:documentation "CPF dispatches list forms to this function.
 The user should register new parse forms by creating the proper method specializations."))

(defclass context nil nil
  (:documentation "specialize this class for different types of input"))

(defclass string-context (context)
  ((string :initarg :string)
   (start :initarg :start)
   (end :initarg :end))
  (:documentation "specialization for parsing a string of text
 While END will often be (length string), making it an explicit parameter should help when the message is known to be shorter.
 For example, many formats encode the message size in a header field."))

(defclass list-context (context)
  (top here end)
  (:documentation "specialization for parsing a list of tokens"))

(defclass array-context (context)
  ((array :initarg :array :reader array-context-array)
   (start :initarg :start :reader array-context-start)
   (end :initarg :end :reader array-context-end))
  (:documentation "specialization for parsing an array of tokens"))

;; print a warning when a form is not recognized
;; in (parse form), form may be incorrect; mark as (parse (cl form)) if intentional
(define-condition warn-parse-form-not-recognized (style-warning)
  ((form :initarg :form :reader warn-pfnr-form))
  (:report
   (lambda (c s)
     (format s "parse form was not recognized: ~A"
             (warn-pfnr-form c)))))

;;;; STRING IMPLEMENTATION

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
                        (warn 'warn-pfnr-form :form exp))
                    exp)
                  x))
         modified))
      form))
                        

(defun match-atom (seq start end atom)
  (declare (ignore end))
  (when (equal (elt seq start) atom)
    (values (1+ start) atom)))

(defmethod cpf (form seq start end)
  "try to match the atom form in the seq"
  `(match-atom ,seq ,start ,end ,form))

#|
(defmethod cpf-list (car form seq start end)
  ;; default method: don't modify the form...
  form)
|#

(defun starts-with (string start end prefix)
  "Does 'string' begin with 'prefix'?"
  (let ((stop (+ start (length prefix))))
    (when (and (<= stop (or end (length string)))
               (string= prefix string :start2 start :end2 stop))
      (values stop prefix))))

(defmethod cpf ((form string) context)
  (values
   (typecase context
     (string-context
      (with-slots (string start end) context
        `(starts-with ,string ,start ,end ,form)))
     (array-context
      (with-slots (array start) context
        `(when (string= (aref ,array ,start) ,form)
           (values ,form (`+ ,start))))))
   t))

#|
(defmethod cpf ((form list) seq start end)
  (let ((car (car form)))
    (if (eql car 'cl)
        (cpf-list (car form) form seq start end))))
|#
(defmethod cpf ((form list) context)
  (cpf-list (car form) form context))

(defmacro defrule (name &body parse-form)
  (assert (= (length parse-form) 1))
  `(defun ,name (string)
     (let ((start 0)
           (end (length string)))
       ,(cpf (car parse-form) (make-instance 'string-context :string 'string :start 'start :end 'end)))))

(defmethod cpf-list ((car (eql 'cl)) context)
  "return nil if any term failed or (values last-end (list val1 ... valn)) if all passed"
  )

(defmethod cpf-list ((car (eql 'and)) form context)
  "Apply the parse forms in order, returning a list of their results or nil if any do not match."
  (let ((term (cadr form))
        (rest (cddr form)))
    (with-slots (string start end) context
      (values
       (let ((e1 (gensym (symbol-name :e1-)))
             (v1 (gensym (symbol-name :v1-))))
         (if rest
             (let ((new-context
                    (make-instance 'string-context
                                   :string string
                                   :start e1
                                   :end end))
                   (erest (gensym (symbol-name :erest-)))
                   (vrest (gensym (symbol-name :vrest-))))
               `(multiple-value-bind (,e1 ,v1) ,(cpf term context)
                  (when ,e1
                    (multiple-value-bind (,erest ,vrest) ,(cpf (cons 'and rest) new-context)
                      (when ,erest (values ,erest (cons ,v1 ,vrest)))))))
             `(multiple-value-bind (,e1 ,v1) ,(cpf term context)
                (when ,e1
                  (values ,e1 (cons ,v1 nil))))))
       t))))

(defrule test-and (and "hello" " " "world"))
;;(test-and "hello world")


(defmethod cpf-list ((car (eql 'or)) form context)
  "Apply the parse forms in order, returning the value of the first that matches or nil in none match."
  (with-slots (string start end) context
    (let ((term (cadr form))
          (rest (cddr form))
          (e1 (gensym (symbol-name :e1-)))
          (v1 (gensym (symbol-name :v1-))))
      (values
       (if rest
           `(multiple-value-bind (,e1 ,v1) ,(cpf term context)
              (if ,e1
                  (values ,e1 ,v1)
                  ,(cpf (cons 'or rest) context)))
          (cpf term context))
       t))))

(defrule test-or
  (or "hello" "world"))
;; (test-or "hello")
;; (test-or "world")

(defrule test2
  (and (or "a" "b")
       (or "c" "d")))
    
    
#|
(defmacro defrule (name &body body)
  `(progn
     (defun ,name (seq start end)
       (cpf ,body seq start end))
     (demethod cpf-list ((car (eql ',name)) seq start end)
             (cpf ,body seq start end)))
|#

;;;; ARRAY IMPLEMENTATION

(defmethod cfp ((form string) (context array-context))
  "match a string in the array"
  (with-slots (array start) context
    (values
     `(when (string= ,form (aref ,array ,start))
        (values ,form (1+ ,start)))
     t)))