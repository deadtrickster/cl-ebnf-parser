#| extensive redesign of the ebnf parser
- allow matching in strings, lists, and other source types (extensible)
- allow writing expressions using normal CL grammar but evaluating in a parse-specific way
- allow easily embedding custom parse forms (including plain CL)
- quote/quasiquote-style mechanism for nesting plain lisp functions
  (:cl (f (:parse rule)))
|#

;; (start end) = bounding index designators, e.g. (int int) or (int nil)
;; except returning end==nil indicates no match; so this might not be a good convention for parameters

;; parser compiler similar to "On Lisp" section 19.5
;; use generic functions to register actions

#| Parser calling convention

Given a form,

if an atom,
- try to expand symbol macros, take the values of constants, and re-dispatch
- if T, return match without advancing
- if NIL, return fail without advancing
- else try to match the literal (using (format nil "~S" f) if needed)

if a list,
- try dispatching to cpf-list
- if (car list) is :cl, then (:cl x) -> x
  and recursively change (:parse x) to (setf context (cpf x context))
  (
|#

;;;; PROTOCOL

(defgeneric cpf (form context env)
  (:documentation "Main entry point for expanding parse forms.
 CPF = compile parse form.
 Given a parse expression and a parsing context, return (values expansion expanded-p)."))

(defgeneric cpf-list (car form context env)
  (:documentation "CPF dispatches list forms to this function.
 The user should register new parse forms by creating the proper method specializations."))

(defmacro cpf-macro (form context &environment env)
  "delay macroexpansion"
  (cpf form context env))

(defclass context nil nil
  (:documentation "specialize this class for different types of input"))

(defclass string-context (context)
  ((string :initarg :string)
   (start :initarg :start)
   (end :initarg :end))
  (:documentation "specialization for parsing a string of text
 While END will often be (length string), making it an explicit parameter should help when the message is known to be shorter.
 For example, many formats encode the message size in a header field."))
#|
possible extensions/optimiztions
- check the length for several rules at once; don't need to check inside
- merge rules into a character-table lookup
- don't generate the value list when it will be discarded (e.g. in token strings)
- flag when start/values can be modified directly
|#

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

;; rather than guessing, use an explicit quasi-quote-style mechanism
#|
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
 (defun cpf-cl-key (form context &optional (key 'parse))
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
|#

#|
 (defun match-atom (seq start end atom)
  (declare (ignore end))
  (when (equal (elt seq start) atom)
    (values (1+ start) atom)))

 (defmethod cpf (form seq start end)
  "try to match the atom form in the seq"
  `(match-atom ,seq ,start ,end ,form))
|#

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

(defmethod cpf ((form string) context env)
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

(defmethod cpf ((form character) context env)
  (values
   (with-slots (string start end) context
     `(when (and (< ,start ,end) (char= ,form (char ,string ,start)))
        (values (1+ ,start) ,form)))
   t))

#|
 (defmethod cpf ((form list) seq start end)
  (let ((car (car form)))
    (if (eql car 'cl)
        (cpf-list (car form) form seq start end))))
|#
(defmethod cpf ((form list) context env)
  (cpf-list (car form) form context env))

(defmethod cpf ((form symbol) context env)
  "expand symbol macros and constants"
  ;; try to expand a symbol-macro
  (multiple-value-bind (exp exp-p) (macroexpand form env)
    (if exp-p
        (cpf exp context env)
        ;; not a symbol-macro, try to expand a constant
        (let* ((p (constantp form))
               (val (and p (symbol-value form))))
          ;; don't recurse on keywords and other self-evaluating forms!
          (if (and p (not (eql val form)))
            (cpf (symbol-value form) context env)
            ;; what to do here should depend on the context...
            (if (eql form t)
                ;; special-case: t always matches without consuming input
                (with-slots (string start end) context
                  `(values end nil))
                (error "do not know how to expand symbol ~A" form)))))))

(defmethod cpf ((form null) context env)
  "always fail without consuming input"
  nil)

#| invocation options
- generic functions
 (defgeneric name (context &optional args))
 (defmethod name ((context string) &optional start end))

- separate package for each context
 (defun string-parser:name (string start end))

- store implementations in *parse-rules* table
 (name) -> `(funcall ,(lookup-rule name) string start end)
or to allow changes to the rule,
 (name) -> `(funcall (car ,(lookup-rule name) string start end))
|#

(defmacro defrule (name &body parse-form &environment env)
  (assert (= (length parse-form) 1))
  `(defun ,name (string &optional (start 0) (end (length string)))
       ,(cpf (car parse-form) (make-instance 'string-context :string 'string :start 'start :end 'end) env)))

;; work on a protocol for defrule, allowing compile-time dispatch
(defvar *parse-rules* (make-hash-table)
  "hashtable of the rules that have been defined.  For each rule name, there is a property list recording possible expansions.  The key :source returns the original form.  Context indicators may return pre-compiled forms.")

(defun get-parse-rule (key context)
  "look up (or create) a parse rule for the given key and context"
  (let* ((props (gethash key *parse-rules*))
         (nonce (gensym))
         (result (getf props context nonce)))
    (assert props)
    (if (eql result nonce)
        (let ((source (getf props :source nonce)))
          (assert (not (eql source nonce)))
          (setf (getf props context) 
                
    
  )))))

;; store an alist (or plist) for each key
;; look up the context (the :form context returns the original source)
;; save pre-compiled forms in a cons; recompile all forms if the source changes
;; then the call site can (funcall (car lookup) args)


#|
 (defun cpf-cl-key (form context &optional (key 'parse))
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
|#

(defmethod cpf-list (car form context env)
  "expand unknown forms as parse calls"
  ;; issue a warning for some forms?
  ;; e.g. (:keyword ...)
  (with-slots (string start end) context
    (destructuring-bind (head &rest rest) form
      `(,head ,string ,start ,end ,@rest))))

(defmethod cpf-list ((car (eql :parse)) form context env)
  (error ":parse not in :cl - ~S" form))
          

(defun expand-nested-parse (form context &optional (key :parse))
  ;; don't pass env; it will be picked up by the call site...
  "destructively recurse through the form, expanding (:parse x) into a (cpf x) call site"
  (when (listp form)
    ;; in (x1 x2 x3), replace xi if it matches (key y)
    (mapl (lambda (x)
            (let ((c (car x)))
              (when (listp c)
                (if (eql (car c) key)
                    (progn
                      (unless (= (length c) 2)
                        (error "expected (:parse rule), got ~S" c))
                      (setf (car x) `(cpf-macro ,(cadr c) ,context)))
                    (expand-nested-parse c context key)))))
          form))
  form)

(defmethod cpf-list ((car (eql :cl)) form context env)
  "bind a cpf context and invoke any nested call sites"
  ;; should this bind a new context?  skip for now...
  (unless (= (length form) 2)
    (error "expected (:cl form), got ~S" form))
  (expand-nested-parse (cadr form) context))

(defrule test-cl
  (:cl (and (:parse "hi"))))

#| original nested method
 (defmethod cpf-list ((car (eql and)) form context)
  "Apply the parse forms in order, returning a list of their results or nil if any do not match."
  ;;"return nil if any term failed or (values last-end (list val1 ... valn)) if all passed"
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
                    (multiple-value-bind (,erest ,vrest) ,(cpf (cons and rest) new-context)
                      (when ,erest (values ,erest (cons ,v1 ,vrest)))))))
             `(multiple-value-bind (,e1 ,v1) ,(cpf term context)
                (when ,e1
                  (values ,e1 (cons ,v1 nil))))))
       t))))
|#

(defmacro pushback (obj tail)
  "push obj to the tail of the list, and update the tail"
  `(setf (cdr ,tail) (cons ,obj nil)
         ,tail (cdr ,tail)))

;; new flat method (less nesting is easier to read?)
(defmethod cpf-list ((car (eql 'and)) form context env)
  (unless (cdr form) ; style tweak, doesn't change runtime, mimics (cl:and)
    (return-from cpf-list (slot-value context 'start)))
  (with-slots (string start end) context
    (let* ((args (cdr form))
           (blocksym (gensym (symbol-name :and-)))
           (endsym (gensym (symbol-name :end-)))
           (valsym (gensym (symbol-name :val-)))
           (listsym (gensym (symbol-name :list-)))
           (tailsym (gensym (symbol-name :tail-)))
           (new-context
            (make-instance 'string-context
                           :string string
                           :start endsym
                           :end end))
           (inner
            `(let* ((,endsym ,start)
                    ,valsym
                    (,listsym (cons nil nil))
                    (,tailsym ,listsym))))
           (last (last inner 1)))
      (dolist (f args)
        (pushback
         `(setf (values ,endsym ,valsym) ,(cpf f new-context env))
         last)
        (pushback
         `(unless ,endsym
            (return-from ,blocksym))
         last)
        (pushback
         `(pushback ,valsym ,tailsym)
         last))
      (pushback `(values ,endsym (cdr ,listsym))
                last)
      `(block ,blocksym ,inner))))
      

(defrule test-and0 (and "hi" (and)))

(defrule test-and (and "hello" " " "world"))
;;(test-and "hello world")

(defrule test-composition (or (test-and) "no"))

#| original recursive implementation
 (defmethod cpf-list ((car (eql 'or)) form context env)
  "Apply the parse forms in order, returning the value of the first that matches or nil in none match."
  (with-slots (string start end) context
    (let ((term (cadr form))
          (rest (cddr form))
          (e1 (gensym (symbol-name :e1-)))
          (v1 (gensym (symbol-name :v1-))))
      (values
       (if rest
           `(multiple-value-bind (,e1 ,v1) ,(cpf term context env)
              (if ,e1
                  (values ,e1 ,v1)
                  ,(cpf (cons 'or rest) context env)))
          (cpf term context env))
       t))))
|#

;; new flat implementation
(defmethod cpf-list ((car (eql 'or)) form context env)
  (unless (cdr form) ; style tweak, doesn't change runtime, mimics (cl:or)
    (return-from cpf-list nil))
  (with-slots (string start end) context
    (let* ((args (cdr form))
           (blocksym (gensym (symbol-name :or-)))
           (endsym (gensym (symbol-name :end-)))
           (valsym (gensym (symbol-name :val-)))
           (inner
            `(let* (,endsym ,valsym)))
           (last (last inner 1)))
      (dolist (f args)
        (pushback
         `(setf (values ,endsym ,valsym) ,(cpf f context env))
         last)
        (pushback
         `(when ,endsym
            (return-from ,blocksym (values ,endsym ,valsym)))
         last))
      `(block ,blocksym ,inner))))

(defrule test-or0 (or (or) "hi"))

(defrule test-or
  (or "hello" "world"))
;; (test-or "hello")
;; (test-or "world")

(defrule test2
  (and (or "a" "b")
       (or "c" "d")))


(defmethod cpf-list ((car (eql 'repeat)) form context env)
  "(repeat min max form) where min is 0 or more and max is an integer or nil for unlimited"
  (with-slots (string start end) context
    (destructuring-bind (min max body) (cdr form)
      (let* ((c (gensym (symbol-name :count-)))
             (e (gensym (symbol-name :end-)))
             (le (gensym (symbol-name :last-end-)))
             (tmp (gensym (symbol-name :temp-)))
             (v (gensym (symbol-name :val-)))
             (new-context
              (make-instance 'string-context
                             :string string
                             :start e
                             :end end)))
        (values
         `(do ((,c 0)
               (,e ,start)
               (,le ,(if (= 0 min)
                         start
                         nil))
               (,tmp nil)
               (,v nil))
              (,(if max
                    `(or (not ,e)
                         (>= ,c ,max))
                    `(not ,e))
               (when (>= ,c ,min)
                 (values ,le (reverse ,v))))
            (setf (values ,e ,tmp) ,(cpf body new-context env))
            (when ,e
              (incf ,c)
              (setf ,le ,e)
              (push ,tmp ,v)))
         t)))))

(defrule test-repeat
  (repeat 1 nil #\a))

(defrule test-repeat
  (and (repeat 0 1 #\a)
       (repeat 1 nil #\b)))    


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
