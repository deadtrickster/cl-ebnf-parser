;; translation of Boost's cpp.re

(defconstant slash-t #\Tab "C escape: \t; (code-char 9)")
(defconstant slash-n #\Linefeed "C escape: \n; (code-char 10)")
(defconstant slash-v (code-char 11) "C escape: \v; (code-char 11)")
(defconstant slash-f #\Page "C escape: \f; (code-char 12)")
(defconstant slash-r #\Return "C escape: \r; (code-char 13)")

(defparameter *c++-mode* nil "indicate that we are parsing C++ code")

(defun ascii-range (string start end char0 char1)
  (when (< start end)
    (let ((c (char string start)))
      (when (<= (char-code char0) (char-code c) (char-code char1))
        (values (1+ start) c)))))

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

(defmacro when-match (name rule &body body)
  `(let ((,name ,rule))
     (when ,name
       (values ,name
               ,@body))))

;; these would need to be cpf expansion rules to be truly useful...
(defmacro with-match (context context-symbols rule &body body)
  (destructuring-bind (stringsym startsym aftersym endsym) context-symbols
    (with-slots (string start end) context
      `(let ((,aftersym (cpf-macro ,rule ,context)))
         (when ,aftersym
           (let ((,stringsym ,string)
                 (,startsym ,start)
                 (,endsym ,end))
             ,@body))))))

;; match-filter isn't very useful as it stands
;; it might be usefule as a cpf-list method
(defmacro match-filter (context context-symbols rule &body body)
  (destructuring-bind (stringsym startsym aftersym endsym) context-symbols
    (declare (ignore stringsym startsym endsym))
    `(with-match ,context ,context-symbols ,rule
       (values ,aftersym ,@body))))


;; 2.3
(defrule hex-quad
  (and (hexadecimal-digit)
       (hexadecimal-digit)
       (hexadecimal-digit)
       (hexadecimal-digit)))

(defrule universal-character-name
  (or (and "\u" (hex-quad))
      (and "\U" (hex-quad) (hex-quad))))


;; 2.10, lex.ppnumber

;; left recursion...
(defrule pp-number
  ;; original, left recursion
  #|
  (or> (digit)
       (and #\. (digit))
       (and (pp-number) (digit))
       (and (pp-number) (identifier-nondigit))
       (and (pp-number) #\e (sign))
       (and (pp-number) #\E (sign))
       (and (pp-number) #\.))
  |#
  ;; equivalent
  (and
   (or (and (repeat 0 nil (digit)) #\. (repeat 1 nil (digit)))
       (and (repeat 1 nil (digit)) #\. (repeat 0 nil (digit)))
       (repeat 1 nil (digit)))
   (optional
    (and (or #\e #\E)
         (sign)
         (repeat 0 nil (digit))))))


;; explore solution to left recursion
;; simulate (or (and (pp-number) (digit)) (digit))
(defparameter *pp-start* (cons -1 nil))
(defrule pp-number
  (:cl
   (progn
     ;;(print (list start *pp-start*))
     (cond
       ;; based on Paull's algorithm
       ;; two cases of sub-recursion
       ;; http://en.wikipedia.org/wiki/Left_recursion#Accommodating_left_recursion_in_top-down_parsing
       ;; given A -> A a1 | ... | A an | b1 | ... | bn
       ;; substitute production

       ;; A -> b1 A' | ... | bm A'
       ;; (i.e. A always fails; then if anything passes, try the rule again)
       ((eql (cdr *pp-start*) :fail)
        nil)

       ;; A' = eps | a1 A' | ... | an A'
       ;; (i.e. A always returns a bogus value)
       ((eql (cdr *pp-start*) :pass)
        (values start nil))

       ((and (= (car *pp-start*) start)
             (cdr *pp-start*))
        (values-list (cdr *pp-start*)))

       ;; detect left-recursion and invoke the two rules
       ((= (car *pp-start*) start) ; left-recursion first detected
        (let (e1 v1 e2 v2)
          ;; find one of the b's
          (let ((*pp-start* (cons start :fail)))
            (setf (values e1 v1)
                  (:parse (or (and (pp-number) (digit)) (digit))))
            (print :v1))
          (unless e1
            (return-from pp-number nil))

          #|
          ;; try to find one of the a's
          (setf start e1) ; using a leaky detail...
          (let ((*pp-start* (cons start nil)))
            (setf (values e2 v2)
                  (:parse (or (and (pp-number) (digit)) (digit)))))
          (setf v2 (list v1 v2))
          |#

          #| alternative, only recurses a couple times...
          |#
          (setf e2 t)
          (do ()
              ((not e2))
            (let ((*pp-start* (cons start (list e1 v1))))
              (setf (values e2 v2)
                    (:parse (or (and (pp-number) (digit)) (digit)))))
            (when e2
              (setf e1 e2
                    start e1
                    ;;v1 (cons v1 v2)
                    )))
            

          ;(print *pp-start*)
          (format t "~A~%" (list :v1 e1 v1 :v2 e2 v2))
          #|
          (if e2
              (values e2 v2)
              (values e1 (cons v1 nil)))))
          |#
          (values e1 v1)))
       (t
        (let ((*pp-start* (cons start nil)))
          (:parse (or (and (pp-number) (digit)) (digit)))))))))

#|
 (defparameter *pp-number* nil)
 (defun pp-number (string &OPTIONAL (START 0) (END (LENGTH STRING)))
  (cond
    ((equal (car *pp-number*) :recur)
     nil)
    ((find start *pp-number*)
     ;; left recursion detected
     (let ((*pp-number* (cons :recur *pp-number*)))
  (if (find start *pp-number*)
      ;; left recursion detected
      (progn
        ;; run, but fail on any further recursions
        )
      ;; normal operation
      (let ((*pp-number* (cons start *pp-number*)))
        
      ))
|#

;; 2.11
(defrule nondigit
  (or (ascii-range #\a #\z)
      (ascii-range #\A #\Z)
      #\_))

(defrule digit
  (ascii-range #\0 #\9))

(defrule identifier-nondigit
  (or (nondigit)
      (universal-character-name)
      ;; other implementation-defined characters
      ))

;; left recursion...
(defrule identifier
  (or> (identifier-nondigit)
       (and (identifier) (identifier-nondigit))
       (and (identifier) (digit))))

;; 2.12 -- see bottom of file

;; 2.14
(defrule nonzero-digit (ascii-range #\1 #\9))

(defrule octal-digit (ascii-range #\0 #\7))

(defrule hexadecimal-digit
  (or
   (ascii-range #\0 #\9)
   (ascii-range #\a #\f)
   (ascii-range #\A #\F)))

(defrule decimal-literal
  (or (nonzero-digit)
      (and (decimal-literal) (digit))))

(defrule octal-literal
  (or #\0
      (and (octal-literal) (octal-digit))))

(defrule hexadecimal-literal
  (or (and "0x" (hexadecimal-digit))
      (and "0X" (hexadecimal-digit))
      (and (hexadecimal-literal) (hexadecimal-digit))))

(defrule integer-literal
  (or (and (decimal-literal) (optional (integer-suffix)))
      (and (octal-literal) (optional (integer-suffix)))
      (and (hexadecimal-literal) (optional (integer-suffix)))))

;; 2.14.3, lex.ccon

(defrule simple-escape-sequence
  (and #\\ (or #\' #\" #\? #\\
               #\a #\b #\f #\n #\r #\t #\v)))

(defrule octal-escape-sequence
  (and #\\ (repeat 1 3 (octal-digit))))

(defrule hexadecimal-escape-sequence
  (and #\\ #\x (repeat 1 nil (hexadecimal-digit))))

(defrule escape-sequence
  (or (simple-escape-sequence)
      (octal-escape-sequence)
      (hexadecimal-escape-sequence)))

(defun any-char (string start end)
  (when (< start end)
    (values (1+ start) (char string start))))

(defrule c-char
  (or (exception (any-char) (or #\' #\\ slash-n))
      (escape-sequence)
      (universal-character-name)))

(defrule c-char-sequence
  (repeat 1 nil (c-char)))

(defrule character-literal
  (or (and #\' (c-char-sequence) #\')
      (and "u'" (c-char-sequence) #\')
      (and "U'" (c-char-sequence) #\')
      (and "L'" (c-char-sequence) #\')))

;; 2.14.4, lex.fcon
(defrule sign
  (or #\+ #\-))

(defrule digit-sequence
  (repeat 1 nil (digit)))

(defrule floating-suffix
  (or #\f #\l #\F #\L))

(defrule exponent-part
  (and (or #\e #\E) (optional (sign)) (digit-sequence)))

(defrule fractional-constant
  (or (and (optional (digit-sequence)) #\. (digit-sequence))
      (and (digit-sequence) #\.)))

(defrule floating-literal
  (or (and (fractional-constant) (optional (exponent-part)) (optional (floating-suffix)))
      (and (digit-sequence) (exponent-part) (optional (floating-suffix)))))


;; --------

;; Integer            = (("0" [xX] HexDigit+) | ("0" OctalDigit*) | ([1-9] Digit*));
(defrule r-integer
  (or
   (:cl
    (when-match match (:parse (and #\0 (or #\x #\X) (repeat 1 nil (hex-digit))))
      (parse-integer (subseq string (+ start 2) match) :radix 16)))
   #|
   ;; alternate implementation of previous clause
   (:cl
    (match-filter (:context) (string start after end)
        (and #\0 (or #\x #\X) (repeat 1 nil (hex-digit)))
      (parse-integer (subseq string (+ start 2) after) :radix 16)))
   |#
   (:cl
    (when-match match (:parse (and #\0 (repeat 0 nil (octal-digit))))
      (parse-integer (subseq string (+ start 1) match) :radix 8)))
   (:cl
    (when-match match (:parse (and (ascii-range #\1 #\9) (repeat 0 nil (digit))))
      (parse-integer (subseq string start match) :radix 10)))))

;; ExponentStart      = [Ee] [+-];
(defrule exponent-start
  (and (or #\E #\e) (or #\+ #\-)))

;; ExponentPart       = [Ee] [+-]? Digit+;
(defrule exponent-part
  (and (or #\E #\e)
       (repeat 0 1 (or #\+ #\-))
       (repeat 1 nil (digit))))

;; FractionalConstant = (Digit* "." Digit+) | (Digit+ ".");
(defrule fractional-constant
  (or (and (repeat 0 nil (digit)) #\. (repeat 1 nil (digit)))
      (and (repeat 1 nil (digit)) #\.)))

;; FloatingSuffix     = [fF] [lL]? | [lL] [fF]?;
(defrule floating-suffix
  (or (and (or #\f #\F) (repeat 0 1 (or #\l #\L)))
      (and (or #\l #\L) (repeat 0 1 (or #\f #\F)))))

;; IntegerSuffix      = [uU] [lL]? | [lL] [uU]?;
(defrule integer-suffix
  (or (and (or #\u #\U) (repeat 0 1 (or #\l #\L)))
      (and (or #\l #\L) (repeat 0 1 (or #\u #\U)))))

;; LongIntegerSuffix  = [uU] ([lL] [lL]) | ([lL] [lL]) [uU]?;
(defrule long-integer-suffix
  (or (and (or #\u #\U) 
  
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
  (:cl (when *c++-mode*
         (:parse
          (and "//"
               (repeat 0 * (exception any (or slash-v slash-f slash-n)))
               (repeat 0 * (or slash-v slash-f (exception whitespace slash-n)))
               slash-n)))))



;; 2.12
;; these apply to full tokens...
(defrule keywords
  (or "alignas"
      "alignof"
      "asm"
      "auto"
      "bool"
      "break"
      "case"
      "catch"
      "char"
      "char16_t"
      "char32_t"
      "class"
      "const"
      "constexpr"
      "const_cast"
      "continue"
      "decltype"
      "default"
      "delete"
      "do"
      "double"
      "dynamic_cast"
      "else"
      "enum"
      "explicit"
      "export"
      "extern"
      "false"
      "float"
      "for"
      "friend"
      "goto"
      "if"
      "inline"
      "int"
      "long"
      "mutable"
      "namespace"
      "new"
      "noexcept"
      "nullptr"
      "operator"
      "private"
      "protected"
      "public"
      "register"
      "reinterpret_cast"
      "return"
      "short"
      "signed"
      "sizeof"
      "static"
      "static_assert"
      "static_cast"
      "struct"
      "switch"
      "template"
      "this"
      "thread_local"
      "throw"
      "true"
      "try"
      "typedef"
      "typeid"
      "typename"
      "union"
      "unsigned"
      "using"
      "virtual"
      "void"
      "volatile"
      "wchar_t"
      "while"))

(defrule alternative-representations
  (or "and"
      "and_eq"
      "bitand"
      "bitor"
      "compl"
      "not"
      "not_eq"
      "or"
      "or_eq"
      "xor"
      "xor_eq"))