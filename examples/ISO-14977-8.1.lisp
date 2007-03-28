;; Goal:  Provide a frontend that auto-generates the parser from raw ISO 14977 EBNF text.
;; e.g. "rule = (* doc *) body ;" => `(defun ,rule (...) ",doc" ,body)

(defpackage "ISO-14977-8.1"
  (:nicknames "ISO14977")
  (:documentation "EBNF grammar, following ISO 14977 section 8.1")
  (:use "COMMON-LISP" "EBNF-PARSER")
  (:export "SYNTAX-PRINTING"
           "SYNTAX-UNCOMMENTED"
           "SYNTAX-ABSTRACT"
           "SYNTAX")
  )

(in-package "ISO-14977-8.1")

;; Cheat a bit; use built-in Lisp functions for a couple rules (for efficiency)
(grammar-rule letter
  (when (< start (length string))
    (let ((c (char string start)))
      (when (alpha-char-p c)
        (values (1+ start) (string c))))))

(grammar-rule decimal-digit
  (when (< start (length string))
    (let ((c (char string start)))
      (when (digit-char-p c)
        (values (1+ start) (string c))))))

(grammar-rule concatenate-symbol
  (grammar-string ","))

(grammar-rule defining-symbol
  (grammar-string "="))

(grammar-rule definition-separator-symbol
  (grammar-or (grammar-string "|")
              (grammar-string "/")
              (grammar-string "!")))

(grammar-rule end-comment-symbol
  (grammar-string "*)"))

(grammar-rule end-group-symbol
  (grammar-string ")"))

(grammar-rule end-option-symbol
  (grammar-or (grammar-string "]")
              (grammar-string "/)")))

(grammar-rule end-repeat-symbol
  (grammar-or (grammar-string "}")
              (grammar-string ":)")))

(grammar-rule except-symbol
  (grammar-char #\-))

(grammar-rule first-quote-symbol
  (grammar-char #\'))

(grammar-rule repetition-symbol
  (grammar-string "*"))

(grammar-rule second-quote-symbol
  (grammar-char #\"))

(grammar-rule special-sequence-symbol
  (grammar-string "?"))

(grammar-rule start-comment-symbol
  (grammar-string "(*"))

(grammar-rule start-group-symbol
  (grammar-string "("))

(grammar-rule start-option-symbol
  (grammar-or (grammar-string "[")
              (grammar-string "(/")))

(grammar-rule start-repeat-symbol
  (grammar-or (grammar-string "{")
              (grammar-string "(:")))

(grammar-rule terminator-symbol
  (grammar-or (grammar-string ";")
              (grammar-string ".")))

(grammar-rule other-character
  (grammar-chartable #\Space #\: #\+ #\_ #\% #\@
                     #\& #\# #\$ #\< #\> #\\
                     #\^ #\` #\~))

(grammar-rule horizontal-tabulation-character
  (grammar-char #\Tab))

(grammar-rule new-line
  (grammar-char #\Newline)) ; Slightly nonstandard

;; Skipping a couple other whitespace rules...

;;;
;;; Removal of unnecessary whitespace.
;;;

(grammar-rule terminal-character
  (grammar-or
   letter
   decimal-digit
   concatenate-symbol
   defining-symbol
   definition-separator-symbol
   end-comment-symbol
   end-group-symbol
   end-option-symbol
   end-repeat-symbol
   except-symbol
   first-quote-symbol
   repetition-symbol
   second-quote-symbol
   special-sequence-symbol
   start-comment-symbol
   start-group-symbol
   start-option-symbol
   start-repeat-symbol
   terminator-symbol
   other-character))

(grammar-rule first-terminal-character
  "see 4.17"
  (grammar-exception terminal-character first-quote-symbol))

(grammar-rule second-terminal-character
  "see 4.18"
  (grammar-exception terminal-character second-quote-symbol))

(grammar-rule terminal-string
  "see 4.16"
  (grammar-or
   (grammar-func
    (grammar-and first-quote-symbol
                 first-terminal-character
                 (grammar-* first-terminal-character)
                 first-quote-symbol)
    (lambda (x) (list (list :terminal-string (format nil "~A~{~A~}" (cadr x) (caddr x))))))
   (grammar-func
    (grammar-and second-quote-symbol
                 second-terminal-character
                 (grammar-* second-terminal-character)
                 second-quote-symbol)
    (lambda (x) (list (list :terminal-string (format nil "~A~{~A~}" (cadr x) (caddr x))))))))

(grammar-rule gap-free-symbol
  "see 6.3"
  (grammar-or
   (grammar-exception terminal-character (grammar-or first-quote-symbol
                                                     second-quote-symbol))
   terminal-string))

(grammar-rule gap-separator
  "see 6.4"
  (grammar-chartable #\Space #\Tab #\Newline #| vertical tab, form feed |#))

(grammar-rule syntax-printing
  "see 6.5
Strips all non-printing characters from the input."
  (grammar-func
   (grammar-and (grammar-* gap-separator)
                gap-free-symbol
                (grammar-* gap-separator)
                (grammar-* (grammar-and (grammar-func gap-free-symbol
                                                      (lambda (x) (declare (ignore x))
                                                              (subseq string start ebnf::end)))
                                        (grammar-func (grammar-* gap-separator)
                                                      (lambda (x) (declare (ignore x))
                                                              nil)))))
   (lambda (x)
     (format nil "~{~A~}" (cons (nth 1 x) (mapcar (lambda (x) (car x)) (nthcdr 3 x)))))))


;;;
;;; Removal of bracketed textual comments from gap-free symbols
;;;


(grammar-rule ebnf-integer
  "see 4.9"
  (grammar-and decimal-digit
               (grammar-* decimal-digit)))

(grammar-rule meta-identifier-character
  "see 4.15"
  (grammar-or letter decimal-digit))

(grammar-rule meta-identifier
  "see 4.14"
  (grammar-func (grammar-and letter
                             (grammar-* meta-identifier-character))
                (lambda (x) `(:meta-identifier ,(format nil "~A~{~A~}" (car x) (cdr x))))))
  

(grammar-rule special-sequence-character
  "see 4.20"
  (grammar-exception terminal-character special-sequence-symbol))

(grammar-rule special-sequence
  "see 4.19"
  (grammar-and special-sequence-symbol
               (grammar-* special-sequence-character)
               special-sequence-symbol))

(grammar-rule commentless-symbol
  "see 6.6"
  (grammar-or
   (grammar-exception terminal-character
                      (grammar-or letter
                                  decimal-digit
                                  first-quote-symbol
                                  second-quote-symbol
                                  start-comment-symbol
                                  end-comment-symbol
                                  special-sequence-symbol
                                  other-character))
   meta-identifier
   ebnf-integer
   terminal-string
   special-sequence))

(grammar-rule bracketed-textual-comment
  "see 6.8"
  (grammar-and start-comment-symbol
               (grammar-* comment-symbol)
               end-comment-symbol))

(grammar-rule comment-symbol
  "see 6.7"
  (grammar-or bracketed-textual-comment
              other-character
              commentless-symbol))

(grammar-rule syntax-uncommented
  "see 6.9
Strips all comments from the input"
  (grammar-func
   (grammar-and (grammar-* bracketed-textual-comment)
                (grammar-func commentless-symbol
                              (lambda (x) (declare (ignore x))
                                      (subseq string start end)))
                (grammar-* bracketed-textual-comment)
                (grammar-* (grammar-and (grammar-func commentless-symbol
                                                      (lambda (x) (declare (ignore x))
                                                              (subseq string start end)))
                                        (grammar-func (grammar-* bracketed-textual-comment)
                                                      (lambda (x) (declare (ignore x))
                                                              nil)))))
   (lambda (x)
     (format nil "~{~A~}" (cons (nth 1 x) (mapcar (lambda (x) (car x)) (nthcdr 3 x)))))))


;;;
;;; Abstract EBNF syntax
;;;

;; Suppress warning
(declaim (ftype function definitions-list))

(grammar-rule optional-sequence
  "see 4.11"
  (grammar-and start-option-symbol
               definitions-list
               end-option-symbol))

(grammar-rule repeated-sequence
  "see 4.12"
  (grammar-and start-repeat-symbol
               definitions-list
               end-repeat-symbol))

(grammar-rule grouped-sequence
  "see 4.13"
  (grammar-and start-group-symbol
               definitions-list
               end-group-symbol))

(grammar-rule empty-sequence
  "see 4.14"
  (declare (ignore ebnf:string))
  (grammar-n 0 nil))

(grammar-rule syntactic-primary
  "see 4.10"
  (grammar-or optional-sequence
              repeated-sequence
              grouped-sequence
              meta-identifier
              terminal-string
              special-sequence
              empty-sequence))

(grammar-rule syntactic-factor
  "see 4.8"
  (grammar-and (grammar-optional (grammar-and ebnf-integer
                                              repetition-symbol))
               syntactic-primary))

(grammar-rule syntactic-exception
  "see 4.7"
  (grammar-n 1 syntactic-factor)) ; modulo nebulous conditions

(grammar-rule syntactic-term
  "see 4.6"
  (grammar-and syntactic-factor
               (grammar-optional (grammar-and except-symbol
                                              syntactic-exception))))

(grammar-rule single-definition
  "see 4.5"
  (grammar-and syntactic-term
               (grammar-* (grammar-and concatenate-symbol
                                       syntactic-term))))

(grammar-rule definitions-list
  "see 4.4"
  (grammar-and single-definition
               (grammar-* (grammar-and definition-separator-symbol
                                       single-definition))))

(grammar-rule syntax-rule
  "see 4.3"
  (grammar-and meta-identifier
               defining-symbol
               definitions-list
               terminator-symbol))

(grammar-rule syntax-abstract
  "see 4.2"
  (grammar-and syntax-rule
               (grammar-* syntax-rule)))

(grammar-rule syntax
  "Full ISO-14977 EBNF syntax"
  (multiple-value-bind (ep vp) (syntax-printing string :start start)
    (when ep
      (multiple-value-bind (ec vc) (syntax-uncommented vp :start 0)
        (when ec
          (syntax-abstract vc :start 0))))))

;; Demos
;(iso14977:syntax-abstract "test='ab c',\"g\'night\"|'c','d';")
;(iso14977:syntax-abstract "test='a b','c d'|3*('e','f');")
;(iso14977:syntax "(* hello *) test='ab c',\"g\'night\"(*test*) | 'c' , 'd';")
