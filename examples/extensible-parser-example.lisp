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
