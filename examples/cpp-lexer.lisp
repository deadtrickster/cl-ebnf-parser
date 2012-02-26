;; CPP lexer
;; based on "JTC1.22.32 - ISO/IEC 14882 - Programming language C++ draft February 2011" (n3242.pdf)

(defpackage #:cpp-lexer
  (:use :common-lisp #:ebnf))

(in-package #:cpp-lexer)

(defgrammar "
(* 2.4 *)")

(defgrammar "(* 2.5 *)
preprocessing-token=header-name|identifier|pp-number|character-literal|user-defined-character-literal|string-literal|user-defined-string-literal|preprocessing-op-or-punc|other-non-whitespace;")

#|
(* 2.6 -- rules need renaming and need substitutions below *)
alt{='<%';
alt}='%>';
alt[='<:';
alt]=':>';
alt#='%:';
alt##='%:%:';
alt&&='and';
alt|='bitor';
alt||='or';
alt^='xor';
alt~='compl';
alt&='bitand';
alt&=='and_eq';
alt|=='or_eq';
alt^=='xor_eq';
alt!='not';
alt!=='not_eq';
|#

(defgrammar "(* 2.7 & see note *)
token=identifier|keyword|literal|operator|punctuator;")

(defgrammar "(* 2.8 *)
c-comment='/*',*,'*/';
cxx-comment='//',*,newline;")

(defgrammar "(* 2.9 *)
header-name=('<',h-char-sequence,'>')|('\"',q-char-sequence,'\"');
h-char-sequence=h-char|(h-char-sequence,h-char);
h-char=char-(new-line|'>');
q-char-sequence=q-char|(q-char-sequence,q-char);
q-char=char-(new-line|'\"');")

(defgrammar "(* 2.10 *)
pp-number=digit
  |('.',digit)
  |(pp-number,digit)
  |(pp-number,identifier-nondigit)
  |(pp-number,'e',sign)
  |(pp-number,'E',sign)
  |(pp-number,'.');")

(defgrammar "(* 2.11 *)
identifier=identifier-nondigit
  |(identifier,identifier-nondigit)
  |(identifier,digit);
identifier-nondigit=nondigit|universal-character-name; (* or impl-defined *)
nondigit='a'|'b'|'c'|'d'|'e'|'f'|'g'|...'Y'|'Z'|'_';
digit='0'...'9';
")