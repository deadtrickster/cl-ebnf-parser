(defun c++-lex-phase3 (string &optional (start 0) (end (length string)))
  "approximate phase 3"
  ())

;; 2.2 -- impl

;; phase 1
;; take input string and produce an output string
;; trigraphs to single characters
;; \r\n to \n
;; complain if anything doesn't match the basic-source-character-set
;; this should get line numbers correct and columns close enough
;; if desired, some structure could track offsets on affected lines

(defun c++-lex-phase2 (source-string)
  "strip out backslash-newline sequences -- need to preserve line numbers..."
  (let* ((match (format nil "\\~%" slash-n))
         (count
          (loop
             for c = 0 then (1+ c)
             for p0 = 0 then (+ 2 pos)
             for pos = (search match source-string :start2 p0)
             while pos
             finally (return c)))
         (result (make-string (- (length source-string) (* 2 count)))))
    (loop
       for r0 = 0 then (+ r0 (- pos p0))
       for p0 = 0 then (+ 2 pos)
       for pos = (search (format nil "\\~%" slash-n) source-string :start2 p0)
       do (replace result source-string :start1 r0 :start2 p0 :end2 (or pos (length source-string)))
       while pos)
    result))

;; approximate phase 3
(defrule whitespace
  (or (c-comment) (c++-comment) slash-t slash-n slash-v slash-f slash-r " "))


(defrule c++-lex-phase3
  ;; need to detect "#" "include" and bind *enable-header* to t
  ;; need to make sure whitespace exists between certain tokens?
  (repeat 0 nil (or (preprocessing-token) (whitespace))))

(c++-lex-phase3 "#define a
//this is a 123456 test
/* test */
void f(int *x);
")

(defun parse-file (filename)
  (let (str
        (*filename* filename))
    (with-open-file (file filename)
      (setf str (make-sequence 'string (file-length file)))
      (read-sequence str file))
    (setf str (c++-lex-phase2 str))
    ;; eventually return the AST, the comment list, and the preproc list
    ;; also return a list of newlines (so line/col can be quickly calculated)
    ;; also return an indication if the parse didn't consume the whole file
    (c++-lex-phase3 str)))
