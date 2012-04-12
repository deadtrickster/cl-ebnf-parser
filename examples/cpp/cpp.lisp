;; C++ standard, chapter 16: Preprocessing directives
;; (PDF p. 426, printed p. 411)

;; need to allow for whitespace, including comments

(defrule if-group
  (or (and "#" "if")
      (and "#" "ifdef")
      (and "#" "ifndef")))

(defrule if-section
  (and (if-group)
       (optional (elif-groups))
       (optional (else-group))
       (assert (endif-line))))

(defrule group-part
  (or (if-section)
      (control-line)
      (text-line)
      (and "#" (non-directive))))

(defrule preprocessing-file
    ;; inline the rule for group
    (repeat 0 nil (group-part)))