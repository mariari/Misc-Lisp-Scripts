
;; matches any character expect newline
(define (r:dot) ".")

;; matches only the beginning of a line
(define (r:bol) "^")

;; matches only the end of a line
(define (r:eol) "$")

;; matches the string
(define (r:quote string)
  (r:seq
   (list->string
    (append-map (lambda (char)
                  (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                (string->list string)))))

;; matches one character that is in the string
(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else
     (bracket string
              (lambda (members)
                (if (lset= eqv? '(#\- #\^) members)
                    '(#\- #\^)
                    (quote-bracketed-contents members)))))))

;; matches one character that is not in the string
(define (r:char-not-from string)
  (bracket string
           (lambda (members)
             (cons #\^ (quote-bracketed-contents members)))))

;; compounds

;; >>= in parser combinators
;; this pattern matches each argument pattern in sequence from left to
;; right
(define (r:seq . exprs)
  ;; ( ) here are made to isolate the environment, but we must escape
  ;; them, as regex treats them oddly
  (string-append "\\(" (apply string-append exprs) "\\)"))

;; <|>/choice in parser combinators
;; this pattern tries each argument pattern from left to right, until
;; one of these alternative matches, if none match then this pattern
;; does not match
(define (r:alt . expers)
  (if (pair? expers)
      (apply r:seq
             (cons (car expers)
                   (append-map (lambda (expr)
                                 (list "\\|" expr))
                               (cdr expers))))
      (r:seq)))

;; many in parser combinators
;; this pattern tries to match the argument pattern a minimum of min
;; times but no more than maximum of max times. if max is given as #f
;; then no maximum is specified. If max equals min, the given pattern
;; must match exactly that many times
(define (r:repeat min max expr)
  (apply r:seq
         (if (and (= 0 min) (not max))
             (list expr "*")
             (list expr
                   "\\{"
                   (number->string min)
                   ","
                   (if (not max) "" (number->string max))
                   "\\}"))))

(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

(define (bourne-shell-quote-string string)
  (list->string
   (append (list #\')
           (append-map (lambda (char)
                         (if (char=? char #\')
                             (list #\' #\\ char #\')
                             (list char)))
                       (string->list string))
           (list #\'))))

(define r:+
  (((curry-argument 2) 1 #f) r:repeat))

(define r:*
  (((curry-argument 2) 0 #f) r:repeat))


;; ---------------------------------
;; Regex helper
;; ---------------------------------

(define chars-needing-quoting
  '(#\. #\[ #\\ #\^ #\$ #\*))


(define (bracket string procedure)
  (list->string
   (append '(#\[)
           (procedure (string->list string))
           '(#\]))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members) (list char) '()))
  (append (optional #\])
          (remove
           (lambda (c)
             (memv c chars-needing-quoting-in-brackets))
           members)
          (optional #\^)
          (optional #\-)))

(define chars-needing-quoting-in-brackets
  '(#\] #\^ #\-))
