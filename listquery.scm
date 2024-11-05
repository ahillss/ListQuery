;;todo: remove from leaves, anys etc from results as they are not needed, only the vals are
;; (import (srfi11))

(define (merge-list lst)
  (foldl (lambda (x r) (map cons x r))
         (map list (car lst))
         (cdr lst)))

(merge-list '((1 2 3) ( a b c) (q r s)))

;; (map (lambda (x y) (list x y)) '(a b c) '(1 2 3))


(define (query2 qs as bs rs depth)
  (cond [(equal? qs '(branch))
         ;;at end of branch parsing
         (values as bs rs #t)]
        [(null? qs)
         ;;on no more queries
         (values as bs rs #t)]
        [(and (null? as) (equal? 'many0 (car qs)))
         ;;on no more input, at end of many parsing
         (values '() bs rs #t)]
        [(null? as)
         ;;no more input and queries left
         (values '() bs rs #f)]
        [(equal? 'value (car qs))
         ;;on value parsing
         (if (list? (car as))
             (values (cdr as) (cons (car as) bs) rs #f)
             (values (cdr as) bs (cons (car as) rs) #t))]
        [(equal? 'any (car qs))
         ;;on any parsing
         (if (list? (car as))
             (values (cdr as) (cons (car as) bs) rs #f)
             (values (cdr as) bs (cons (car as) rs) #t))]
        [(equal? 'leaf (car qs))
         ;;on leaf parsing
         (if (equal? (cadr qs) (car as))
             (values (cdr as) bs (cons (car as) rs) #t)
             (values (cdr as) (cons (car as) bs) rs #f))]
        [(equal? 'relative0 (car qs))
         (if (list? (car as))
             (let-values ([(as2 bs2 rs2 ok2)
                   (query2 (cons 'relative (cdr qs)) (car as) '() rs (+ 1 depth))])
               (if ok2
                   (values (cons (append-reverse bs2 as2) (cdr as)) bs rs2 #t)
                   (query2 qs (cdr as) (cons (car as) bs) rs depth) ))
             (query2 qs (cdr as) (cons (car as) bs) rs depth) )]
        [(equal? 'relative (car qs))
         (let-values ([(as2 bs2 rs2 ok2)
               (query2 (cons 'branch (cdr qs)) as bs rs (+ 1 depth))])
           (if ok2
               (values as2 bs2 rs2 #t)
               (query2 (cons 'relative0 (cdr qs)) as bs rs depth) ))]
        [(or (equal? 'many (car qs))
             (equal? 'many0 (car qs)))
         ;;on many starting or continuing parsing
         (let-values ([(as2 bs2 rs2 ok2)
               ;;try query within many query
               (query2 (list 'branch (cadr qs)) as bs '() (+ 1 depth))])
           (cond [ok2
                  ;;on success, try query again from last pos
                  (query2 (cons 'many0 (cdr qs)) as2 bs2 (cons rs2 rs) depth)]
                 [(equal? 'many0 (car qs))
                  ;;on fail and a continued many, return sucess
                  (values as bs rs #t)]
                 [else
                  ;;on fail and a first many, return failed
                  (values as2 bs2 rs2 #f)]))]
        [(equal? 'branch (car qs))
         ;;on branch
         (cond [(equal? 'relative (caadr qs))
                ;;on inner relative
                (let-values ([(as2 bs2 rs2 ok2)
                      ;;try inner relative query
                      (query2 (cadr qs) as bs '() (+ 1 depth))])
                  (if ok2
                      ;;on success, try next query within branch and restore
                      ;;discarded input
                      (query2 (cons 'branch (cddr qs)) (append-reverse bs2 as2) '() (cons rs2 rs) depth)
                      ;;on fail, return failed
                      (values as2 bs2 rs #f)
                      ))]
               [(equal? 'many (caadr qs))
                ;;on inner many
                (let-values ([(as2 bs2 rs2 ok2)
                              ;;try inner many query
                              (query2 (cadr qs) as bs '() (+ 1 depth))])
                  (if ok2
                      ;;on success, try next query within branch and restore
                      ;;discarded input
                      (query2 (cons 'branch (cddr qs)) (append-reverse bs2 as2) '()
                              (append
                                     (merge-list rs2)
                                      rs) depth)
                      ;;on fail, return failed
                      (values as2 bs2 rs #f)
                      ))]
               [(and (equal? 'branch (caadr qs)) (list? (car as)))
                ;;on inner branch and current input element is a list
                (let-values ([(as2 bs2 rs2 ok2)
                      ;;try inner branch query
                      (query2 (cadr qs) (car as) '() rs (+ 1 depth))])
                  (if ok2
                      ;;on success, try next query within branch and restore
                      ;;discarded input
                      (query2 (cons 'branch (cddr qs)) (append-reverse bs (cdr as)) '() rs2 depth)
                      ;;on fail, try the same query on the next input element,
                      ;;discarding current input element
                      (query2 qs (cdr as) (cons (car as) bs) rs depth) ))]
               [(equal? 'branch (caadr qs))
                ;;on inner branch and current input element is not a list,
                ;;try the same query on the next input element, discarding
                ;;current input element
                (query2 qs (cdr as) (cons (car as) bs) rs depth)]
               [else
                ;;on inner query not a branch
                (let-values ([(as2 bs2 rs2 ok2)
                      ;;try inner query
                      (query2 (cadr qs) as bs rs (+ 1 depth))])
                  (if ok2
                      ;;on success, try next query within branch and restore
                      ;;discarded input
                      (query2 (cons 'branch (cddr qs)) (append-reverse bs2 as2) '() rs2 depth)
                      ;;on fail, try the same query on the next input element,
                      ;;discarding current input element
                      (query2 qs as2 bs2 rs depth) ))])]
        [else
         ;;unable to parse query
         ;; (values as bs rs #f)
         (error "problem with" qs as bs rs) ]))

(define (append-reverse lst tail)
  ;; (append lst tail)
  ;; (append (reverse lst) tail)
  (if (null? lst)
      tail
      (append-reverse (cdr lst) (cons (car lst) tail)))
  )

;; (append-reverse (list 1 2 3 4) (list 5))

(define (whitespace n)
  (if (> n 0)
      (string-append "  " (whitespace (- n 1)))
      ""))

;; (when #t
;;   (set! query2
;;         (let ([query3 query2]
;;               [count 0])
;;           (lambda (qs as bs rs depth)
;;             (set! count (+ 1 count))
;;             (let ([count2 count])
;;               (display (whitespace depth))
;;               (write (list '+ count2 '@ depth 'in:   qs as bs rs )) (newline)
;;               (let-values ([(as2 bs2 rs2 ok2)
;;                             (query3 qs as bs rs depth)])
;;                 (display (whitespace depth))
;;                 (write (list '- count2 '@ depth 'out   as2 bs2 rs2 ok2)) (newline)
;;                 (values as2 bs2 rs2 ok2))
;;               )))))

(define (query qs as)
  (let-values ([(as2 bs2 rs2 ok2) (query2 qs as '() '() 0)])
    ;; (write (list 'result as2 bs2 rs2 ok2)) (newline)
    (if ok2
        ;; (reverse rs2)
        rs2
        '())))






;; (define-syntax (qmatch-case stx)
;;   (syntax-case stx ()
;;     [(_ x (c0 r0) ) #'2]
;;     [(_ x (c0 r0) (c r) ... (else e)) #'1]
;;     [(_ x (c0 r0) (c r) ... (else e)) #'1]

;;     ))

;; (define-syntax (qmatch stx)
;;   (syntax-case stx (else)
;;     ;; [(_ ) #'(error "query match: expecting input.")]
;;     ;; [(_ x) #'(error "query match: expecting cases.")]
;;     [(_ x (c0 r0) ) #'2]
;;     [(_ x (c0 r0) (c r) ... (else e)) #'1]
;;     [(_ x (c0 r0) (c r) ... (else e)) #'1]

;;     ))


;; (qmatch '(html (body (p "hello")))
;;   [a b]
;;   [c d]
;;   [else e]
;;   )



;; (define-syntax (qqq stx)
;;   (syntax-case stx ()
;;     [(_ x ,y ) #'(quote ,y)]

;;     ))
;; (qqq 1 ,())

(let* ([q '(branch (leaf body)
                   (many
                    (branch
                     (leaf p) (value)
                     (many (branch (leaf a) (value)))
                     ))
                   ;; (many (value))
                   ;; (branch (leaf y) (branch (leaf z) (leaf w)))
                   )]
       [d '(body
            "yo1" "hello1"
            (p "hello" (a "world") (a "there"))
            (p "x" (a "y"))
            ;; (p "hello") (p "world")
            (y (z w))
            )]
       [r (query q d)]
       ;; [x (group-values q r)]
       )

  (pretty-print q)
  ;; (pretty-print (reverse-query q))
  (pretty-print d)
  (pretty-print r)


  ;; (pretty-print x)
  ;; (pretty-print (reverse-query q))
  )
