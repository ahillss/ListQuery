;; (import (srfi11))
(define style 'style2)

;; (define (query-branch-end qs as bs rs depth)
;;   (if (equal? qs '(branch))
;;       (values as bs
;;               (cond [(or (equal? 'style1 style) (equal? 'style2 style))
;;                      rs]
;;                     [(or (equal? 'style3 style) (equal? 'style4 style))
;;                      (cons 'branch rs)]
;;                     [else (error "no style.")])
;;               #t)
;;       #f))
;; (define (query-qs-done qs as bs rs depth)
;;   (if (null? qs)
;;       (values as bs rs #t)
;;      #f))

;; (define (query-as-done qs as bs rs depth)
;;   (if (null? qs)
;;       (values as bs rs #t)
;;       #f))

(define (query2 qs as bs rs depth)
  (cond [(equal? qs '(branch))
         ;;at end of branch parsing
         (values as bs
                 (cond [(or (equal? 'style1 style) (equal? 'style2 style))
                        rs]
                       [(or (equal? 'style3 style) (equal? 'style4 style))
                        (cons 'branch rs)]
                       [else (error "no style.")])
                 #t)]
        [(null? qs)
         ;;on no more queries
         (values as bs rs #t)]
        [(and (null? as) (equal? 'many0 (car qs)))
         ;;on no more input, at end of many parsing
         (values '() bs
                 (cond [(or (equal? 'style1 style) (equal? 'style2 style))
                        rs]
                       [(or (equal? 'style3 style) (equal? 'style4 style))
                        (cons 'many rs)]
                       [else (error "no style.")])
                 #t)]
        [(null? as)
         ;;no more input and queries left
         (values '() bs rs #f)]
        [(equal? 'value (car qs))
         ;;on value parsing
         (values (cdr as) bs
                 (cond [(or (equal? 'style1 style) (equal? 'style2 style))
                        (cons (car as) rs)]
                       [(equal? 'style3 style)
                        (cons (list 'value
                                    ;; (cadr qs)
                                    (car as)) rs)]
                       [(equal? 'style4 style)
                        (list 'value
                              ;; (cadr qs)
                              (car as))]
                       [else (error "no style.")])
                 #t)]
        [(equal? 'any (car qs))
         ;;on any parsing
         (values (cdr as) bs
                 (cond [(or (equal? 'style1 style) (equal? 'style2 style))
                        (cons (car as) rs)]
                       [(equal? 'style3 style)
                        (cons (list 'any (car as)) rs)]
                       [(equal? 'style4 style)
                        (list 'any (car as))]
                       [else (error "no style.")])
                 #t)]
        [(equal? 'leaf (car qs))
         ;;on leaf parsing
         (if (equal? (cadr qs) (car as))
             (values (cdr as) bs
                     (cond [(or (equal? 'style1 style) (equal? 'style2 style))
                            (cons (car as) rs)]
                           [(equal? 'style3 style)
                            (cons (list 'leaf (car as)) rs)]
                           [(equal? 'style4 style)
                            (list 'leaf (car as))]
                           [else (error "no style.")])
                     #t)
             (values (cdr as) (cons (car as) bs) rs #f))]
        [(equal? 'relative0 (car qs))
         (if (list? (car as))
             (let-values
                 ([(as2 bs2 rs2 ok2)
                   (query2 (cons 'relative (cdr qs)) (car as) '()
                           rs (+ 1 depth))])
               (if ok2
                   (values (cons (append-reverse bs2 as2) (cdr as)) bs
                           (cond [(or (equal? 'style1 style) (equal? 'style2 style)) rs2]
                                 [(or (equal? 'style3 style) (equal? 'style4 style))
                                  (cons 'relative (cdr rs2))]
                                 [else (error "no style.")])
                           #t)
                   (query2 qs (cdr as) (cons (car as) bs) rs depth)
                   ))
             (query2 qs (cdr as) (cons (car as) bs) rs depth)
             )]
        [(equal? 'relative (car qs))
         (let-values
             ([(as2 bs2 rs2 ok2)
               (query2 (cons 'branch (cdr qs)) as bs rs (+ 1 depth))])
           (if ok2
               (values as2 bs2
                       (cond [(or (equal? 'style1 style) (equal? 'style2 style))
                              rs2]
                             [(or (equal? 'style3 style) (equal? 'style4 style))
                              (cons 'relative (cdr rs2))]
                             [else (error "no style.")])
                       #t)
               (query2 (cons 'relative0 (cdr qs)) as bs rs depth)
               ))]
        [(or (equal? 'many (car qs))
             (equal? 'many0 (car qs)))
         ;;on many starting or continuing parsing
         (let-values
             ([(as2 bs2 rs2 ok2)
               ;;try query within many query
               (query2 (list 'branch (cadr qs)) as bs
                       rs (+ 1 depth))])
           (cond [ok2
                  ;;on success, try query again from last pos
                  (query2 (cons 'many0 (cdr qs)) as2 bs2
                          (cond [(or (equal? 'style1 style) (equal? 'style2 style))
                                 rs2]
                                [(or (equal? 'style3 style) (equal? 'style4 style))
                                 (cdr rs2)]
                                [else (error "no style.")])
                          depth)]
                 [(equal? 'many0 (car qs))
                  ;;on fail and a continued many, return sucess
                  (values as bs
                          (cond [(or (equal? 'style1 style) (equal? 'style2 style))
                                 rs]
                                [(or (equal? 'style3 style) (equal? 'style4 style))
                                 (cons 'many rs)]
                                [else (error "no style.")])
                          #t)]
                 [else
                  ;;on fail and a first many, return failed
                  (values as2 bs2 rs2 #f)]))]
        [(equal? 'branch (car qs))
         ;;on branch
         (cond [(or (equal? 'many (caadr qs))
                    (equal? 'relative (caadr qs)))
                ;;on inner many or relative
                (let-values
                    ([(as2 bs2 rs2 ok2)
                      ;;try inner many/relative query
                      (query2 (cadr qs) as bs
                              (cond [(equal? 'style1 style)
                                     rs]
                                    [(or (equal? 'style2 style)
                                         (equal? 'style3 style)
                                         (equal? 'style4 style))
                                     '()]
                                    [else (error "no style.")])
                              (+ 1 depth))])
                  (if ok2
                      ;;on success, try next query within branch and restore
                      ;;discarded input
                      (query2 (cons 'branch (cddr qs))
                              (append-reverse bs2 as2) '()
                              (cond [(equal? 'style1 style) rs2]
                                    [(or (equal? 'style2 style)
                                         (equal? 'style3 style)
                                         (equal? 'style4 style))
                                     (cons rs2 rs)]
                                    [else (error "no style.")])
                              depth)
                      ;;on fail, return failed
                      (values as2 bs2 rs #f)
                      ))]
               [(and (equal? 'branch (caadr qs)) (list? (car as)))
                ;;on inner branch and current input element is a list
                (let-values
                    ([(as2 bs2 rs2 ok2)
                      ;;try inner branch query
                      (query2 (cadr qs) (car as) '() '() (+ 1 depth))])
                  (if ok2
                      ;;on success, try next query within branch and restore
                      ;;discarded input
                      (query2 (cons 'branch (cddr qs))
                              (append-reverse bs (cdr as)) '()
                              (cons rs2 rs) depth)
                      ;;on fail, try the same query on the next input element,
                      ;;discarding current input element
                      (query2 qs (cdr as) (cons (car as) bs) rs depth)
                      ))]
               [(equal? 'branch (caadr qs))
                ;;on inner branch and current input element is not a list,
                ;;try the same query on the next input element, discarding
                ;;current input element
                (query2 qs (cdr as) (cons (car as) bs) rs depth)]
               [else
                ;;on inner query not a branch
                (let-values
                    ([(as2 bs2 rs2 ok2)
                      ;;try inner query
                      (query2 (cadr qs) as bs rs (+ 1 depth))])
                  (if ok2
                      ;;on success, try next query within branch and restore
                      ;;discarded input
                      (query2 (cons 'branch (cddr qs))
                              (append-reverse bs2 as2)
                              '()
                              (cond [(or (equal? 'style1 style) (equal? 'style2 style)) rs2]
                                    [(or (equal? 'style3 style) (equal? 'style4 style)) (cons rs2 rs)]
                                    [else (error "no style.")])
                              depth)

                      ;;on fail, try the same query on the next input element,
                      ;;discarding current input element
                      (query2 qs as2 bs2 rs depth)
                      ))])]
        [else
         ;;unable to parse query
         ;; (values as bs rs #f)
         (error "problem with ~a" qs)
         ]))

(define (append-reverse lst tail)
  ;; (append (reverse lst) tail)
  (if (null? lst)
      tail
      (append-reverse (cdr lst) (cons (car lst) tail))))

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

(define (reverse-query qs)

  1)
(define (group-values qs rs)
  1)

;; (define (pattern->query p)
;;   (cond [])
;;   `(branch)
;;   1)

(let* ([q '(branch
            (leaf q)
            (many (branch (leaf a) (value) (any)))
            (relative (branch (leaf b) (any) (value))))]
       [d '(q (q (b 1 11) (c 2 22)) (a 3 33) (a 4 44))]
       [r (query q d)]
       [x (group-values q r)])

  (pretty-print q)
  (pretty-print d)
  (pretty-print r)

  (pretty-print x)
  (pretty-print (reverse-query q))
  )
;; (query-pattern '(a))

;; (require macro-debugger/stepper)
;; (require macro-debugger/expand)
;; (require (for-syntax racket/syntax))

;; (define-syntax (qpattern stx)
;;   (syntax-case stx ()
;;     [(_ m p v)
;;      #'(list 'car p v)]))

;; (define-syntax (qmatch stx)
;;   (syntax-case stx (else)
;;     [(_ m (else r)) #'r]
;;     [(_ m) #'#f]
;;     [(_ m (p r) rest ...)

;;      #'(let* ((mm m)
;;               (pp p)
;;               (qq (query pp mm)))
;;          (if (not (null? qq))
;;              r
;;              (qmatch mm rest ...)
;;              ))
;;      ]))

;; ;; (pretty-print
;; ;;  (syntax->datum
;; ;;   (expand-only
;; ;;    #'(qmatch '(html (body (p "hello")))
;; ;;        [1 2]
;; ;;        [3 4]
;; ;;        [else 5]
;; ;;        )

;; ;;    (list #'qmatch))))

;;  (qmatch '(html (body (p "hello")))
;;     [(html (body )) 2]
;;     [3 4]
;;     [else 6]
;;     )
