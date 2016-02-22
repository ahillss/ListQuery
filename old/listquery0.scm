;;

(define (append-reverse lst tail)
  ;; (append (reverse lst) tail)
  (if (null? lst)
      tail
      (append-reverse (cdr lst) (cons (car lst) tail))))

(define (whitespace n)
  (if (> n 0)
      (string-append "  " (whitespace (- n 1)))
      ""))

(define query2
  (let ([count 0])
    (lambda (qs as bs rs depth)
      (set! count (+ 1 count))
      (let ([count2 count])
        (display (whitespace depth))
        (write (list '+ count2 '@ depth 'in:   qs as bs rs )) (newline)
        (let-values ([(as2 bs2 rs2 ok2)
                      (query3 qs as bs rs depth)])
          (display (whitespace depth))
          (write (list '- count2 '@ depth 'out   as2 bs2 rs2 ok2)) (newline)
          (values as2 bs2 rs2 ok2))
        )
      ))
  )


(define (query3 qs as bs rs depth)
  (cond [(equal? qs '(branch))
         (values as bs rs #t)]
        [(null? qs)
         (values as bs rs #t)]
        [(and (null? as) (equal? 'many0 (car qs)))
         (values '() bs rs #t)]
        [(null? as)
         (values '() bs rs #f)]
        [(equal? 'any (car qs))
         (values (cdr as) bs (cons (car as) rs) #t)]
        [(equal? 'leaf (car qs))
         (if (or (and (procedure? (cadr qs)) ((cadr qs) (car as)))
                 (equal? (cadr qs) (car as)))
             (values (cdr as) bs (cons (car as) rs) #t)
             (values (cdr as) (cons (car as) bs) rs #f)
             )]
        [(equal? 'relative0 (car qs))
         (if (list? (car as))
             (let-values
                 ([(as2 bs2 rs2 ok2)
                   (query2 (cons 'relative (cdr qs)) (car as) '()
                           rs (+ 1 depth))])
               (if ok2
                   (values (cons (append-reverse bs2 as2) (cdr as)) bs rs2 #t)
                   (query2 qs (cdr as) (cons (car as) bs) rs depth)
                   ))
             (query2 qs (cdr as) (cons (car as) bs) rs depth)
             )]
        [(equal? 'relative (car qs))
         (let-values
             ([(as2 bs2 rs2 ok2)
               (query2 (cons 'branch (cdr qs)) as bs rs (+ 1 depth))])
           (if ok2
               (values as2 bs2 rs2 #t)
               (query2 (cons 'relative0 (cdr qs)) as bs rs depth)
               ))]
        [(or (equal? 'many (car qs))
             (equal? 'many0 (car qs)))
         (let-values
             ([(as2 bs2 rs2 ok2)
               (query2 (list 'branch (cadr qs)) as bs rs (+ 1 depth))])
           (cond [(and (equal? 'many0 (car qs)) (not ok2))
                  (values as bs rs #t)]
                 [(not ok2)
                  (values as2 bs2 rs2 #f)]
                 [else
                  (query2 (cons 'many0 (cdr qs)) as2 bs2 rs2 depth)
                  ]))]
        [(or (equal? 'branch (car qs))
             (equal? 'splice-branch (car qs)))
         (cond [(and (or (equal? 'branch (caadr qs))
                         (equal? 'splice-branch (caadr qs)))
                     (list? (car as)))
                (let-values
                    ([(as2 bs2 rs2 ok2)
                      (query2 (cadr qs) (car as) '()
                              (if (equal? 'splice-branch (caadr qs))
                                  rs '())
                              (+ 1 depth))])
                  (if ok2
                      (query2 (cons 'branch (cddr qs))
                              ;; (cdr as) bs

                              (append-reverse bs (cdr as)) '()

                              (if (equal? 'splice-branch (caadr qs))
                                  rs2 (cons (reverse rs2) rs))
                              depth)
                      (query2 qs (cdr as) (cons (car as) bs) rs depth)
                      ))]
               [(or (equal? 'branch (caadr qs))
                    (equal? 'splice-branch (caadr qs)))
                (query2 qs (cdr as) (cons (car as) bs) rs depth)]
               [else
                (let-values
                    ([(as2 bs2 rs2 ok2)
                      (query2 (cadr qs) as bs rs (+ 1 depth))])
                  (if ok2
                      (query2 (cons 'branch (cddr qs))
                              ;; as2 bs2
                               (append-reverse bs2 as2) '()
                              rs2 depth)
                      (query2 qs as2 bs2 rs depth)
                      ))])]
        [(equal? 'discard (car qs))
         (let-values
             ([(as2 bs2 rs2 ok2)
               (query2 (cons 'branch (cdr qs)) as bs rs (+ 1 depth))])
           (values as2 bs2 (if ok2 rs rs2) ok2))]
        [else
         ;; (values as bs rs #f)
         (error "problem with ~a" qs)
         ]))

(define (query qs as)
  (newline)
  (let-values ([(as2 bs2 rs2 ok2) (query2 qs as '() '() 0)])
    ;; (write (list 'result as2 bs2 rs2 ok2)) (newline)
    (if ok2
        (reverse rs2)
        '())))

(display
  (query '(branch (many (branch (leaf a) (branch (leaf c) (any)) (many (branch (leaf b) (any)))))
                 (any))
        '((a (c 1) (b 11) (b 22) (b 33) )
          (a (c 2) (b 44) (b 55))
          (bla 1 2 3)
          )   )

  ;; (query '(branch (many (branch (leaf a) (any))) (many (branch (leaf b) (any))))
        ;; '(a 3 (a 1) (a 2) (a 6) (b 9 ) x (a 7) (b 4) (a 10) (b 5) ))
 ;; (query '(many (branch (leaf a) (any))) '(q (a 1) ( a 2)))
   ;; (query '(branch  (branch (leaf a) (any)) (branch (leaf b) (any))) '((b 5) (a 1) )   )
 ;; (query '(branch (leaf a)  (leaf b) ) '(b a )   )
 )
(newline)
