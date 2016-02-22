;;todo:
;;make a query converter from a match macro style syntax
;;output is converted into a list an element for each var specified in the query


(define (query2 qs as bs rs depth)
  (cond [(equal? qs '(branch))
         ;;at end of branch parsing
            ;; (values as bs rs #t) ;;style1,style2
         (values as bs (cons 'branch rs) #t) ;;style3
         ]
        [(null? qs)
         ;;on no more queries
         (values as bs rs #t)]
        [(and (null? as) (equal? 'many0 (car qs)))
         ;;on no more input, at end of many parsing
         (values '() bs
                 ;; rs ;;style1, style2
                 (cons 'many rs) ;;style3
                 #t)]
        [(null? as)
         ;;no more input and queries left
         (values '() bs rs #f)]
        [(equal? 'var (car qs))
         ;;on var parsing
         (values (cdr as) bs
                 ;; (cons (car as) rs) ;;style1, style2
                 (cons (list 'var (cadr qs) (car as)) rs) ;;style3
                 ;; (list 'var (cadr qs) (car as)) ;;style3b
                 #t)]
        [(equal? 'any (car qs))
         ;;on any parsing
         (values (cdr as) bs
                 ;; (cons (car as) rs) ;;style1, style2
                 (cons (list 'any (car as)) rs) ;;style3
                 ;; (list 'any (car as)) ;;style3b
                 #t)]
        [(equal? 'leaf (car qs))
         ;;on leaf parsing
         (if (equal? (cadr qs) (car as))
             (values (cdr as) bs
                     ;; (cons (car as) rs) ;;style1, style2
                     (cons (list 'leaf (car as)) rs) ;;style3
                     ;; (list 'leaf (car as)) ;;style3b
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
                           ;; rs2 ;;style1, style2
                           (cons 'relative (cdr rs2)) ;;style3
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
                       ;; rs2 ;;style1, style2
                       (cons 'relative (cdr rs2)) ;;style3
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
                          ;; rs2 ;;style1, style2
                          (cdr rs2) ;;style3
                          depth)]
                 [(equal? 'many0 (car qs))
                  ;;on fail and a continued many, return sucess
                  (values as bs
                          ;; rs ;;style1, style2
                          (cons 'many rs) ;;style3
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
                              ;; rs ;;style1
                              '() ;;style2, style3
                              (+ 1 depth))])
                  (if ok2
                      ;;on success, try next query within branch and restore
                      ;;discarded input
                      (query2 (cons 'branch (cddr qs))
                              (append-reverse bs2 as2) '()
                              ;; rs2 ;;style1
                              (cons rs2 rs);;style2, style3
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
                              ;; rs2 ;;style1, style2
                              (cons rs2 rs) ;;style3
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
  ;; (newline)
  ;; (print "qs:")
  (pretty-print qs)
  ;; (print "as: ")
  (pretty-print as)
  ;; (print "rs: ")
  (let-values ([(as2 bs2 rs2 ok2) (query2 qs as '() '() 0)])
    ;; (write (list 'result as2 bs2 rs2 ok2)) (newline)
    (if ok2
        ;; (reverse rs2)
        rs2
        '())))

;; (pretty-print (query '(branch (leaf a) (leaf 1)) '(a 1))) (newline)
;; (pretty-print  (query '(branch (branch (leaf a) (var x))) '((a 1)))) (newline)
;; (pretty-print  (query '(branch (var x) (var y) (var z)) '(1 2 3 4 5))) (newline)
;; (pretty-print (query '(many (branch (leaf a) (var x))) '(c (a 1) b (a 2) )))

;; (pretty-print (query '(branch (leaf c) (many (branch (leaf a) (var x)))) '(c (a 1) b (a 2) )))
;; (pretty-print (query '(many (leaf a)) '(c a b a )))
;; (pretty-print (query '(branch (many (leaf a))) '(c a b a )))
;; (pretty-print (query '(leaf a) '(a c a b a )))
;; (pretty-print
;;  (query '(branch (many (branch (leaf a) (var x))) (many (branch (leaf b) (var y))))
;;  '(a 3 (a 1) (a 2) (a 6) (b 9 ) x (a 7) (b 4) (a 10) (b 5) )))

;;  ;; (query '(branch (leaf a) (var x)) '(a 1))

;;  ;; (query '(branch (branch (leaf b))) '((b) (a 1)))

;; (pretty-print
;;  (query
;;   '(branch
;;     (branch (leaf q) (leaf 1))
;;     (many
;;      (branch
;;       (leaf a)
;;       (branch (leaf c) (var x))
;;       (many (branch (leaf b) (var y)))))
;;     (var z))
;;   '((q 1)
;;     (a (c 1) (b 11) (b 22) (b 33) )
;;     (a (c 2) (b 44) (b 55))
;;     (bla 1 2 3))
;;   ))

;; (pretty-print (query
;;                '(relative (branch (leaf p) (any)))
;;                '(html (body (p 1) (p 2)))
;;                ))



;; (pretty-print (query
;;                '(many (relative (branch (leaf p) (any))))
;;                '(html (body (p 1) (p 2)))
;;                ))


;; (pretty-print (query
;;                '(many (relative (branch (leaf p) (any))))
;;                '(html (body (p 1) (p 2 (p 3)) ((p 5) p 4) (div (p 6))))
;;                ))


;; (pretty-print (query
;;                '(many (relative (branch (leaf p) (any))))
;;                '(html (body (p 1) (p 2 (p 3)) ((p 5) p 4) (div (p 6) (p 7)) (p 8)))
;;                ))
;;=>
;; (many (relative (branch (any 7) (leaf p)))
;;       (relative (branch (any 6) (leaf p)))
;;       (relative (branch (any 8) (leaf p)))
;;       (relative (branch (any (p 5)) (leaf p)))
;;       (relative (branch (any 2) (leaf p)))
;;       (relative (branch (any 1) (leaf p))))


;; (pretty-print (query
;;                '(many (relative (branch (leaf p) (any))))
;;                '(html (body (p 1) (div (p 6)) (p 8)))
;;                ))

;;=>
;; (many (relative (branch (any 6) (leaf p)))
;;       (relative (branch (any 8) (leaf p)))
;;       (relative (branch (any 1) (leaf p))))




;; (pretty-print (query
;;                '(many (relative (leaf a)))
;;                '(a)
;;                ))



;; (pretty-print (query
;;                '(relative (leaf a))
;;                '(a)
;;                ))

;; (pretty-print (query '  (leaf a) '(a a a) ))
;; (pretty-print (query '(many (leaf a)) '(a a a) ))







;; (pretty-print
;;  (query
;;   '(many (relative (branch (leaf a) (any))))
;;   '(a 99 (a 1) (b (a 97) (bb (bbb (a 98)))) (c (a 2 (a 3)) (a 33)) d (e (a 4) (a 5) f) g) ))






;; (pretty-print
;;  (query
;;   ' (relative (branch (leaf a) (any)))
;;   '(a 99 (a 1) (b (a 97) (bb (bbb (a 98)))) (c (a 2 (a 3)) (a 33)) d (e (a 4) (a 5) f) g) ))













(pretty-print
 (query
  ' (branch (leaf q) (many (branch (leaf a) (any))) (relative (branch (leaf b) (any))) )
  '(q (q (b 1) (c 2)) (a 3) (a 4))))








       ;; [(equal? 'relative1 (car qs))
        ;;  ]
        ;; [(equal? 'relative0 (car qs))
        ;;   (let-values ([(as2 bs2 rs2 ok2)
        ;;                 (query2 (cadr qs) as bs rs (+ 1 depth))])
        ;;     (print (cons rs2 rs))
        ;;     (if ok2
        ;;         (values (cons (append-reverse bs2 as2) (cdr as)) bs
        ;;                 (cons 'relative (cons rs2 rs)) #t)
        ;;         (values as bs rs #f)
        ;;         ))
        ;;  ;; (if (list? as))
        ;;  ;; (let-values ([(as2 bs2 rs2 ok2)
        ;;                ;; (query2 (cadr qs) as bs rs (+ 1 depth))])
        ;;  ;; (let-values ([(as2 bs2 rs2 ok2) (query2 (cadr qs) as bs
        ;;  ;;                                         rs (+ 1 depth))])
        ;;  ;;   (cond [ok2
        ;;  ;;          (values (cons (append-reverse bs2 as2) (cdr as)) bs
        ;;  ;;                  (cons 'relative (cons rs2 rs)) #t)]
        ;;  ;;         [(list? (car as))
        ;;  ;;          (let-values ([(as3 bs3 rs3 ok3) (query2 (car as) '() rs (+ 1 depth))])
        ;;  ;;          (if ok3
        ;;  ;;              (values (cons (append-reverse bs3 as3) (cdr as)) bs
        ;;  ;;                  (cons 'relative (cons rs3 rs) ) #t)
        ;;  ;;              (values (cdr as) (cons (car as) bs) rs #f)
        ;;  ;;              ))]
        ;;  ;;         [else
        ;;  ;;          (values (cdr as) (cons (car as) bs) rs #f)]))

        ;;  ;; (values (cdr as) (cons (car as) bs) rs #f)


        ;;  ;; (values as bs rs #f)
        ;;  ]
        ;; [(equal? 'relative (car qs))
        ;;  (let-values ([(as2 bs2 rs2 ok2)
        ;;                (query2 (list 'branch (cons 'relative0 (cdr qs)))
        ;;                        as bs rs (+ 1 depth))])
        ;;    (if ok2
        ;;        (values as2 bs2 (cdr rs2) #t)
        ;;        (values as bs rs #f)
        ;;        ))

        ;;  ;; (values as bs rs #f)

        ;;  ]
