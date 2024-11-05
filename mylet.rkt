
;; (module mylet racket

  (define-for-syntax (mylet-param param-stx)
    (syntax-case param-stx ()
      [((a ...) b) #'((a ...) b)]
      [(a b) #'((a) (values b))]))

  (define-for-syntax (mylet-params params-stx)
    (datum->syntax params-stx (map mylet-param (syntax->list params-stx))))

  (define-syntax (mylet_ stx)
    (syntax-case stx ()
      [(_ n () body ...) #'(begin body ...)]
      [(_ n (a ...) body ...)
       #`(n #,(mylet-params #'(a ...)) body ...)]))

  (define-syntax mylet
    (syntax-rules ()
      [(_ a ...) (mylet_ let-values a ...)]))

  (define-syntax mylet*
    (syntax-rules ()
      [(_ a ...) (mylet_ let*-values a ...)]))

  (define-syntax myletrec
    (syntax-rules ()
      [(_ a ...) (mylet_ letrec-values a ...)]))

;; (provide mylet mylet* myletrec))




(mylet ([a 5]
        [(b c) (values 7 8)])
       (display (list a b c)))

;; (mylet () 1)
