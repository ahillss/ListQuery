(define-syntax myflatten
  (syntax-rules (unquote)
    [(_) '()]
    [(_ (unquote (_ ...)) _ ...) (error "syntax error, cannot unquote list")]
    [(_ (unquote x) y ...) (cons (quote x) (myflatten y ...))]
    [(_ (x ...) y ...) `(,@(myflatten x ...) ,@(myflatten y ...))]
    [(_ _ y ...)   (myflatten y ...)]
    ))

;; (myflatten x (y z) w)
(myflatten x (y ,z (a (,b ,c ,d))) w)
