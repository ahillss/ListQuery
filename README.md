####List Query

A query language written in Scheme for querying lists (e.g. trees, SXML).

Only backend implemented so far.

######Similar to the Scheme "match" macro to perform queries on lists:
```scheme
(define m 
  '(html (body
          (h1 "example")
          (a (@ (href "link1.com")) "link1 desc")
          (br)
          (a (@ (href "link2.com")) "link2 desc"))))
		  
(query-match m
  ((html (body (a (@ (href ,x)) ,y) ...))
   (list x y))
  (else #f))
````
=> `'(("link1.com" "link1 desc") ("link2.com" "link2 desc"))`

######Predicates:
```scheme
(define m '((val 5) (val 120) (val 33) (val 3) (val 65)))

(query-match m
  (((val (? > ,x 20)) ...)
   x)
  (else #f))
```
=> `'(120 33 65)`

######Recursive search, like a normal search except when no match found, it will recursively run on the sublists:
```scheme
(define m
  '(html (body
          (p (a (@ (href "q.com") "q")) (br) (a (@ (href "r.com") "r"))) 
          (div (a (@ (href "s.com") "s"))
               (a (@ (href "t.com") "t" (div (@ (name "aa") 111))))
               (div (@ (name "bb") 222))
               (div (@ (name "cc") 333 (div (@ (name "dd") 444))))
               (div (@ (name "ee") (p 5) (p 6)))))))

(query-match m
  ( (^(a (@ (href ,x))) ...
     ^(div (@ (name ,y) z ...)) ...)
   (list x y z) )
  (else #f))
```
=>
```
'( ("q.com" "r.com" "s.com" "t.com")
   ("aa" "bb" "cc" "ee") 
   ((111)
    (222)
    (333 (div (@ (name "dd") 444)))
    ((p 5) (p 6))) )
```

#####TODO:
* match macro for frontend
* look at using 'joins' (SQL like)
* look at using backtracking (PROLOG like)

<!--- TODO

SQL like "joins":
```scheme
(query-match '(html (body
				(h1 "example")
				(p (h2 "Colors")
					(ul (li (strong "joe") "green")
					(li (strong "bob") "red")
					(li (strong "wanda") "purple")))
				(p (h2 "Hobbies")
					(ul (li (strong "jenny") "hiking")
					(li (strong "wanda") "swimming")
					(li (strong "bob") "boating")))))
	((html (body 
			(p (h2 "Colors") (ul (li (strong ,x) ,y) ...)) 
			(p (h2 "Hobbies") (ul (li (strong ,x) ,z) ...))))
	  (list x y z))
	(else #f))
```
=> `'(("bob" "wanda") ("red" "purple") ("boating" "swimming"))`

Another SQL like "joins":
```scheme
(query-match '(html (body
				(h1 "example")
				(p (h2 "Colors")
					(ul (li (strong "joe") "green")
					(li (strong "bob") "red")
					(li (strong "wanda") "purple")))
				(p (h2 "Hobbies")
					(ul (li (strong "jenny") "hiking")
					(li (strong "wanda") "swimming")
					(li (strong "bob") "boating")
					(li (strong "bob") "fishing")))))
	((html (body 
			(p (h2 "Colors") (ul (li (strong ,x) ,y) ...)) 
			(p (h2 "Hobbies") (ul (li (strong ,x) ,z) ...))))
	  (list x y))
	(else #f))
```
=> `'(("bob" "wanda") ("red" "purple") ("boating" "swimming"))`






-->