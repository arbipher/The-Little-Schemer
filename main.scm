;Ch1

(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))))

;Ch2

(define lat?
  (lambda (l)
  	(cond
  		((null? l) #t)
        ((atom? (car l)) (lat? (cdr l)))
    	(else #f))))

(define member?
  (lambda (a lat)
  	(cond
  		((null? lat) #f)
  		(else (or (eq? (car lat) a)
  				(member? a (cdr lat)))))))

;Ch3

;(define rember
;  (lambda (a lat)
;  	(cond
;  		((null? lat) '())
;  		((eq? a (car lat)) (cdr lat))
;  		(else (cons (car lat) 
;  		(rember a (cdr lat)))))))

(define firsts
  (lambda (l)
  	(cond
  		((null? l) '())
  		(else (cons (car (car l))
  						(firsts (cdr l)))))))

;(define insertR
;	(lambda (new old lat)
;		(cond
;			((null? lat) '())
;			((eq? (car lat) old) (cons old (cons new (cdr lat))))
;			(else (cons (car lat)
;							(insertR new old (cdr lat))))
;		)
;	)
;)

;(define insertL
;	(lambda (new old lat)
;		(cond
;			((null? lat) '())
;			((eq? (car lat) old) (cons new lat))
;			(else (cons (car lat)
;							(insertL new old (cdr lat))))
;		)
;	)
;)

;(define subst
;	(lambda (new old lat)
;		(cond
;			((null? lat) '())
;			((eq? (car lat) old) (cons new (cdr lat)))
;			(else (cons (car lat)
;							(subst new old (cdr lat))))
;		)
;	)
;)

(define subst2
	(lambda (new o1 o2 lat)
		(cond
			((null? lat) '())
			((or (eq? (car lat) o1) (eq? (car lat) o2))
				(cons new (cdr lat)))
			(else (cons (car lat)
							(subst2 new o1 o2 (cdr lat))))
		)
	)
)

(define multirember
  (lambda (a lat)
  	(cond
  		((null? lat) '())
  		((eq? a (car lat)) (multirember a (cdr lat)))
  		(else (cons (car lat) 
  			(multirember a (cdr lat)))))))

(define multiinsertR
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
			(else (cons (car lat)
							(multiinsertR new old (cdr lat))))
		)
	)
)

(define multiinsertL
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
			(else (cons (car lat)
							(multiinsertL new old (cdr lat))))
		)
	)
)

(define multisubst
	(lambda (new old lat)
		(cond
			((null? lat) '())
			((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
			(else (cons (car lat)
							(multisubst new old (cdr lat))))
		)
	)
)

;Ch4
(define add1
	(lambda (n)
		(+ n 1)
	)
)

(define sub1
	(lambda (n)
		(- n 1)
	)
)

(define n+
	(lambda (n m)
		(cond
			((zero? m) n)
			(else (add1 (n+ n (sub1 m))))
		)
	)
)

(define n-
	(lambda (n m)
		(cond
			((zero? m) n)
			(else (sub1 (n- n (sub1 m))))
		)
	)
)

;typo in the book 4th edition; three ) needed

(define addtup
	(lambda (tup)
		(cond
			((null? tup) 0)
			(else (n+ (car tup) (addtup (cdr tup))))
		)
	)
)

(define n*
	(lambda (n m)
		(cond
			((zero? m) 0)
			(else (n+ n (n* n (sub1 m))))
		)
	)
)

(define tup+
	(lambda (tup1 tup2)
		(cond
			((null? tup1) tup2)
			((null? tup2) tup1)
			(else
				(cons (n+ (car tup1) (car tup2))
					(tup+
						(cdr tup1) (cdr tup2)
					)
				)
			)
		)
	)
)

(define n>
	(lambda (n m)
		(cond
			((zero? n) #f)
			((zero? m) #t)
			(else (n> (sub1 n) (sub1 m)))
		)
	)
)

(define n<
	(lambda (n m)
		(cond
			((zero? m) #f)
			((zero? n) #t)
			(else (n< (sub1 n) (sub1 m)))
		)
	)
)

;(define n=
;  (lambda (n m)
;  	(cond
;  		((zero? m) (zero? n))
;  		((zero? n) #f)
;  		(else (= (sub1 n) (sub1 m)))
;  	)
;  )
;)

(define n=
	(lambda (n m)
		(cond
			((> n m) #f)
			((< n m) #f)
			(else #t)
		)
	)
)

(define np
	(lambda (n m)
		(cond
			((zero? m) 1)
			(else (n* n (np n (sub1 m))))
		)
	)
)

(define n/
	(lambda (n m)
		(cond
			((< n m) 0)
			(else (add1 (n/ (n- n m) m)))
		)
	)
)

(define pick
	(lambda (n lat)
		(cond
			((zero? (sub1 n)) (car lat))
			(else (pick (sub1 n) (cdr lat)))
		)
	)
)

(define no-nums
	(lambda (lat)
		(cond
			((null? lat) '())
			((number? (car lat)) (no-nums (cdr lat)))
			(else (cons (car lat) (no-nums (cdr lat))))
		)
	)
)

(define all-nums
	(lambda (lat)
		(cond
			((null? lat) '())
			((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
			(else (all-nums (cdr lat)))
		)
	)
)

(define eqan?
	(lambda (a1 a2)
		(cond
			((and (number? a1) (number? a2))
				(= a1 a2))
			((or (number? a1) (number? a2))
				#f)
			(else (eq? a1 a2))
		)
	)
)

(define occur
	(lambda (a lat)
		(cond
			((null? lat) 0)
			((eq? a (car lat)) (add1 (occur a (cdr lat))))
			(else (occur a (cdr lat)))
		)
	)
)

(define one?
	(lambda (n)
		(= n 1)
	)
)

(define rempick
	(lambda (n lat)
		(cond
			((one? n) (cdr lat))
			(else (cons (car lat) (rempick (sub1 n) (cdr lat))))
		)
	)
)

;Ch5

(define rember*
	(lambda (a l)
		(cond
			((null? l) '())
			((atom? (car l))
				(cond
					((eq? a (car l)) (rember* a (cdr l)))
					(else (cons (car l) (rember* a (cdr l))))
				)
			)
			(else (cons (rember* a (car l)) (rember* a (cdr l))))
		)
	)
)

(define insertR*
	(lambda (new old l)
		(cond
			((null? l) '())
			((atom? (car l))
				(cond
					((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))) )
					(else (cons (car l) (insertR* new old (cdr l))))
				)
			)
			(else (cons (insertR* new old (car l))
							(insertR* new old (cdr l))))
		)
	)
)

(define occur*
	(lambda (a l)
		(cond
			((null? l) 0)
			((atom? (car l))
				(cond
					((eq? a (car l)) (add1 (occur* a (cdr l))) )
					(else (occur* a (cdr l)))
				)
			)
			(else (n+ (occur* a (car l))
								(occur* a (cdr l))))
		)
	)
)

(define subst*
	(lambda (new old l)
		(cond
			((null? l) '())
			((atom? (car l)) 
				(cond
					((eq? old (car l)) (cons new (subst* new old (cdr l))))
					(else (cons (car l) (subst* new old (cdr l))))
				)
			)
			(else (cons (subst* new old (car l)) 
									(subst* new old (cdr l))))
		)
	)
)

(define insertL*
	(lambda (new old l)
		(cond
			((null? l) '())
			((atom? (car l))
				(cond
					((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))) )
					(else (cons (car l) (insertL* new old (cdr l))))
				)
			)
			(else (cons (insertL* new old (car l))
							(insertL* new old (cdr l))))
		)
	)
)

(define member*
	(lambda (a l)
		(cond
			((null? l) #f)
			((atom? (car l))
				(or
					(eq? a (car l))
					(member* a (cdr l))
				)
			)
			(else (or (member* a (car l))
								(member* a (cdr l))))
		)
	)
)

(define leftmost
	(lambda (l)
		(cond
			((atom? (car l)) (car l))
			(else (leftmost (car l)))
		)
	)
)

(define eqlist?
	(lambda (l1 l2)
		(cond
			((and (null? l1) (null? l2)) #t)
			((or (null? l1) (null? l2)) #f)
			((and (atom? (car l1)) (atom? (car l2)))
				(and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
			((or (atom? (car l1)) (atom? (car l2))) #f)
			(else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
		)
	)
)

(define equal?
	(lambda (s1 s2)
		(cond
			((and (atom? s1) (atom? s2))
				(eqan? s1 s2))
			((or (atom? s1) (atom? s2)) #f)
			(else (eqlist? s1 s2))
		)
	)
)

(define rember
  (lambda (s l)
  	(cond
  		((null? l) '())
  		((equal? s (car l)) (cdr l))
  		(else (cons (car l) 
  								(rember s (cdr l)))))))

;Ch6

(define numbered?
	(lambda (aexp)
		(cond
			((atom? aexp) (number? aexp))
			(else
				(and (numbered? (car aexp))
							(numbered? (car (cdr (cdr aexp)))))
			)
		)
	)
)

(define 1st-sub-exp
	(lambda (aexp)
		(car (cdr aexp))
	)
)

(define 2nd-sub-exp
	(lambda (aexp)
		(car(cdr (cdr aexp)))
	)
)

(define operator
	(lambda (aexp)
		(car aexp)
	)
)

;(define value
;	(lambda (nexp)
;		(cond
;			((atom? nexp) nexp)
;			((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp))
;																		(value (2nd-sub-exp nexp))))
;			((eq? (operator nexp) '*) (* (value (1st-sub-exp nexp))
;																		(value (2nd-sub-exp nexp))))
;			((eq? (operator nexp) '^) (np (value (1st-sub-exp nexp))
;																		(value (2nd-sub-exp nexp))))
;			(else 0)
;		)
;	)
;)

(define sero?
	(lambda (n)
		(null? n)
	)
)

(define edd1
	(lambda (n)
		(cons '() n)
	)
)

(define zub1
	(lambda (n)
		(cdr n)
	)
)

(define l+
	(lambda (n m)
		(cond
			((sero? m) n)
			(else (edd1 (l+ n (zub1 m))))
		)
	)
)

;Ch7

(define set?
	(lambda (lat)
		(cond
			((null? lat) #t)
			((member? (car lat) (cdr lat)) #f)
			(else (set? (cdr lat)))
		)
	)
)

(define makeset
	(lambda (lat)
		(cond
			((null? lat) '())
			((member? (car lat) (cdr lat)) (makeset (cdr lat)))
			(else (cons (car lat) 
									(makeset
										(multirember (car lat) (cdr lat)))))
		)
	)
)

(define subset?
	(lambda (set1 set2)
		(cond
			((null? set1) #t)
			((member? (car set1) set2)
				(subset? (cdr set1) set2))
			(else #f)
		)
	)
)

(define eqset?
	(lambda (set1 set2)
		(and
			(subset? set1 set2)
			(subset? set2 set1)
		)
	)
)

(define intersect?
	(lambda (set1 set2)
		(cond
			((null? set1) #f)
			(else (or 
				(member? (car set1) set2)
				(intersect? (cdr set1) set2))
			)
		)
	)
)

(define intersect
	(lambda (set1 set2)
		(cond
			((null? set1) '())
			((member? (car set1) set2)
				(cons (car set1) (intersect (cdr set1) set2)))
			(else (intersect (cdr set1) set2))
		)
	)
)

(define union
	(lambda (set1 set2)
		(cond
			((null? set1) set2)
			((member? (car set1) set2)
				(union (cdr set1) set2))
			(else (cons (car set1) (union (cdr set1) set2)))
		)
	)
)

(define difference
	(lambda (set1 set2)
		(cond
			((null? set1) '())
			((member? (car set1) set2)
				(difference (cdr set1) set2))
			(else (cons (car set1)
									(difference (cdr set1) set2)))
		)
	)
)

(define intersectall
	(lambda (l-set)
		(cond
			((null? (cdr l-set)) (car l-set))
			(else (intersect (car l-set) (intersectall (cdr l-set))))
		)
	)
)

(define a-pair?
	(lambda (x)
		(cond
			((null? x) #f)
			((atom? x) #f)
			((null? (cdr x)) #f)
			((null? (cdr (cdr x))) #t)
			(else #f)
		)
	)
)

(define first
	(lambda (p)
		(car p)
	)
)

(define second
	(lambda (p)
		(car (cdr p))
	)
)

(define build
	(lambda (s1 s2)
		(cons s1 (cons s2 '()))
	)
)

(define fun?
	(lambda (rel)
		(set? (firsts rel))
	)
)

(define revpair
	(lambda (pair)
		(build (second pair) (first pair))
	)
)

(define revrel
	(lambda (rel)
		(cond
			((null? rel) '())
			(else (cons (revpair (car rel))
									(revrel (cdr rel))))
		)
	)
)

;(define seconds
;  (lambda (l)
;  	(cond
;  		((null? l) '())
;  		(else (cons (car (cdr (car l)))
;  						(seconds (cdr l)))))))

;(define fullfun?
;	(lambda (fun)
;		(set? (seconds rel))
;	)
;)

(define one-to-one?
	(lambda (fun)
		(fun? (revrel fun))
	)
)

;Ch8

(define rember-f
	(lambda (test?)
		(lambda (a l)
			(cond
				((null? l) '())
				((test? a (car l)) (cdr l))
				(else (cons (car l)
										((rember-f test?) a (cdr l))))
			)
		)
	)
)

(define rember-eq? (rember-f eq?))

(define eq?-c
	(lambda (a)
		(lambda (x)
			(eq? x a)
		)
	)
)


(define insertL-f
	(lambda (test?)
		(lambda (new old lat)
			(cond
				((null? lat) '())
				((test? (car lat) old) (cons new lat))
				(else (cons (car lat)
								((insertL-f test?) new old (cdr lat))))
			)
		)
	)
)

(define insertR-f
	(lambda (test?)
		(lambda (new old lat)
			(cond
				((null? lat) '())
				((eq? (car lat) old) (cons old (cons new (cdr lat))))
				(else (cons (car lat)
								((insertR-f test?) new old (cdr lat))))
			)
		)
	)
)

(define seqL
	(lambda (new old l)
		(cons new (cons old l))
	)
)

(define seqR
	(lambda (new old l)
		(cons old (cons new l))
	)
)

(define insert-g
	(lambda (seq)
		(lambda (new old lat)
			(cond
				((null? lat) '())
				((eq? (car lat) old) (seq new old (cdr lat)))
				(else (cons (car lat)
								((insert-g seq) new old (cdr lat))))
			)
		)
	)
)

(define insertL (insert-g seqL))

(define insertR 
	(insert-g 
		(lambda (new old l)
			(cons old (cons new l))
		)
	)
)

(define seqS
	(lambda (new old l)
		(cons new l)
	)
)

(define subst (insert-g seqS))

(define atom-to-function
	(lambda (x)
		(cond
			((eq? x '+) n+)
			((eq? x '*) n*)
			(else np)
		)
	)
)

;(define value
;	(lambda (nexp)
;		(cond
;			((atom? nexp) nexp)
;			(else
;				((atom-to-function (operator nexp))
;					(value (1st-sub-exp nexp))
;					(value (2nd-sub-exp nexp))
;				)
;			)
;		)
;	)
;)

(define multirember-f
	(lambda (test?)
		(lambda (a lat)
  	(cond
  		((null? lat) '())
  		((eq? a (car lat)) 
  			((multirember-f test?) a (cdr lat)))
  		(else (cons (car lat) 
  			((multirember-f test?) a (cdr lat))))))
	)
)

(define multirember-eq?
	(multirember-f eq?)
)

(define multiremberT
	(lambda (test? lat)
  	(cond
  		((null? lat) '())
  		((test? (car lat)) 
  			((multiremberT test?) a (cdr lat)))
  		(else (cons (car lat) 
  			((multiremberT test?) a (cdr lat)))))
  )
)

(define new-friend
	(lambda (newlat seen)
		(col newlat
			(cons (car lat) seen)
		)
	)
)

(define latest-friend
	(lambda (newlat seen)
		(a-friend (cons 'and newlat) seen)
	)
)

(define new-friend
	(lambda (newlat seen)
		(col newlat
			(cons (car lat) seen))
	)
)

(define multirember&co
	(lambda (a lat col)
		(cond
			((null? lat)
				(col '() '()))
			((eq? a (car lat))
				(multirember&co a
					(cdr lat)
					(lambda (newlat seen)
						(col newlat (cons (car lat) seen))))
			)
			(else 
				(multirember&co a
					(cdr lat)
					(lambda (newlat seen)
						(col (cons (car lat) newlat) seen))
				)
			)
		)
	)
)

(define a-friend
	(lambda (x y)
		(null? y)
	)
)

(define multiinsertLR
	(lambda (new oldL oldR lat)
		(cond
			((null? lat) '())
			((eq? (car lat) oldL) 
				(cons new 
					(cons oldL
						(multiinsertLR new oldL oldR (cdr lat)))))
			((eq? (car lat) oldR) 
				(cons oldR
					(cons new
						(multiinsertLR new oldL oldR (cdr lat)))))
			(else (cons (car lat)
							(multiinsertLR new oldL oldR (cdr lat))))
		)
	)
)

(define multiinsertLR&co
	(lambda (new oldL oldR lat col)
		(cond
			((null? lat)
				(col '() 0 0))
			((eq? (car lat) oldL) 
				(multiinsertLR&co new oldL oldR 
					(cdr lat)
					(lambda (newlat L R)
						(col (cons new (cons oldL newlat)) 
							(add1 L) R))))
			((eq? (car lat) oldR) 
				(multiinsertLR&co new oldL oldR 
					(cdr lat)
					(lambda (newlat L R)
						(col (cons oldR (cons new newlat)) 
							L (add1 R)))))
			(else (cons (car lat)
							(multiinsertLR new oldL oldR (cdr lat))))
		)
	)
)

(define evens-only*
	(lambda (l)
		(cond
			((null? l) '())
			((atom? (car l))
				(cond
					((even? (car l))
						(cons (car l)
							(evens-only* (cdr l))))
					(else (evens-only* (cdr l)))))
			(else (cons (evens-only* (car l))
							(evens-only* (cdr l))))
		)
	)
)

(define evens-only&co
	(lambda (l col)
		(cond
			((null? l)
				(col '() 1 0))
			((atom? (car l))
				(cond
					((even? (car l))
						(evens-only&co (cdr l)
							(lambda (newl p s)
								(col (cons (car l) newl)
									(* (car l) p) s))))
					(else (evens-only&co (cdr l)
							(lambda (newl p s)
								(col newl
									p (+ (car l) s)))))))
			(else (evens-only&co (car l)
							(lambda (al ap as)
								(evens-only&co (cdr l)
									(lambda (dl dp ds)
										(col (cons al dl)
											(* ap dp)
											(+ as ds)))))))
		)
	)
)

(define appd-p-s
	(lambda (newl p s)
		(cons s
			(cons p newl)
		)
	)
)

;Ch9

(define keep-looking
	(lambda (a sorn lat)
		(cond
			((number? sorn)
				(keep-looking a (pick sorn lat) lat))
			(else (eq? sorn a))
		)
	)
)

(define looking
	(lambda (a lat)
		(keep-looking a (pick 1 lat) lat)
	)
)

(define shift
	(lambda (pair)
		(build (first (first pair))
			(build (second (first pair))
				(second pair)
			)
		)
	)
)

(define align
	(lambda (pora)
		(cond
			((atom? pora) pora)
			((a-pair? (first pora))
				(align (shift pora)))
			(else (build (first pora)
							(align (second pora))))
		)
	)
)

(define length*
	(lambda (pora)
		(cond
			((atom? pora) 1)
			(else
				(+ (length* (first pora))
						(length* (second pora))
				)
			)
		)
	)
)

(define weight*
	(lambda (pora)
		(cond
			((atom? pora) 1)
			(else
				(+ (* (weight* (first pora)) 2)
						(weight* (second pora)))
			)
		)
	)
)

(define shuffle
	(lambda (pora)
		(cond
			((atom? pora) pora)
			((a-pair? (first pora))
				(shuffle (revpair pora)))
			(else (build (first pora)
							(shuffle (second pora))))
		)
	)
)

(define C
	(lambda (n)
		(cond
			((one? n) 1)
			((even? n) (C (/ n 2)))
			(else (C (+ 1 (* 3 n))))
		)
	)
)

(define A
	(lambda (n m)
		(cond
			((zero? n) (add1 m))
			((zero? m) (A (sub1 n) 1))
			(else (A (sub1 n) (A n (sub1 m))))
		)
	)
)

;(define will-stop?
;	(lambda (f)
;	)
;)

(define eternity
	(lambda (x)
		(eternity x)
	)
)

(define last-try
	(lambda (x)
		(and (will-stop? last-try)
			(eternity x)
		)
	)
)

;(will-stop? last-try)
(define length
	(lambda (l)
		(cond
			((null? l) 0)
			(else (add1 (length (cdr l))))
		)
	)
)

;length<=1
((lambda (l)
	(cond
		((null? l) 0)
		(else 
			(add1 
				((lambda (l)
					(cond
						((null? l) 0)
						(else (add1 
										(eternity (cdr l))))))
				(cdr l)))))
	) '(1))

;length_0
((lambda (length)
	(lambda (l)
		(cond
			((null? l) 0)
			(else (add1 (length (cdr l))))
		)
	))
	eternity)

(
	((lambda (length)
	(lambda (l)
		(cond
			((null? l) 0)
			(else (add1 (length (cdr l)))))))
	((lambda (length)
	(lambda (l)
		(cond
			((null? l) 0)
			(else (add1 (length (cdr l)))))))
	((lambda (length)
	(lambda (l)
		(cond
			((null? l) 0)
			(else (add1 (length (cdr l)))))))
	((lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l)))))))
		eternity
	))))
	'(1 2 3)
)

;length_0
(((lambda (mk-length)
	(mk-length eternity))
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))))))
	'()
)

;length_le_1
(((lambda (mk-length)
	(mk-length
		(mk-length eternity)))
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))))))
	'(1)
)

;length_le_2
(((lambda (mk-length)
	(mk-length
		(mk-length
			(mk-length eternity))))
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))))))
	'(1 2)
)

(((lambda (mk-length)
	(mk-length mk-length))
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (length (cdr l))))))))
	'()
)


(((lambda (mk-length)
	(mk-length mk-length))
	(lambda (mk-length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (add1 (mk-length (cdr l))))))))
	'()
)

;last correct version
(((lambda (mk-length)
	(mk-length mk-length))
	(lambda (mk-length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (+ 1 ((mk-length mk-length) (cdr l)))))))
	)
	'(1 2 3)
)

;wrong_version
;(((lambda (mk-length)
;	(mk-length mk-length))
;		(lambda (mk-length)
;			((lambda (length)
;					(lambda (l)
;						(cond
;							((null? l) 0)
;							(else (+ 1 (length (cdr l)))))))
;				(mk-length mk-length))))
;	'()
;)

(((lambda (mk-length)
	(mk-length mk-length))
	(lambda (mk-length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else (+ 1 
								((lambda (x)
									((mk-length mk-length) x))
									(cdr l))
	))))))
	'(1 2)
)

(((lambda (mk-length)
	(mk-length mk-length))
	(lambda (mk-length)
		((lambda (length)
			(lambda (l)
				(cond
					((null? l) 0)
					(else 
						(add1 (length (cdr l)))))))
			(lambda (x)
				((mk-length mk-length) x)))))
	'(1 2 3)
)

(((lambda (le)
		((lambda (mk-length)
		(mk-length mk-length))
		(lambda (mk-length)
			(le
				(lambda (x)
					((mk-length mk-length) x))))))
	(lambda (length)
		(lambda (l)
			(cond
				((null? l) 0)
				(else 
					(add1 (length (cdr l))))))))
	'(1 2 3 4 5)
)

(define Y
	(lambda (le)
		((lambda (f) (f f))
			(lambda (f)
				(le (lambda (x) ((f f) x)))))))

((Y (lambda (length)
	(lambda (l)
		(cond
			((null? l) 0)
			(else 
				(add1 (length (cdr l))))))))
	'(1 2 3 4 5 6 7))

;Ch10
(define lookup-in-entry-help
	(lambda (name names values entry-f)
		(cond
			((null? names) (entry-f name))
			((eq? name (car names)) 
				(car values))
			(else (lookup-in-entry-help name (cdr names) (cdr values) entry-f))
		)
	)
)

(define lookup-in-entry
	(lambda (name entry entry-f)
		(lookup-in-entry-help name
			(first entry)
			(second entry)
			entry-f
		)
	)
)

(define extend-table cons)

(define lookup-in-table
	(lambda (name table table-f)
		(cond
			((null? table) 
				(table-f name))
			(else 
				(lookup-in-entry name
					(car table)
					(lambda (name)
						(lookup-in-table name
							(cdr table)
							table-f)))))))

(lookup-in-table 
	'a 
	'(((a) (b)) ((c) (d)))
	(lambda (name) 1)
)

(cons 'car
	(cons (cons 'quote
			(cons
				(cons 'a
					(cons 'b
						(cons 'c '())))
				'()))
	'()
))

(define atom-to-action
	(lambda (e)
		(cond
			((number? e) *const)
			((eq? e #t) *const)
			((eq? e #f) *const)
			((eq? e 'cons) *const)
			((eq? e 'car) *const)
			((eq? e 'cdr) *const)
			((eq? e 'null) *const)
			((eq? e 'eq?) *const)
			((eq? e 'atom?) *const)
			((eq? e 'zero?) *const)
			((eq? e 'add1) *const)
			((eq? e 'sub1) *const)
			((eq? e 'number?) *const)
			(else *identifier)
		)
	)
)

(define *const
	(lambda (e table)
		(cond
			((number? e) e)
			((eq? e #t) #t)
			((eq? e #f) #f)
			(else (build 'primitive e))
		)
	)
)

(define *quote
	(lambda (e table)
		(text-of e)))

(define text-of second)

(define *identifier
	(lambda (e table)
		(lookup-in-table e table initial-table)))

(define initial-table
	(lambda (name)
		(car '())
	)
)

(define *lambda
	(lambda (e table)
		(build 'non-primitive
			(cons table (cdr e)))))

(define else?
	(lambda (x)
		(cond
			((atom? x) (eq? x 'else))
			(else #f))))

(define question-of first)

(define answer-of second)

(define expression-to-action
	(lambda (e)
		(cond
			((atom? e) (atom-to-action e))
			(else (list-to-action e))
		)
	)
)

(define meaning
	(lambda (e table)
		((expression-to-action e) e table)))

(define value
	(lambda (e)
		(meaning e '()))
)

(define evcon
	(lambda (lines table)
		(cond
			((else? (question-of (car lines)))
				(meaning (answer-of (car lines)) table))
			((meaning (question-of (car lines)) table)
				(meaning (answer-of (car lines)) table))
			(else (evcon (cdr lines) table)))))

(define cond-lines-of cdr)

(define *cond
	(lambda (e table)
		(evcon (cond-lines-of e) table)))

(*cond 
	'(cond (coffee klatsch) (else party))
	'(((coffee) (#t)) ((klatsch party) (5 (6))))
)

(define table-of first)

(define formals-of second)

(define body-of third)

(define list-to-action
	(lambda (e)
		(cond
			((atom? (car e))
				(cond
					((eq? (car e) 'quote) *quote)
					((eq? (car e) 'lambda) *lambda)
					((eq? (car e) 'cond) *cond)
					(else *application)))
			(else *application)
		)
	)
)

(define evlis
	(lambda (args table)
		(cond
			((null? args) '())
			(else 
				(cons (meaning (car args) table)
					(evlis (cdr args) table))))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
	(lambda (l)
		(eq? (first l) 'primitive)))

(define non-primitive?
	(lambda (l)
		(eq? (first l) 'non-primitive)))

(define *application
	(lambda (e table)
		(apply 
			(meaning (function-of e) table)
			(evlis (arguments-of e) table))))

(define apply
	(lambda (fun vals)
		(cond
			((primitive? fun)
				(apply-primitive (second fun) vals))
			((non-pritimitive? fun)
				(apply-closure (second fun) vals)))))

(define :atom?
	(lambda (x)
		(cond
			((atom? x) #t)
			((null? x) #f)
			((eq? (car x) 'primitive) #t)
			((eq? (car x) 'non-primitive) #t)
			(else #f))))

(define apply-primitive
	(lambda (name vals)
		(cond
			((eq? name 'cons)
				(cons (first vals) (second vals)))
			((eq? name 'car)
				(car (first vals)))
			((eq? name 'cdr)
				(cdr (first vals)))
			((eq? name 'null?)
				(null? (first vals)))
			((eq? name 'eq?)
				(eq? (first vals) (second vals)))
			((eq? name 'atom?)
				(:atom? (first vals)))
			((eq? name 'zero?)
				(zero? (first vals)))
			((eq? name 'add1)
				(add1 (first vals)))
			((eq? name 'sub1)
				(sub1 (first vals)))
			((eq? name 'number?)
				(number? (first vals))))))

(define new-entry build)

(define apply-closure
	(lambda (closure vals)
		(meaning (body-of closure)
			(extend-table
				(new-entry
					(formals-of closure)
					vals)
					(table-of closure)))))

(apply-closure
	'(
			(
				(
					(u v w)
					(1 2 3)
				)
				(
					(x y z)
					(4 5 6)
				)
			)
			(x y)
			(cons z x)
		)
	'(
		(a b c)
		(d e f)
	)
)
