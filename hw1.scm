#lang mzscheme
;;HI BARAK FOR QUESTION 4 I DECIDED TO USE EXPAND-ANDOR WHICH TAKES IN ANY STARTING VALUE (AND/OR) AND USES THE REQUESTED FUNCTION ---------------------
;; question 1
;;FIGURING OUT THE PREFIX WAS BY FAR THE HARDEST PART OF THIS ASSIGNMENT, AFTER USING IT AS AN API FOR BEFORE-SEQFACT TO FIND OUT IF THE SUBSEQUENCE EXISTED I THEN RECORDED THE PRECEDING VALUE IF THE SUBSEQ DID IN FACT EXIST
(define before-seq
  (lambda (xs ys)
    (before-seqfact xs ys)))

(define before-seqfact
  (lambda (xs ys)
    (if(null? ys)
       '()
       (let ((iterate (before-seqfact xs (cdr ys))))
         (if (prefix? xs (cdr ys))
             (cons  (car ys) iterate)
             iterate)))))

(define prefix?
  (lambda (xs ys)
    (or (null? xs)
        (and (not (null? ys))
             (equal? (car xs) (car ys))
             (prefix? (cdr xs) (cdr ys))))))

;;test cases

(define (Q1T1)
  (if (equal? (before-seq '(a b) '(x y z a b 1 2 3 4 a b c d a a b)) '(z 4 a))
      0
      1))
(define (Q1T2)
  (if (equal? (before-seq '(a b) '(a b c d)) '())
      0
  1))
(define (Q1T3)
  (if (equal? (before-seq '() '(j k l m n)) '(j k l m n))
      0
  1))
(define (Q1T4)
  (if (equal? (before-seq '(t) '(a b t u v t t)) '(b v t))
      0
  1))

(define (test-before-seq)
 (cons (Q1T1) (cons (Q1T2) (cons (Q1T3) (cons (Q1T4) `())))))


;; question 2 COULD NOT FIGURE OUT OR UNDERSTAND

;; question 3 USING THE NOTES AS THE GUIDELINE AND GOING LINE BY LINE WITH IT
(define cfib
  (λ (c n)
    (= c (λ (ne0)
	  (if ne0
	      (c 1)
	      (c= (λ (ne1)
		    (if ne1
			(c 1)
			(c- (λ (nm1)
			      (c- (λ (nm2)
				    (cfib (λ (fnm1)
					    (cfib (λ (fnm2)
						    (c+ c fnm1 fnm2))
						  nm2))
					  nm1))
				  n 2))
			    n 1)))
		  n 1)))
	n 0)))


(define c-ify2 (λ (f) (λ (c x y) (c (f x y)))))

(define c= (c-ify2 =))
(define c* (c-ify2 *))
(define c- (c-ify2 -))
(define c+ (c-ify2 +))

;;question 4 MOST FUN PART

;;I USED EXPAND-ANDOR AS A GATEWAY TO MY AND/OR LOOPS, I COULD'VE MADE THIS EVEN CLEANER BUT THIS SHOULD DO
;;ALL THIS LOOP DOES IS FILTER THE FIRST TWO CASES OF THE AND/OR FUNCTIONS AS STATED IN THE QUESTION E.G THE SECOND IF STATEMENT SIMPLY CHECKS IF THE SECOND ITEM ON THE LIST IS NULL, IF SO THAT MEANS THE ONLY VALUE IN THE LIST IS AND MEANING IT SHOULD PRINT OUT #t
(define expand-andor
  (λ (ls)
    (if (eqv? (car ls) 'and)
        (if(null? (cdr ls))
           #t
           (if(null? (cddr ls))
              (cdr ls)
              (expand-and(cdr ls))))
        (if (null? (cdr ls))
            #f
            (if(null? (cddr ls))
               (cdr ls)
               (cons '|cond| (expand-or(cdr ls))))))))
 
;;HERE CREATES A LIST OF ITEMS WITH THE CORRESPONDING MESSAGES MENTIONED IN THE QUESTIONS, THE FILTER CONSISTS OF A CDDR WHICH CHECKS IF THE THIRD ITEM IS NULL, IF IT ISNT NULL THEN IT PRINTS OUT (if X **recursion** 
(define expand-and
  (λ (ls)
    (cond
      ((not (null? (cddr ls))) (cons 'if (cons (car ls) (cons (expand-and (cdr ls)) '(#f)))))
      (else (cons 'if ls))
      )))

;;SAME IDEA AS ABOVE I COUDLVE USED AN IF STATEMENT HOWEVER I PREVIOUSLY HAD MORE CONDITIONS TOO LAZY TO CHANGE IT LOL
(define expand-or
  (λ (ls)
    (cond
      ((not (null? (cdr ls))) (cons (cons (car ls) '(=> (λ (x) x))) (expand-or (cdr ls))))
      (else (cons 'else ls))
      )))
;;test cases Q4T3 HAS A 1 BECAUSE IT DOESN'T MOVE ONTO TO THE NEXT LINE, I JUST DIDN'T ADD IN \n I HAD TESTED AS TO WHERE TO PUT IT BUT IT WASN'T WORKING FOR SOME STRANGE REASON SO I LEFT IT OUT

(define (Q4T1)
  (if (equal? (expand-andor '(or ONE)) '(ONE))
      0
      1))

(define (Q4T2)
  (if (equal? (expand-andor '(or)) #f)
      0
      1))

(define (Q4T3)
  (if (equal? (expand-andor '(or ONE TWO THREE)) '((ONE => (λ (x) x)) (TWO => (λ (x) x)) else THREE))
      0
      1))

(define (Q4T4)
  (if (equal? (expand-andor '(and ONE)) '(ONE))
      0
      1))

(define (Q4T5)
  (if (equal? (expand-andor '(and)) #t)
      0
      1))

(define (Q4T6)
  (if (equal? (expand-andor '(and ONE TWO THREE FOUR)) '(if ONE (if TWO (if THREE FOUR) #f) #f))
      0
      1))

(define (test-expand-andor)
 (cons (Q4T1) (cons (Q4T2) (cons (Q4T3) (cons (Q4T4) (cons (Q4T5) (cons (Q4T6) '())))))))









;;question 5
;;GROVEL ADD USES TWO FUNCTIONS ONE FOR FILTERING AND ONE FOR ITERATING THROUGH
(define grovel-add
  (lambda (p s)
    ;;SIMPLY RETURNS 0 IF THE EXP HAS NOTHING IN IT 
    (if(null? s)
       0
       (iterate p (filter s)))))
;;GOES THROUGH AND FINDS THE VALUE OF THE ATOMS IN THE EXP
(define filter
  (lambda (s)
    (if (null? s)
        null
        (if (list? (car s))
            (append (filter (car s)) (filter (cdr s)))
            (append (cons (car s) `()) (filter (cdr s)))))))
;;ITERATES THROUGH WITH FILTERED EXPR AND APPLIES RULES
(define iterate
  (lambda (p s)
    (if (null? s)
        0
        (if(number? (car s))
           (if (p (car s))
               (+ (car s) (iterate p (cdr s)))
               (+ 0 (iterate p (cdr s))))
           (iterate p (cdr s))))))


;Test Cases

(define (Q5T1)
  (if (= (grovel-add (λ (x) #t) '(a b (5 x y (z 2)))) 7)
      0
      1))

(define (Q5T2)
  (if (= (grovel-add (λ (x) (< x 4)) '(a b (5 x y (z 2)))) 2)
      0
      1))

(define (test-grovel-add)
  (cons (Q5T1) (cons (Q5T2) '())))