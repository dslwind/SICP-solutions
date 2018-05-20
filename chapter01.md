## 练习1.1
```
10
12
8
3
-16
a
b
19
#f
-1
4
16
6
16
```

## 练习1.2
```scheme
(/ (+ 5 4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
    (* 3
       (- 6 2)
       (- 2 7)))
```

## 练习1.3
```scheme
(define (max x y z)
   (cond ((and (> x y) (> x z)) x)
         ((and (> y x) (> y z)) y)
         (else z)))

(define (sum-of-max x y z)
    (max (+ x y) (+ x z) (+ y z)))
```

## 练习1.4

## 练习1.5

## 练习1.6

## 练习1.7
```scheme
(define (sqrt x)
    (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
    (if (good-enough? guess (improve guess x))
        (improve guess x)
        (sqrt-iter (improve guess x) x)))

(define (good-enough? old-guess new-guess)
    (< (/ (abs (- new-guess old-guess))
          old-guess)
       0.0001))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (abs x)
    (if (< x 0)
        (- x)
        x))
```

## 练习1.8
```scheme
(define (cube-root x)
    (cube-root-iter 1.0 x))

(define (cube-root-iter y x)
    (if (good-enough y x)
        y
        (cube-root-iter (improve y x) x)))                

(define (good-enough y x)
    (< (abs (- (cube y) x))
       0.0001))

(define (cube x)
    (* x x x))

(define (improve y x)
    (/ (+ (/ x (* y y))
          (* 2 y))
       3))

(define (abs x)
    (if (< x 0)
        (- x)
        x))
```

## 练习1.11
**递归写法**
```scheme
(define (f n)
   (if (< n 3)  
       n
       (+ (f (- n 1))      
          (f (* 2 (- n 2)))
          (f (* 3 (- n 3))))))

```
**非递归写法**
```scheme
(define (f n)
   (if (< n 3)
       n
       (iter 2 1 0 n)))

(define (iter a b c count)
    (if (< count 3)
        a
        (iter (+ a (* 2 b) (* 3 c))
              a b (- count 1))))

```

## 练习1.12
```scheme
(define (pas x y)
	(cond ((= y 0) 1)
		  ((= y x) 1)
		  (else (+ (pas (- x 1) (- y 1))
		  	       (pas (- x 1) y)))))
```

## 练习1.16
```scheme
(define (fast-expt b n)
  (expt-iter b n 1)
)

(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (square b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* a b)))
))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))
```

## 练习1.17
```scheme
(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (multi a b)
  (cond ((= b 0) 0)
        ((even? b)
          (double (multi a (halve b))))
        (else
          (+ (multi a (- b 1)) a))))

```

## 练习1.18
```scheme
(define (multi-iter a b product)
    (cond ((= b 0)
            product)
          ((even? b)
            (multi-iter (double a) (halve b) product))
          (else
            (multi-iter a (- b 1) (+ a product)))))

(define (multi a b)
    (multi-iter a b 0))
```
## 练习1.19
```scheme
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
```

## 练习1.22
```scheme
(define (search-for-primes n)
  (search-primes n 3))

(define (search-primes n count)
  (cond ((= count 0)
          (display "are primes."))
        ((prime? n)
          (display n)
          (newline)
          (search-primes (next-odd n) (- count 1)))
        (else
          (search-primes (next-odd n) count))
))

(define (next-odd n)
  (if (even? n)
      (+ n 1)
      (+ n 2)))
```

## 练习1.23
```scheme
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next divisor)
  (if (= divisor 2)
      3
      (+ divisor 2)))

(define (divides? a b)
  (= (remainder b a ) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
```

## 练习1.27
```scheme
(define (fermat-test2 n)
  (define (test a)
    (cond ((= a n) true)
          ((= (expmod a n n) a)
              (test (+ a 1)))
          (else false)))
  (test 1))
```

## 练习1.29
```scheme
(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b)
        )))

(define (cube x) (* x x x))

(define (itergral f a b n)
    (define (add2h x) (+ x (* 2 (/ (- b a) n))))
    (define (add-h x) (+ x (/ (- b a) n)))
    (/ (* (/ (- b a) n)
          (+ (f a) (f b)
            (* 4 (sum f (add-h a) add2h b))
            (* 2 (sum f (add2h a) add2h b))
          )
        )
        3.0
    )
)
```

## 练习1.30
```scheme
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ (term a) result))))
  (iter a 0))
```

## 练习1.31
**product迭代写法**
```scheme
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* (term a) result))))
  (iter a 1))
```
**product递归写法**
```scheme
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
```
**factorial**
```scheme
(define (factorial n)
  (product id 1 inc n))

(define (id x) x)

(define (inc x) (+ x 1))
```
**PI的计算**
```scheme
(define (square-of-odd x)
  (square (+ (* x 2) 1.0)))

(define (pi-john-wallis n)
  (define (pi-term x)
    (/ (- (square-of-odd x) 1)
       (square-of-odd x)))
  (product pi-term 1 inc n))
```

## 练习1.32
```scheme
;;;递归写法
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
        (term a)
        (accumulate combiner null-value term (next a) next b))))

;;;迭代写法
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate multi 1 term a next b))
```

## 练习1.33
```scheme
(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a)
            (combiner
              (term a)
              (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value term (next a) next b))
  ))

(define (sum-of-prime a b)
  (filtered-accumulate prime? + 0 id a inc b))
```
