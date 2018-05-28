#lang racket
(require math)
(define (powmod a b n)
  (with-modulus n
    (define (pow a b)
      (if (= b 1)
          a
          (* a (pow a (- b 1)))))
    (mod (pow a b))))
(define (powmodspecial a b m n)
  (with-modulus n
    (define (pow a b)
      (if (= b 1)
          a
          (* a (pow a (- b 1)))))
    (mod (* m (pow a b)))))
(define (getKey)
  (let* ([p (nth-prime (random 10000 20000))]
         [g (random 1 (- p 1))]
         [x (random 1 (- p 1))]
         [y (powmod g x p)])
    (list p g y x)))
(define (pubKey key)
  (reverse (cdr (reverse key))))
(define (message->int m)
  (define (helper acc m)
    (if (= (string-length m) 0)
        acc
        (let* ([lst (string->list m)]
               [c (first lst)]
               [ord (char->integer c)])
          (helper (+ ord (* 128 acc)) (substring m 1)))))
  (helper 0 m))
(define (int->message i)
  (define (helper acc i)
    (if (= i 0)
        acc
        (let* ([num (quotient i 128)]
               [rem (with-modulus 128 (mod i))]
               [c (integer->char rem)])
          (string-append (string c) acc (int->message num)))))
  (helper "" i))
(define (string-reverse s)
    (list->string (reverse (string->list s))))
(define (encrypt message key) ;encrypt. key is p g y x
  (let* ([k (random 1 1000)]
         [a (powmod (second key) k (first key))]
         [b (powmodspecial (third key) k message (first key))])
    (list a b)))
(define (decrypt pair key) ; decrypt
  (let* ([pow (- (first key) (fourth key) 1)])
    (powmodspecial (first pair) pow (second pair) (first key))))