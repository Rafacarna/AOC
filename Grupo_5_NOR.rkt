;Exercicio de AOC
;Grupo 5 - Rafael Carnaval e Yuri Arten
;Implementar o Nor

#lang racket

(define (p-nor a b)
  (cond
    [(and (= a 0) (= b 0)) 1]
    [else 0]))

(define (p-not a)
  (p-nor a a))

(define (p-or a b)
  (p-not (p-nor a b)))

(define (p-and a b)
  (p-nor (p-not a) (p-not b)))

(define (p-xor a b)
  (let ( (c (p-nor a b)))
    (p-not (p-nor  (p-nor a c)
                   (p-nor c b)))))

(define (p-xnor a b)
  (p-not (p-xor a b)))

