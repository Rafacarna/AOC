;Exercicio de AOC
;Grupo 5 - Rafael Carnaval e Yuri Arten
;Implementar portas logicas com 16 entradas

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

(define (p-mux in1 in2 sel)
  (p-or (p-and (p-not sel) in1) (p-and sel in2)))

(define (p-demux in sel)
  (list (p-and (p-not sel) in) (p-and sel in)))

(define (p-not-16 a)
  (for/list ( (c '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))) ;;Listando uma lista de
                                                            ;;16 argumentos em um loop
                                                            ;;usando for/list do racket.
    
    (p-not (list-ref a c))))                                ;;Usando a funçao p-not ,
                                                            ;;ja criado anteriormente ,
                                                            ;;fazemos uma recursao
                                                            ;;para ajudar o processo.
                                                            ;;Usando a list-ref
                                                            ;;temos controle sobra a lista

(define (p-and-16 a b)
  (for/list ( (c '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))) ;;Mesmo processo da anterior
                                                            ;;e das demais funçoes abaixo
    (p-and (list-ref a c) (list-ref b c))))

(define (p-or-16 a b)
  (for/list ( (c '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
    (p-or (list-ref a c) (list-ref b c))))

(define (p-xor-16 a b)
  (for/list ( (c '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
    (p-xor (list-ref a c) (list-ref b c))))

(define (p-xnor-16 a b)
  (for/list ( (c '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
    (p-xnor (list-ref a c) (list-ref b c))))

(define (p-mux-16 in1 in2 sel)
  (for/list ( (c '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
    (p-mux (list-ref in1 c) (list-ref in2 c) sel)))

(define (p-demux-16 in1 in2 sel)
  (for/list ( (c '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
    (p-demux (list-ref in1 c) (list-ref in2 c) sel)))