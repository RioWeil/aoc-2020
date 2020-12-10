;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname day1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/batch-io)

;; CONSTANTS/INPUT
(define INPUT-STR (read-lines "input.txt"))
(define INPUT-NAT (map string->number INPUT-STR))

;; FUNCTIONS
(@htdf two-sumto)
(@signature (listof Natural) Natural -> (listof Natural) or false)
;; Produces list of two Naturals in lon that sum to Natural or fail.
(check-expect (two-sumto (list 1000 1020) 2020) (list 1000 1020))
(check-expect (two-sumto (list 12 500 1453 1453 2001 407 567 2) 2020)
              (list 1453 567))
(check-expect (two-sumto (list 1024 102 607 511) 2020) false)

(@template (listof Natural))

(define (two-sumto lon sum)
  (cond [(empty? lon) false]
        [else
          (local [(define try (sumto (first lon) (rest lon) sum))]
            (if (not (false? try))
                try
                (two-sumto (rest lon) sum)))]))

(@htdf sumto)
(@signature Natural (listof Natural) Natural -> (listof Natural) or false)
;; Produces listof first Nat. and Nat. in lon that sumto 2nd Nat. or fail.
(check-expect (sumto 1000 (list 1020) 2020) (list 1000 1020))
(check-expect (sumto 500 (list 1453 2001 407 567 2) 2020) false)
(check-expect (sumto 1453 (list 1453 2001 407 567 2) 2020) (list 1453 567))

(@template (listof Natural))

(define (sumto n lon sum)
  (cond [(empty? lon) false]
        [else
         (if (= (+ n (first lon)) sum)
             (list n (first lon))
             (sumto n (rest lon) sum))]))

(@htdf three-sumto)
(@signature (listof Natural) Natural -> (listof Natural))
;; Produces list of three Naturals in lon that sum to Natural or fail.
(check-expect (three-sumto (list 20 1500 500) 2020) (list 20 1500 500))
(check-expect (three-sumto (list 10 1249 591 400) 2020) false)
(check-expect (three-sumto (list 2 1501 781 249 502 2021 569 737 104) 2020)
              (list 781 502 737))

(@template (listof Natural))

(define (three-sumto lon sum)
  (cond [(empty? lon) false]
        [else
         (local [(define firstn (first lon))
                 (define diff (- sum firstn))
                 (define try (two-sumto (rest lon) diff))]
           (if (not (false? try))
               (cons firstn try)
               (three-sumto (rest lon) sum)))]))

;; SOLUTIONS

(define sol1 (foldr * 1 (two-sumto INPUT-NAT 2020)))
(define sol2 (foldr * 1 (three-sumto INPUT-NAT 2020)))