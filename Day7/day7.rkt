;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname day7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/batch-io)
(require racket/string)

;; CONSTANTS/INPUT
(define INPUT (read-lines "input.txt"))


;; DATA DEFINITIONS
(@htdd Bag)
(define-struct bag (colour subs))
;; Bag is (make-bag String (listof String))
;; interp. is a bag with colour specified by string
;;              subs are colours of bags contained in the bag.
(define B1 (make-bag "red" empty))
(define B2 (make-bag "blue" (list "orange")))
(define B3 (make-bag "green" (list "red" "blue")))
(define B4 (make-bag "orange" (list "green" "red")))

(define TEST-BAG-NETWORK
  (list (make-bag "light red" (list "bright white"
                                    "muted yellow"
                                    "muted yellow"))
        (make-bag "dark orange" (list "bright white"
                                      "bright white"
                                      "bright white"
                                      "muted yellow"
                                      "muted yellow"
                                      "muted yellow"
                                      "muted yellow"))
        (make-bag "bright white" (list "shiny gold"))
        (make-bag "muted yellow" (list "shiny gold"
                                       "shiny gold"
                                       "faded blue"
                                       "faded blue"
                                       "faded blue"
                                       "faded blue"
                                       "faded blue"
                                       "faded blue"
                                       "faded blue"
                                       "faded blue"))
        (make-bag "shiny gold" (list "dark olive"
                                     "vibrant plum"
                                     "vibrant plum"))
        (make-bag "dark olive" (list "faded blue"
                                     "faded blue"
                                     "faded blue"
                                     "dotted black"
                                     "dotted black"
                                     "dotted black"
                                     "dotted black"))
        (make-bag "vibrant plum" (list "faded blue"
                                       "faded blue"
                                       "faded blue"
                                       "faded blue"
                                       "faded blue"
                                       "dotted black"
                                       "dotted black"
                                       "dotted black"
                                       "dotted black"
                                       "dotted black"
                                       "dotted black"))
        (make-bag "faded blue" empty)
        (make-bag "dotted black" empty)))

(@htdf lookup-bag)
(@signature String (listof Bag) -> Bag)
;; looks up a Bag in a list given the String corresponding to its colour.

(define (lookup-bag name lob)
  (local [(define (scan lst)
            (cond [(empty? lst) (error "No bag named " name)]
                  [else
                   (if (string=? (bag-colour (first lst)) name)
                       (first lst)
                       (scan (rest lst)))]))]
    (scan lob)))

(@template encapsulated genrec Bag (listof String))

(define (fn-for-bag b lob)
  (local [(define (fn-for-bag b)
            (... (bag-colour b)
                 (fn-for-los (bag-subs b))))

          (define (fn-for-los los)
            (cond [(empty? los) (...)]
                  [else
                   (... (fn-for-bag (lookup-bag (first los) lob))
                        (fn-for-los (rest los)))]))]
    (fn-for-bag b)))
  

;; FUNCTIONS
(@htdf input-to-lob)
(@signature (listof String) -> (listof Bag))
;; Converts the input list of strings to a list of Bags (w/ correct colour, subs)

(@template (listof String))

(define (input-to-lob los)
  (cond [(empty? los) empty]
        [else
         (cons (string-to-bag (first los))
               (input-to-lob (rest los)))]))

(@htdf string-to-bag)
(@signature String -> Bag)
;; Converts one string (one line of the .txt input file) into the bag data.
(check-expect (string-to-bag "posh magenta bags contain no other bags.")
              (make-bag "posh magenta" empty))
(check-expect (string-to-bag "dull aqua bags contain 4 dark fuchsia bags, 1 shiny purple bag.")
              (make-bag "dull aqua" (list "dark fuchsia"
                                          "dark fuchsia"
                                          "dark fuchsia"
                                          "dark fuchsia"
                                          "shiny purple")))


(define (string-to-bag s)
  (local [(define split (string-split s))]
    (make-bag (string-append (first split) " " (second split))
              (ss-to-subs (rest (rest (rest (rest split))))))))

(@htdf ss-to-subs)
(@signature (listof String) -> (listof Bag))
;; Converts split strings to list of subs for the bag
(check-expect (ss-to-subs (list "no"
                                "other"
                                "bags."))
              empty)
(check-expect (ss-to-subs (list "4"
                    "dark"
                    "fuchsia"
                    "bags,"
                    "1"
                    "shiny"
                    "purple"
                    "bag."))
              (list "dark fuchsia"
                    "dark fuchsia"
                    "dark fuchsia"
                    "dark fuchsia"
                    "shiny purple"))

(define (ss-to-subs los)
  (cond [(empty? los) empty]
        [(string=? (first los) "no") empty]
        [else
         (append (build-list (string->number (first los))
                     (λ (n) (string-append (second los)
                                           " "
                                           (third los))))
                 (ss-to-subs (rest (rest (rest (rest los))))))]))
  

(@htdf shiny-gold-reachable?)
(@signature Bag (listof Bag) -> Boolean)
;;Return true if shiny gold bag reachable from prov. Bag in (listof Bag).
(check-expect (shiny-gold-reachable?
               (make-bag "shiny gold"
                         (list "dark olive"
                               "vibrant plum"
                               "vibrant plum"))
               TEST-BAG-NETWORK) true)
(check-expect (shiny-gold-reachable?
               (make-bag "light red"
                         (list "bright white"
                               "muted yellow"
                               "muted yellow"))
               TEST-BAG-NETWORK) true)
(check-expect (shiny-gold-reachable?
               (make-bag "dark olive"
                         (list "faded blue"
                               "faded blue"
                               "faded blue"
                               "dotted black"
                               "dotted black"
                               "dotted black"
                               "dotted black"))
               TEST-BAG-NETWORK) false)
(check-expect (shiny-gold-reachable?
               (make-bag "dotted black" empty)
               TEST-BAG-NETWORK) false)

(@template encapsulated genrec Bag (listof String) accumulator)

(define (shiny-gold-reachable? b0 lob)
  ; b-wl is (listof String); colours of bags left to visit.
  ; visited is (listof String); colours of bags visited already.
  ;                             (prevents redundant function calls)
  ; TERMINATION ARGUMENT: There are finitely many bags and bags cannnot
  ;                       be visited more than once, so eventually we either
  ;                       find a shiny gold bag or the search space is exhausted.
  (local [(define (fn-for-bag b b-wl visited)
            (cond [(member? (bag-colour b) visited)
                   (fn-for-los b-wl visited)]
                  [(string=? "shiny gold" (bag-colour b))
                   true]
                  [else
                   (fn-for-los (append (bag-subs b) b-wl)
                               (cons (bag-colour b) visited))]))
          
          (define (fn-for-los b-wl visited)
            (cond [(empty? b-wl) false]
                  [else
                   (fn-for-bag (lookup-bag (first b-wl) lob)
                               (rest b-wl)
                               visited)]))]
    (fn-for-bag b0 empty empty)))


(@htdf count-internal-bags)
(@signature Bag (listof Bag) -> Natural)
;; Counts all bags contained within the given bag in the given bag network.
(check-expect (count-internal-bags
               (make-bag "shiny gold"
                         (list "dark olive"
                               "vibrant plum"
                               "vibrant plum"))
               TEST-BAG-NETWORK) 32)

(@template encapsulated genrec Bag (listof String) accumulator)

(define (count-internal-bags b0 lob)
  ; b-wl is (listof String); colours of bags left to visit.
  ; count is Natural; number of bags already visited
  ; TERMINATION ARGUMENT: There are finitely many bags and bags on one
  ;                       path cannot be visited more than once (there are
  ;                       no cycles in the data structure) so eventually
  ;                       the function must terminate.
  (local [(define (fn-for-bag b b-wl count)
            (fn-for-los (append (bag-subs b) b-wl)
                        (add1 count)))
          
          (define (fn-for-los b-wl count)
            (cond [(empty? b-wl) (sub1 count)] ;sub1 to remove initial gold bag
                  [else
                   (fn-for-bag (lookup-bag (first b-wl) lob)
                               (rest b-wl)
                               count)]))]
    (fn-for-bag b0 empty 0)))


;; SOLUTION

(define INPUT-BAG-NETWORK (input-to-lob INPUT))
(define SOL1 (sub1 (length (filter (λ (n) (shiny-gold-reachable?
                                           n
                                           INPUT-BAG-NETWORK))
                                   INPUT-BAG-NETWORK))))
; sub1 is there to not include the shiny bag in the list.
(define SOL2 (count-internal-bags (lookup-bag "shiny gold" INPUT-BAG-NETWORK)
                                  INPUT-BAG-NETWORK))
