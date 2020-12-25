;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname day14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/batch-io)
(require racket/string)


;; CONSTANTS/INPUT
(define INPUT (read-lines "input.txt"))


;; DATA DEFINITIONS
(@htdd BitMask)
;; Bitmask is String
;; interp. is a 36-bit-long memory bitmask of X (nothing) 0s, and 1s.
(define BM1 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")

(@htdd Memory)
(define-struct mem (loc val))
;; Memory is (make-mem String String)
;; interp. is a value in memory, at location loc and with (binary) val.
(define MEM1 (make-mem "0" "000000000000000000000000000001100101"))


;; FUNCTIONS PART 1
(@htdf process-input)
(@signature (listof String) -> (listof Memory))
;; Writes the instructions into memory
(check-expect (process-input (list "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                                   "mem[8] = 11"
                                   "mem[7] = 101"
                                   "mem[8] = 0"))
              (list (make-mem "8" "000000000000000000000000000001000000")
                    (make-mem "7" "000000000000000000000000000001100101")))

(@template accumulator (listof String))

(define (process-input los0)
  (local [(define (process-input los bm rsf)
            (cond [(empty? los) rsf]
                  [else
                   (if (string=? (substring (first los) 0 3) "mem")
                       (process-input (rest los) bm (update-memory (first los) bm rsf))
                       (process-input (rest los) (new-bm (first los)) rsf))]))]
    (process-input los0
                   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                   empty)))

(@htdf update-memory)
(@signature String BitMask (listof Memory) -> (listof Memory))
;; Updates the currently written memory according to instruction and current bitmask.
(check-expect (update-memory "mem[8] = 0"
                             "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                              (list (make-mem "8" "000000000000000000000000000001001001")
                                    (make-mem "7" "000000000000000000000000000001100101")))
               (list (make-mem "8" "000000000000000000000000000001000000")
                    (make-mem "7" "000000000000000000000000000001100101")))

(@template fn-composition use-abstract-fn)

(define (update-memory s bm lom)
  (local [(define split-instruction (string-split s))
          (define mem-val-to-write (string->number (third split-instruction)))
          (define mem-addr-to-write (substring (first split-instruction)
                                               4 (- (string-length
                                                     (first split-instruction)) 1)))]
    (cons (make-mem mem-addr-to-write
                    (apply-bitmask (base10-to-binary mem-val-to-write) bm))
          (filter (λ (m)
                    (not (address-assigned? m
                                            mem-addr-to-write)))
                  lom))))

(@htdf address-assigned?)
(@signature Memory String -> Boolean)
;; Produces true if memory address has the same value as string
(check-expect (address-assigned? (make-mem "8" "000000000000000000000000000001000000") "8")
              true)
(check-expect (address-assigned? (make-mem "8" "000000000000000000000000000001000000") "7")
              false)

(@template Memory)

(define (address-assigned? m s)
  (string=? (mem-loc m) s))


(@htdf base10-to-binary)
(@signature Natural -> String)
;; Produces a string corresponding to the base 2 representation of a number < 2^36
(check-expect (base10-to-binary 0)
              "000000000000000000000000000000000000")
(check-expect (base10-to-binary 51539607553)
              "110000000000000000000000000000000001")

(@template accumulator)

(define (base10-to-binary n0)
  (local [(define (base10-to-binary n place)
            (cond [(= place -1) ""]
                  [else
                   (if (< (- n (expt 2 place)) 0)
                       (string-append "0" (base10-to-binary n (sub1 place)))
                       (string-append "1" (base10-to-binary
                                           (- n (expt 2 place))
                                           (sub1 place))))]))]
    (base10-to-binary n0 35)))


(@htdf new-bm)
(@signature String -> BitMask)
;; Produces the new bitmask from the line of instruction.
(check-expect (new-bm "mask = 10011X0100001X1110X0X001000X10X01101")
              "10011X0100001X1110X0X001000X10X01101")

(define (new-bm s)
  (third (string-split s)))
                   

(@htdf apply-bitmask)
(@signature String BitMask -> String)
;; Applies bitmask to memory value.
;; CONSTRAINT: val is a String of 36 0s and 1s.
(check-expect (apply-bitmask "000000000000000000000000000001100101"
              "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
              "000000000000000000000000000001100101")
(check-expect (apply-bitmask "000000000000000000000000000000001011"
                             "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
              "000000000000000000000000000001001001")
(check-expect (apply-bitmask "000000000000000000000000000000000000"
                             "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
              "000000000000000000000000000001000000")

(define (apply-bitmask val bm)
  (cond [(string=? val "") ""]
        [else
         (string-append (apply-bitmask-bit
                         (substring val 0 1)
                         (substring bm 0 1))
                        (apply-bitmask
                         (substring val 1)
                         (substring bm 1)))]))


(@htdf apply-bitmask-bit)
(@signature String String -> String)
;; Produces first String if second is X, second string otherwise.
;; CONSTRIANT: val-bit is "0" or "1", bm-bit is "X" or "0" or "1".
(check-expect (apply-bitmask-bit "0" "X") "0")
(check-expect (apply-bitmask-bit "1" "X") "1")
(check-expect (apply-bitmask-bit "0" "0") "0")
(check-expect (apply-bitmask-bit "0" "1") "1")
(check-expect (apply-bitmask-bit "1" "0") "0")
(check-expect (apply-bitmask-bit "1" "1") "1")

(define (apply-bitmask-bit val-bit bm-bit)
  (if (string=? bm-bit "X")
      val-bit
      bm-bit))


(@htdf sum-memory)
(@signature (listof Memory) -> Natural)
;; Produces the sum of the values in memory (after converting from decimal).
(check-expect (sum-memory (list (make-mem "0" "000000000000000000000000000000000000")))
              0)
(check-expect (sum-memory (list (make-mem "0" "000000000000000000000000000000001110")
                                (make-mem "15" "000000000000000000000000000000101110")))
              60)

(@template (listof Memory))

(define (sum-memory lom)
  (cond [(empty? lom) 0]
        [else
         (+ (get-memory-val (first lom))
            (sum-memory (rest lom)))]))


(@htdf get-memory-val)
(@signature Memory -> Natural)
;; Produces the value (in base 10) located in that location in memory.
(check-expect (get-memory-val (make-mem "0" "000000000000000000000000000000000000"))
              0)
(check-expect (get-memory-val (make-mem "15" "000000000000000000000000000000101110"))
              46)

(@template Memory)

(define (get-memory-val m)
  (binary-to-base10 (mem-val m)))


(@htdf binary-to-base10)
(@signature String -> Natural)
;; Produces the Natural corresponding to that string.
;; CONSTRAINT: String is of length 36, and only has 0s and 1s
(check-expect (binary-to-base10 "000000000000000000000000000000000000")
              0)
(check-expect (binary-to-base10 "100000000000000000000000000000000000")
              (expt 2 35))
(check-expect (binary-to-base10 "110000000000000000000000000000000001")
              (+ (expt 2 35)
                 (expt 2 34)
                 (expt 2 0)))

(@template accumulator)

(define (binary-to-base10 s0)
  ;; place is Natural; the value of the bit.
  (local [(define (binary-to-base10 s place)
            (cond [(= place -1) 0]
                  [else
                   (+ (* (string->number
                       (substring s 0 1))
                         (expt 2 place))
                      (binary-to-base10
                       (substring s 1)
                       (sub1 place)))]))]
    (binary-to-base10 s0 35)))

;; FUNCTIONS PART 2
(@htdf process-input-2)
(@signature (listof String) -> (listof Memory))
;; Writes the instructions into memory
(check-expect (process-input-2 (list "mask = 000000000000000000000000000000X1001X"
                                   "mem[42] = 100"
                                   "mask = 00000000000000000000000000000000X0XX"
                                   "mem[26] = 1"))
              (list (make-mem "16" "000000000000000000000000000000000001")
                    (make-mem "17" "000000000000000000000000000000000001")
                    (make-mem "18" "000000000000000000000000000000000001")
                    (make-mem "19" "000000000000000000000000000000000001")
                    (make-mem "24" "000000000000000000000000000000000001")
                    (make-mem "25" "000000000000000000000000000000000001")
                    (make-mem "26" "000000000000000000000000000000000001")
                    (make-mem "27" "000000000000000000000000000000000001")
                    (make-mem "58" "000000000000000000000000000001100100")
                    (make-mem "59" "000000000000000000000000000001100100")))
                    

(@template accumulator (listof String))

(define (process-input-2 los0)
  (local [(define (process-input los bm rsf)
            (cond [(empty? los) rsf]
                  [else
                   (if (string=? (substring (first los) 0 3) "mem")
                       (process-input (rest los) bm (update-memory-2 (first los) bm rsf))
                       (process-input (rest los) (new-bm (first los)) rsf))]))]
    (process-input los0
                   "000000000000000000000000000000000000"
                   empty)))

(@htdf update-memory-2)
(@signature String BitMask (listof Memory) -> (listof Memory))
;; Follows instruction and updates memory according to bitmask.
(check-expect (update-memory-2
               "mem[42] = 100"
               "000000000000000000000000000000X1001X"
               empty)
              (list
               (make-mem "26" "000000000000000000000000000001100100")
               (make-mem "27" "000000000000000000000000000001100100")
               (make-mem "58" "000000000000000000000000000001100100")
               (make-mem "59" "000000000000000000000000000001100100")))
(check-expect (update-memory-2
               "mem[26] = 1"
               "00000000000000000000000000000000X0XX"
               (list
               (make-mem "26" "000000000000000000000000000001100100")
               (make-mem "27" "000000000000000000000000000001100100")
               (make-mem "58" "000000000000000000000000000001100100")
               (make-mem "59" "000000000000000000000000000001100100")))
              (list (make-mem "16" "000000000000000000000000000000000001")
                    (make-mem "17" "000000000000000000000000000000000001")
                    (make-mem "18" "000000000000000000000000000000000001")
                    (make-mem "19" "000000000000000000000000000000000001")
                    (make-mem "24" "000000000000000000000000000000000001")
                    (make-mem "25" "000000000000000000000000000000000001")
                    (make-mem "26" "000000000000000000000000000000000001")
                    (make-mem "27" "000000000000000000000000000000000001")
                    (make-mem "58" "000000000000000000000000000001100100")
                    (make-mem "59" "000000000000000000000000000001100100")))
              
               

(define (update-memory-2 s bm lom)
  (local [(define split-instruction (string-split s))
          (define mem-val-base10 (string->number (third split-instruction)))
          (define mem-val-binary (base10-to-binary mem-val-base10))
          (define specified-address (substring (first split-instruction)
                                               4 (- (string-length
                                                     (first split-instruction)) 1)))
          (define specified-address-bin (base10-to-binary (string->number specified-address)))
          (define adds-to-write (get-addresses specified-address-bin bm))]
    (append (foldr (λ (addr nmr) (cons (make-mem addr mem-val-binary) nmr))
                   empty
                   adds-to-write)
            (remove-newly-written adds-to-write lom))))


(@htdf get-addresses)
(@signature String BitMask -> (listof String))
;; Produce list of all (base10) Numbers (Strings) corresponding to the address and bitmask.
;; !!!
(check-expect (get-addresses "000000000000000000000000000000101010"
                             "000000000000000000000000000000X1001X")
              (list "26" "27" "58" "59"))
(check-expect (get-addresses "000000000000000000000000000000011010"
                             "00000000000000000000000000000000X0XX")
              (list "16" "17" "18" "19" "24" "25" "26" "27"))


(@template fn-composition use-abstract-fn)

(define (get-addresses addr bm)
  (map number->string (map binary-to-base10 (get-addresses-binary addr bm))))


(@htdf get-addresses-binary)
(@signature String BitMask -> (listof String))
;; Produce list of all (binary) numbers (Strings) corresponding to the address and bitmask.
(check-expect (get-addresses-binary "000000000000000000000000000000101010"
                             "000000000000000000000000000000X1001X")
              (list "000000000000000000000000000000011010"
                    "000000000000000000000000000000011011"
                    "000000000000000000000000000000111010"
                    "000000000000000000000000000000111011"))

(define (get-addresses-binary addr bm)
  (cond [(string=? addr "") (list "")]
        [else
         (local [(define first-addr (substring addr 0 1))
                 (define first-bm (substring bm 0 1))
                 (define recursion (get-addresses-binary (substring addr 1) (substring bm 1)))]
           (cond [(string=? first-bm "0")
                  (map (λ (s) (string-append first-addr s)) recursion)]
                 [(string=? first-bm "1")
                  (map (λ (s) (string-append "1" s)) recursion)]
                 [else ;(string=? first-bm "X")
                  (append
                   (map (λ (s) (string-append "0" s)) recursion)
                   (map (λ (s) (string-append "1" s)) recursion))]))]))
         


(@htdf remove-newly-written)
(@signature (listof String) (listof Memory) -> (listof Memory))
;; Removes all entries in lom which correspond to addresses/locations in los.
(check-expect (remove-newly-written (list "10" "40" "25")
                                    (list (make-mem "16" "000000000000000000000000000000000001")
                                          (make-mem "10" "000000000000000000000000000000000001")
                                          (make-mem "18" "000000000000000000000000000000000001")
                                          (make-mem "19" "000000000000000000000000000000000001")
                                          (make-mem "24" "000000000000000000000000000000000001")
                                          (make-mem "25" "000000000000000000000000000000000001")))
              (list (make-mem "16" "000000000000000000000000000000000001")
                    (make-mem "18" "000000000000000000000000000000000001")
                    (make-mem "19" "000000000000000000000000000000000001")
                    (make-mem "24" "000000000000000000000000000000000001")))

(@template (listof Memory))

(define (remove-newly-written los lom)
  (cond [(empty? lom) empty]
        [else
         (if (member? (mem-loc (first lom)) los)
             (remove-newly-written los (rest lom))
             (cons (first lom) (remove-newly-written los (rest lom))))]))
                                     

;; SOLUTION
(define FINAL-MEMORY-P1 (process-input INPUT))
(define SOL1 (sum-memory FINAL-MEMORY-P1))

(define FINAL-MEMORY-P2 (process-input-2 INPUT))
(define SOL2 (sum-memory FINAL-MEMORY-P2))

