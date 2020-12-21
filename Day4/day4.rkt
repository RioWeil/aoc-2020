;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname day4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/batch-io)
(require racket/string)

;; CONSTANTS/INPUT
(define INPUT (read-lines "input.txt"))

;; FUNCTIONS

(@htdf group-passports)
(@signature (listof String) -> (listof String))
;; Produces list of strings with each passport grouped into a single string.
;(check-expect (group-passports empty) (list ""))
;(check-expect (group-passports (list "aa" "bbbb" "" "cccc"))
;              (list " cccc" " aa bbbb"))

(@template encapsulated accumulator (listof String))

(define (group-passports unsorted0)
  ;; sorted is (listof String); all the grouped passported so-far
  ;; passport is String; the current passport being grouped together.
  (local [(define (group-passports unsorted sorted passport)
            (cond [(empty? unsorted) (cons passport sorted)]
                  [else
                   (if (string=? "" (first unsorted))
                       (group-passports (rest unsorted)
                                        (cons passport sorted)
                                        "")
                       (group-passports (rest unsorted)
                                        sorted 
                                        (string-append passport " "
                                                       (first unsorted))))]))]
    (group-passports unsorted0 empty "")))


(@htdf valid-passport?)
(@signature String -> Boolean)
;; Produces true if supplied passport is valid (contains 7 required fields)
(check-expect (valid-passport? "ecl:gry pid:860033327 eyr:2020 hcl:#fffffdbyr:1937 iyr:2017 cid:147 hgt:183cm")
              true)
(check-expect (valid-passport? "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884hcl:#cfa07d byr:1929")
              false)
(check-expect (valid-passport? "hcl:#ae17e1 iyr:2013eyr:2024ecl:brn pid:760753108 byr:1931hgt:179cm")
              true)
(check-expect (valid-passport? "hcl:#cfa07d eyr:2025 pid:166559648iyr:2011 ecl:brn hgt:59in")
              false)

(@template String)

(define (valid-passport? s)
  (and (string-contains? s "byr")
       (string-contains? s "iyr")
       (string-contains? s "eyr")
       (string-contains? s "hgt")
       (string-contains? s "hcl")
       (string-contains? s "ecl")
       (string-contains? s "pid")))

;;PROBLEM 2 Conditions
;;
;;byr (Birth Year) - four digits; at least 1920 and at most 2002.
;;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;;hgt (Height) - a number followed by either cm or in:
;;If cm, the number must be at least 150 and at most 193.
;;If in, the number must be at least 59 and at most 76.
;;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;;pid (Passport ID) - a nine-digit number, including leading zeroes.
;;cid (Country ID) - ignored, missing or not.

(@htdf stricter-valid-passport?)
(@signature (listof String) -> Boolean)
;; Returns true if all valid passport conditions are satisfied
;; CONSTRAINT: All 7 required fields must be present in the list.
(check-expect (stricter-valid-passport? (list "eyr:1972"
                                              "cid:100"
                                              "hcl:#18171d"
                                              "ecl:amb"
                                              "hgt:170"
                                              "pid:186cm"
                                              "iyr:2018"
                                              "byr:1926"))
              false)
(check-expect (stricter-valid-passport? (list "iyr:2019"
                                              "hcl:#602927"
                                              "eyr:1967"
                                              "hgt:170cm"
                                              "ecl:grn"
                                              "pid:012533040"
                                              "byr:1946"))
              false)
(check-expect (stricter-valid-passport? (list "pid:087499704"
                                              "hgt:74in"
                                              "ecl:grn"
                                              "iyr:2012"
                                              "eyr:2030"
                                              "byr:1980"
                                              "hcl:#623a2f"))
              true)
(check-expect (stricter-valid-passport? (list "eyr:2029"
                                              "ecl:blu"
                                              "cid:129"
                                              "byr:1989"
                                              "iyr:2014"
                                              "pid:896056539"
                                              "hcl:#a97842"
                                              "hgt:165cm"))
              true)

(@template fn-composition)

(define (stricter-valid-passport? los)
  (valid-ordered-passport? (sort (remove-cid los) string<?)))

(@htdf remove-cid)
(@signature (listof String) -> (listof String))
;; Removes cid from passport
(check-expect (remove-cid (list "pid:087499704"
                                "hgt:74in"
                                "ecl:grn"
                                "iyr:2012"
                                "eyr:2030"
                                "byr:1980"
                                "hcl:#623a2f"))
              (list "pid:087499704"
                    "hgt:74in"
                    "ecl:grn"
                    "iyr:2012"
                    "eyr:2030"
                    "byr:1980"
                    "hcl:#623a2f"))
(check-expect (remove-cid (list "eyr:2029"
                                "ecl:blu"
                                "cid:129"
                                "byr:1989"
                                "iyr:2014"
                                "pid:896056539"
                                "hcl:#a97842"
                                "hgt:165cm"))
              (list "eyr:2029"
                    "ecl:blu"
                    "byr:1989"
                    "iyr:2014"
                    "pid:896056539"
                    "hcl:#a97842"
                    "hgt:165cm"))

(@template (listof String))

(define (remove-cid los)
  (cond [(empty? los) empty]
        [else
         (if (string=? "cid" (substring (first los) 0 3))
             (remove-cid (rest los))
             (cons (first los) (remove-cid (rest los))))]))

(@htdf valid-ordered-passport?)
(@signature (listof String) -> Boolean)
;; Checks to see if an (ordered, cid removed) passport is valid.
;; CONSTRAINT: (listof String) must be a 7-element ordered list.

(@template fn-composition)

(define (valid-ordered-passport? los)
  (and (valid-byr? (first los))
       (valid-ecl? (second los))
       (valid-eyr? (third los))
       (valid-hcl? (fourth los))
       (valid-hgt? (fifth los))
       (valid-iyr? (sixth los))
       (valid-pid? (seventh los))))

(@htdf valid-byr?)
(@signature String -> Boolean)
;; Produces true if byr is between 1920 and 2002
;; CONSTRAINT: String must be of form "byr:XXXX"
(check-expect (valid-byr? "byr:1920") true)
(check-expect (valid-byr? "byr:1980") true)
(check-expect (valid-byr? "byr:2002") true)
(check-expect (valid-byr? "byr:2003") false)
(check-expect (valid-byr? "byr:1919") false)

(@template String)

(define (valid-byr? s)
  (if (= (string-length s) 8)
      (<= 1920 (string->number (substring s 4 8)) 2002)
      false))

(define EYE-COLOURS (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth"))

(@htdf valid-ecl?)
(@signature String -> Boolean)
;; Produces true if ecl is exactly one of amb blu brn gry grn hzl oth.
(check-expect (valid-ecl? "ecl:amb") true)
(check-expect (valid-ecl? "ecl:grn") true)
(check-expect (valid-ecl? "ecl:red") false)
(check-expect (valid-ecl? "ecl:ambblu") false)

(@template String use-abstract-fn)

(define (valid-ecl? s)
  (if (= (string-length s) 7)
      (ormap (λ (n) (string=? n (substring s 4 7))) EYE-COLOURS)
      false))


(@htdf valid-eyr?)
(@signature String -> Boolean)
;; Produces true if eyr is between 2020 and 2030
(check-expect (valid-eyr? "eyr:2020") true)
(check-expect (valid-eyr? "eyr:2030") true)
(check-expect (valid-eyr? "eyr:2025") true)
(check-expect (valid-eyr? "eyr:2019") false)
(check-expect (valid-eyr? "eyr:2031") false)

(@template String)

(define (valid-eyr? s)
  (if (= (string-length s) 8)
      (<= 2020 (string->number (substring s 4 8)) 2030)
      false))

(define VALID-CHARS (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
                          "a" "b" "c" "d" "e" "f"))
                     
(@htdf valid-hcl?)
(@signature String -> Boolean)
;; Produces true if hcl is a # followed by exactly six characters 0-9 or a-f.
(check-expect (valid-hcl? "hcl:#1af029") true)
(check-expect (valid-hcl? "hcl:4210596") false)
(check-expect (valid-hcl? "hcl:#19245") false)
(check-expect (valid-hcl? "hcl:#9124095") false)
(check-expect (valid-hcl? "hcl:#fj4201") false)

(@template String)

(define (valid-hcl? s)
  (and (= (string-length s) 11)
       (string=? (substring s 4 5) "#")
       (ormap (λ (n) (string=? n (substring s 5 6))) VALID-CHARS)
       (ormap (λ (n) (string=? n (substring s 6 7))) VALID-CHARS)
       (ormap (λ (n) (string=? n (substring s 7 8))) VALID-CHARS)
       (ormap (λ (n) (string=? n (substring s 8 9))) VALID-CHARS)
       (ormap (λ (n) (string=? n (substring s 9 10))) VALID-CHARS)
       (ormap (λ (n) (string=? n (substring s 10 11))) VALID-CHARS)))

(@htdf valid-hgt?)
(@signature String -> Boolean)
;; Produces true if hgt is a number followed by either cm such that
;; if in cm, must be at least 150 and at most 193
;; if in in, must be at least 59 and at most 76
(check-expect (valid-hgt? "hgt:89cm") false)
(check-expect (valid-hgt? "hgt:149cm") false)
(check-expect (valid-hgt? "hgt:150cm") true)
(check-expect (valid-hgt? "hgt:193cm") true)
(check-expect (valid-hgt? "hgt:194cm") false)
(check-expect (valid-hgt? "hgt:1in") false)
(check-expect (valid-hgt? "hgt:58in") false)
(check-expect (valid-hgt? "hgt:59in") true)
(check-expect (valid-hgt? "hgt:76in") true)
(check-expect (valid-hgt? "hgt:77in") false)
(check-expect (valid-hgt? "hgt:100in") false)

(@template String)

(define (valid-hgt? s)
  (local [(define len (string-length s))
          (define units (substring s (- len 2) len))]
    (cond [(string=? "cm" units)
           (and (= len 9)
                (<= 150 (string->number (substring s 4 7)) 193))]
          [(string=? "in" units)
           (and (= len 8)
                (<= 59 (string->number (substring s 4 6)) 76))]
          [else
           false])))

(@htdf valid-iyr?)
(@signature String -> Boolean)
;; Produces true if iyr is at least 2010 and at most 2020
(check-expect (valid-iyr? "iyr:2010") true)
(check-expect (valid-iyr? "iyr:2020") true)
(check-expect (valid-iyr? "iyr:2015") true)
(check-expect (valid-iyr? "iyr:2009") false)
(check-expect (valid-iyr? "iyr:2021") false)

(@template String)

(define (valid-iyr? s)
  (if (= (string-length s) 8)
      (<= 2010 (string->number (substring s 4 8)) 2020)
      false))

(@htdf valid-pid?)
(@signature String -> Boolean)
;; Produces true if pid is a nine-digit number
(check-expect (valid-pid? "pid:123456789") true)
(check-expect (valid-pid? "pid:12345678") false)
(check-expect (valid-pid? "pid:0123456789") false)
(check-expect (valid-pid? "pid:12345678p") false)


(@template String use-abstract-fn)

(define (valid-pid? s)
  (if (= (string-length s) 13)
      (not (false? (string->number (substring s 4 13))))
      false))

;; SOLUTION

(define SORTED (group-passports INPUT)) ;Sorts input .txt file into individual passports
(define VALID-PPS (filter valid-passport? SORTED)) ;Filters out passports based on Problem 1 conditions
(define SPLIT (map string-split VALID-PPS)) ; splits into sublists with separated passport fields
(define STRICT-VALID-PPS (filter stricter-valid-passport? SPLIT))

(define SOL1 (length VALID-PPS))
(define SOL2 (length STRICT-VALID-PPS))
