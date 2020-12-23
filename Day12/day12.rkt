;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname day12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/batch-io)
(require racket/string)

;; CONSTANTS/INPUT
(define INPUT (read-lines "input.txt"))

;; FUNCTIONS

(@htdf m-distance)
(@signature (listof String) -> Natural)
;; Produce the manhattan distance from origin after instructions.
(check-expect (m-distance empty) 0)
(check-expect (m-distance (list "N10")) 10)
(check-expect (m-distance (list "S10")) 10)
(check-expect (m-distance (list "E10")) 10)
(check-expect (m-distance (list "W10")) 10)
(check-expect (m-distance (list "F10")) 10)
(check-expect (m-distance (list "N10" "S10" "E10" "W10")) 0)
(check-expect (m-distance (list "R180")) 0)
(check-expect (m-distance (list "N10" "E10")) 20)
(check-expect (m-distance (list "F10" "N3" "F7" "R90" "F11")) 25)
(check-expect (m-distance
               (list "F35" "N2" "W5" "F83" "S5" "R180" "S3" "L270" "N1" "F35"))
              143)
              
(@template (listof String) accumulator)

(define (m-distance los0)
  ;; nsc is Natural; North-South coordinate
  ;; ewc is Natural; East-West coordinate
  ;; angle is Natural (one of 0, 90, 180, 270); angle of ship.
  (local [(define (m-distance los nsc ewc angle)
            (cond [(empty? los) (+ (abs nsc) (abs ewc))]
                  [else
                   (local [(define first-i (first los))
                           (define rest-i (rest los))
                           (define inst (substring first-i 0 1))
                           (define val
                             (string->number (substring first-i 1)))]
                     (cond [(string=? inst "N")
                            (m-distance rest-i (+ nsc val) ewc angle)]
                           [(string=? inst "S")
                            (m-distance rest-i (- nsc val) ewc angle)]
                           [(string=? inst "E")
                            (m-distance rest-i nsc (+ ewc val) angle)]
                           [(string=? inst "W")
                            (m-distance rest-i nsc (- ewc val) angle)]
                           [(string=? inst "R")
                            (m-distance rest-i nsc ewc
                                        (modulo (+ angle val) 360))]
                           [(string=? inst "L")
                            (m-distance rest-i nsc ewc
                                        (modulo (- angle val) 360))]
                           [(string=? inst "F")
                            (cond [(= angle 0)
                                   (m-distance rest-i (+ nsc val) ewc angle)]
                                  [(= angle 90)
                                   (m-distance rest-i nsc (+ ewc val) angle)]
                                  [(= angle 180)
                                   (m-distance rest-i (- nsc val) ewc angle)]
                                  [(= angle 270)
                                   (m-distance rest-i nsc (- ewc val) angle)])]))]))]
          (m-distance los0 0 0 90)))

(@htdf m-distance-waypoint)
(@signature (listof String) -> Natural)
;; Produce manhattan distance from origin after instructions (waypoint mechanics)
;; Waypoint starts at 10 units E and 1 unit N relative to the ship
(check-expect (m-distance-waypoint (list "N10" "S20" "W30" "E40")) 0)
(check-expect (m-distance-waypoint (list "F1")) 11)
(check-expect (m-distance-waypoint (list "F10" "N3" "F7" "R90" "F11")) 286)

(@template (listof String) accumulator)

(define (m-distance-waypoint los0)
  ;; ns-ship is Natural; North-South coordinate of ship
  ;; ew-ship is Natural; East-West coordinate of ship
  ;; ns-wp is Natural; North-South coordinate of waypoint (relative to ship)
  ;; ew-wp is Natural; East-West coordinate of waypoint (relative to ship)
  (local [(define (m-dist-wp los ns-ship ew-ship ns-wp ew-wp)
            (cond [(empty? los) (+ (abs ns-ship) (abs ew-ship))]
                  [else
                   (local [(define first-i (first los))
                           (define rest-i (rest los))
                           (define inst (substring first-i 0 1))
                           (define val
                             (string->number (substring first-i 1)))]
                     (cond [(string=? inst "N")
                            (m-dist-wp rest-i ns-ship ew-ship (+ ns-wp val) ew-wp)]
                           [(string=? inst "S")
                            (m-dist-wp rest-i ns-ship ew-ship (- ns-wp val) ew-wp)]
                           [(string=? inst "E")
                            (m-dist-wp rest-i ns-ship ew-ship ns-wp (+ ew-wp val))]
                           [(string=? inst "W")
                            (m-dist-wp rest-i ns-ship ew-ship ns-wp (- ew-wp val))]
                           [(or (and (string=? inst "R") (= val 90))
                                (and (string=? inst "L") (= val 270)))
                            (m-dist-wp rest-i ns-ship ew-ship (* -1 ew-wp) ns-wp)]
                           [(or (and (string=? inst "R") (= val 180))
                                (and (string=? inst "L") (= val 180)))
                            (m-dist-wp rest-i ns-ship ew-ship (* -1 ns-wp) (* -1 ew-wp))]
                           [(or (and (string=? inst "R") (= val 270))
                                (and (string=? inst "L") (= val 90)))
                            (m-dist-wp rest-i ns-ship ew-ship ew-wp (* -1 ns-wp))]
                           [(string=? inst "F")
                            (m-dist-wp rest-i
                                       (+ (* val ns-wp)
                                          ns-ship)
                                       (+ (* val ew-wp)
                                          ew-ship)
                                       ns-wp
                                       ew-wp)]))]))]
          (m-dist-wp los0 0 0 1 10)))

;; SOLUTION
(define sol1 (m-distance INPUT))
(define sol2 (m-distance-waypoint INPUT))
