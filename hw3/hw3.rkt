#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "../include/cs151-universe.rkt")

;; === data definitions

(define-type (Optional A)
  (U 'None (Some A)))

(define-struct (Some A)
  ([value : A]))

(define-struct Click
  ([x : Integer]
   [y : Integer]))

(define-struct CircleWorld
  ([background-color : Image-Color]
   [initial-color    : Image-Color]
   [change-to-color  : Image-Color]
   [num-circles : Integer]
   [radius  : Integer]
   [padding : Integer]
   [clicked-on? : (Listof Boolean)]))
                               
;; === operations and calculations


(: replace-at : All (A) Integer A (Listof A) -> (Listof A))

;; replace the item at the given position
;; position counting starts at 0
;; ex: (replace-at 0 'Z '(a b c)) -> '(Z b c)
;; ex: (replace-at 1 'Z '(a b c)) -> '(a Z c)

(define (replace-at index value list)
  (match list
    ['() '()]
    [(cons head tail) (if (= index 0)

                          
                          (cons value (replace-at (- index 1) value tail))
                          (cons head (replace-at (- index 1) value tail)))]))
                          



(: clicked-within : CircleWorld Integer Integer (Listof Integer) -> (Optional Integer))

;looks into center list and returns index if exists within a circle otherwise 'None
;; assume each circle has an index from 0 to (n-1), counting from the left
;; if the click is within circle i, this function returns (Some i)
;; otherwise 'None

(define (clicked-within para x y list)
  (match list
    ['() 'None]
    [(cons head tail) (local
                        {(define p 0)
                         (define yMid (+ (CircleWorld-radius para)  (CircleWorld-padding para)))}


                        (if (< (+ (sqr (- head x)) (sqr (- yMid y))) (sqr  (CircleWorld-radius para)))
                            (Some (floor (/ (- head (+ (CircleWorld-radius para) (CircleWorld-padding para))) (+ (* 2 (CircleWorld-radius para)) (CircleWorld-padding para)))))
                            (clicked-within para x y tail)))]))

;;generates list of circle centers from right to left
(: makeList : CircleWorld Integer -> (Listof Integer))

(define (makeList p counter)
  (local
    {(define r (CircleWorld-radius p))
     (define pad (CircleWorld-padding p))
     (define toNext (+ (* 2 (CircleWorld-radius p)) (CircleWorld-padding p)))
     (define centerx (+ r pad))}


  (if (= counter 0)
      (cons (+ (* (- counter 0) centerx) centerx) '())
      
      (cons (+ (* (- counter 0)   toNext  ) centerx) (makeList p (- counter 1))))))

;; === universe support

(: draw : CircleWorld -> Image)

;; draw the CircleWorld, taking care that padding is correct
(define (draw p)
  
  (overlay 
  
   (match (CircleWorld-clicked-on? p)
    
     [x (local
          {(: returnCircles : Integer Integer Image-Color Image-Color (Listof Boolean) -> Image)
           (define (returnCircles r p i a list)
             (match list
               ['() empty-image]
               [(cons head tail) (beside (if (boolean=? head #f)
                                             (if (match tail
                                                   ['() #f]
                                                   [_ #t])
                                                 (overlay/offset (circle r 'solid i) (+ r  p) 0 empty-image)
                                                 (circle r 'solid i))
                                             (if (match tail
                                                   ['() #f]
                                                   [_ #t])
                                             (overlay/offset (circle r 'solid a) (+ r  p) 0 empty-image)
                                             (circle r 'solid a)
                                             ))
                                         (returnCircles r p i a tail))]))}
          
          (returnCircles (CircleWorld-radius p)
                         (CircleWorld-padding p)
                         (CircleWorld-initial-color p)
                         (CircleWorld-change-to-color p)
                         (CircleWorld-clicked-on? p)))]) 
   (rectangle (+ (* 2 (CircleWorld-radius p)
                         (CircleWorld-num-circles p)) (* (CircleWorld-padding p) (- (CircleWorld-num-circles p) 1) ) (* 2 (CircleWorld-padding p)) )
              
                      (+ (* 2 (CircleWorld-radius p)) (* (CircleWorld-padding p) 2))
                      
                      'solid
                      
                      (CircleWorld-background-color p))))


(: react-to-mouse : CircleWorld Integer Integer Mouse-Event -> CircleWorld)

;; if the user clicks on a circle, change the color to the "change-to-color"
;; if the user clicks outside any circle, reset all circles to the initial color

(define (react-to-mouse world x y e)
  (match e
    ["button-down" (CircleWorld (CircleWorld-background-color world)
                          (CircleWorld-initial-color world)
                          (CircleWorld-change-to-color world)
                          (CircleWorld-num-circles world)
                          (CircleWorld-radius world)
                          (CircleWorld-padding world)

                          (if (match (clicked-within world x y (makeList world (- (length (CircleWorld-clicked-on? world)) 1)))
                                ['None #t]
                                [_ #f])
                              
                              (make-list (CircleWorld-num-circles world) #f)
                              (replace-at (match (clicked-within world x y (makeList world (- (length (CircleWorld-clicked-on? world)) 1)))
                                            ['None 999999]
                                            [(Some head) head])

                                          
                                          #t (CircleWorld-clicked-on? world))))]
                          
;                         
    [_ world]))

                         



(: run : Image-Color Image-Color Image-Color Integer Integer Integer (Listof Boolean) -> CircleWorld)

;; run the world given setup details
(define (run bc ic ctc nc r p listy)
  (big-bang (CircleWorld bc ic ctc nc r p listy) : CircleWorld
    [to-draw draw]
    [on-mouse react-to-mouse]))


