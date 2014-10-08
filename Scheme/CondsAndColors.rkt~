#lang racket
(require gigls/unsafe)


;Problem 1: Types

(define is-color?
  (lambda (value)
    (or (irgb? value) (color-name? value) )))
  

;b) 

(define color-to-irgb
  (lambda (some-color)
    (cond ((irgb? some-color)  some-color)
          ((color-name? some-color) (color-name->irgb some-color))
          (else #f))))
  

;Problem 2: Hue Transforms

;chroma
(define chroma
  (lambda(red green blue)
    (-(max red green blue)
      (min red green blue)
      )))

;reddish-hue
(define reddish-hue
  (lambda (color)
   (let* ([red-component 
          (irgb-red color)]
         [green-component(irgb-green color)]
         [blue-component(irgb-blue color)]
         [Chroma (chroma(red-component green-component blue-component))])
      (/(- green-component blue-component) Chroma))))
     
;greenish-hue

(define greenish-hue
  (lambda (color)
   (let* ([red-component (irgb-red color)]
         [green-component(irgb-green color)]
         [blue-component(irgb-blue color)]
         [Chroma (chroma red-component green-component blue-component)])
     (+ (/(- blue-component red-component) Chroma) 2))))
  
;bluish-hue     
(define blueish-hue
  (lambda (color)
   (let* ([red-component (irgb-red color)]
         [green-component(irgb-green color)]
         [blue-component(irgb-blue color)]
         [Chroma (chroma red-component green-component blue-component)])
     (+ (/(- red-component green-component) Chroma) 4))))
            
;c)

(define irgb->hue-angle 
  (lambda (irgb)
    ( (let* ([red-component (irgb-red irgb)]
             [green-component(irgb-green irgb)]
             [blue-component(irgb-blue irgb)]
             [maximum (max red-component green-component blue-component)]
             [minimum (min red-component green-component blue-component)]
             [Chroma (- maximum minimum)]
             [Hue (cond ((= maximum red-component) (reddish-hue irgb))
                        ((= maximum green-component) (greenish-hue irgb))
                        ((= maximum blue-component) (blueish-hue irgb)))])
        
        (if ( > Hue 0) 
            (modulo (* Hue 60) 360)
            (modulo (*(+ Hue 6) 60) 360))))))
             

     
    
;Problem 3: Changing Colors

;a

(define irgb-change-hue
  (lambda (irgb-color hue)
     (hsv->irgb (cons (/ hue 60) (cdr (irgb->hsv irgb-color))))))

;b) 

(define irgb-true-complement 
  (lambda (irgb-color)
    (let* ([color-HSV  (irgb->hsv irgb-color)]
           [withoutHue cdr color-HSV])
      (hsv->irgb(cons (modulo (* 180 (car color-HSV)) 360 ) withoutHue)))))

;Problem 4: Hue-Based Transforms

;a

(define irgb-rotate-hue 
  (lambda (irgb-color angle)
    ( let*([hue (irgb->hue irgb-color)]
           [rotated-hue (modulo (+ hue angle) 360)]
           [saturation (irgb->saturation irgb-color)] 
           [value (irgb->value irgb-color)]
           )
       (hsv->irgb (list rotated-hue saturation value))
       )))

;b
(define rotate-image-hue
  (lambda (image)
    (image-variant image ((lambda irgb-color) (irgb-rotate-hue irgb-color 30)))
    ))


;Problem 5

;Procedure  : irgb-transform-hue
;Parameters : irgb-color, the color to be transformed
;           : angle, the angle to shift the hue by
;Process    : changes the hue of the color conditionally
;Precondition: irgb-color is an irgb selected color
;Postcondition: if (random 3) 1 ; rotate hue by 10
;               if (random 3) 2  ; rotate hue by 20
;               if (random 3) 3 ; rotate hue by 30


(define irgb-transform-hue
  (lambda (irgb-color angle)
    ( (let ([Random (random 3)])
        (cond  [(= Random 3) (irgb-rotate-hue irgb-color 10)]
               [ (= Random 2) (irgb-rotate-hue irgb-color 20)]
               [  (= Random 1) (irgb-rotate-hue irgb-color 20)])))))

       

     

