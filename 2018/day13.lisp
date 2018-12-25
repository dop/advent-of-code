(ql:quickload '(:rutils :optima))
(cl:defpackage #:aoc-2018-day13 (:use #:cl #:optima #:dop))
(cl:in-package #:aoc-2018-day13)

(deftype direction ()
  '(member left right top bottom))

(deftype track ()
  '(member horizontal vertical slash backslash cross empty))

(defstruct cart
  (direction 'left :type direction)
  (turn-order (list 'left 'keep 'right))
  (x 0 :type integer)
  (y 0 :type integer))

(defun direction-delta (direction)
  (ecase direction
    (left (cons -1 0))
    (right (cons 1 0))
    (top (cons 0 -1))
    (bottom (cons 0 1))))

(defun update-cart (cart direction)
  (destructuring-bind (x . y) (direction-delta direction)
    (setf (cart-x cart) (+ (cart-x cart) x)
          (cart-y cart) (+ (cart-y cart) y)
          (cart-direction cart) direction)))

(defun turn-cart (cart rules)
  (let ((direction (cdr (assoc (car (cart-turn-order cart)) rules))))
    (update-cart cart direction)
    (setf (cart-turn-order cart) (dop:rotate (cart-turn-order cart)))))

(defun move-cart (cart track)
  (let ((direction (cart-direction cart)))
    (match (cons direction track)
      ((cons (or 'left 'right) 'horizontal)
       (update-cart cart direction))

      ((cons (or 'top 'bottom) 'vertical)
       (update-cart cart direction))

      ((cons 'left 'slash)
       (update-cart cart 'bottom))

      ((cons 'left 'backslash)
       (update-cart cart 'top))

      ((cons 'bottom 'backslash)
       (update-cart cart 'right))

      ((cons 'bottom 'slash)
       (update-cart cart 'left))

      ((cons 'right 'slash)
       (update-cart cart 'top))

      ((cons 'right 'backslash)
       (update-cart cart 'bottom))

      ((cons 'top 'slash)
       (update-cart cart 'right))

      ((cons 'top 'backslash)
       (update-cart cart 'left))

      ((cons 'left 'cross)
       (turn-cart cart '((left . bottom) (right . top) (keep . left))))

      ((cons 'right 'cross)
       (turn-cart cart '((left . top) (right . bottom) (keep . right))))

      ((cons 'top 'cross)
       (turn-cart cart '((left . left) (right . right) (keep . top))))

      ((cons 'bottom 'cross)
       (turn-cart cart '((left . right) (right . left) (keep . bottom))))

      (otherwise
       (error "Nonsense move. Direction ~a, track ~a." direction track))))
  cart)

(defparameter *tracks*
  (loop
    :with y := 0 :and x := 0
    :and tracks := (make-array '(150 150) :initial-element 'empty :element-type 'track)
    :and carts := nil
    :for c :across (rutils:slurp "day13.txt")
    :maximize x :into width
    :maximize y :into height
    :do (if (char= #\newline c)
            (progn
              (incf y)
              (setf x 0))
            (progn
              (let ((cart (case c
                            (#\> (make-cart :direction 'right :y y :x x))
                            (#\< (make-cart :direction 'left :y y :x x))
                            (#\^ (make-cart :direction 'top :y y :x x))
                            (#\v (make-cart :direction 'bottom :y y :x x)))))
                (when cart (push cart carts)))
              (setf (aref tracks y x)
                    (case c
                      (#\- 'horizontal)
                      (#\| 'vertical)
                      (#\\ 'backslash)
                      (#\/ 'slash)
                      (#\+ 'cross)
                      (#\> 'horizontal)
                      (#\< 'horizontal)
                      (#\^ 'vertical)
                      (#\v 'vertical)
                      (t   'empty)))
              (incf x)))
    :finally
       (return (list (nreverse carts) tracks))))

(defun by-cart-coords (a b)
  (or (< (cart-y a) (cart-y b))
      (and (= (cart-y a) (cart-y b))
           (< (cart-x a) (cart-x b)))))

(destructuring-bind (ocarts tracks) *tracks*
  (loop
    :with carts := (mapcar #'copy-structure ocarts)
    :named main
    :for i :from 0 :to 1000
    :do
       (loop
         :for cart :in (sort (copy-list carts) #'by-cart-coords)
         :do
            (move-cart cart (aref tracks (cart-y cart) (cart-x cart)))
            (loop :for (cart . rest) :on carts
                  :when (find-if (lambda (other)
                                   (and (= (cart-x cart) (cart-x other))
                                        (= (cart-y cart) (cart-y other))))
                                 rest)
                    :do (return-from main cart)))
    :finally
       (format t "~a~%" carts)))

;; => #S(CART :DIRECTION BOTTOM :TURN-ORDER (LEFT KEEP RIGHT) :X 118 :Y 66)

(destructuring-bind (ocarts tracks) *tracks*
  (loop
    :with carts := (mapcar #'copy-structure ocarts)
    :named main
    :for i :from 0
    :do
       (loop
         :for cart :in (sort (copy-list carts) #'by-cart-coords)
         :do
            (move-cart cart (aref tracks (cart-y cart) (cart-x cart)))
            (loop :named removal :for (cart . rest) :on carts :do
              (let ((other (find-if (lambda (other)
                                      (and (= (cart-x cart) (cart-x other))
                                           (= (cart-y cart) (cart-y other))))
                                    rest)))
                (when other
                  (setf carts (remove other (remove cart carts)))))))
       (when (= 1 (length carts))
         (return-from main (list i carts)))))

;; => (12630 (#S(CART :DIRECTION BOTTOM :TURN-ORDER (KEEP RIGHT LEFT) :X 70 :Y 129)))


(defun track-to-char (track)
  (ecase track
    (empty #\ )
    (cross #\+)
    (vertical #\|)
    (horizontal #\-)
    (slash #\/)
    (backslash #\\)))

(defun cart-to-char (cart)
  (ecase (cart-direction cart)
    (left #\<)
    (right #\>)
    (top #\^)
    (bottom #\v)))

(defun print-tracks (tracks carts)
  (let ((board (dop:amap tracks (lambda (x y) (track-to-char (aref tracks x y))))))
    (loop
      :for cart :in carts
      :do (setf (aref board (cart-y cart) (cart-x cart))
                (cart-to-char cart)))
    (loop :for i :from 0 :below (array-dimension board 0) :do
      (loop :for j :from 0 :below (array-dimension board 1) :do
        (format t "~a" (aref board i j)))
      (format t "~%"))))

(print-tracks (second *tracks*) (first *tracks*))
