;; -*- lexical-binding: t; -*-

(load (expand-file-name "lisp/my-library.el" user-emacs-directory))
(load (expand-file-name "aoc.el" default-directory))
(load (expand-file-name "heap.el" default-directory))
(load (car (file-expand-wildcards
            (expand-file-name "elpa/queue-*/queue.el" user-emacs-directory))))

(with-puzzle "day24.txt"
  (let ((from 200000000000000)
        (to 400000000000000)
        ;; (from 7)
        ;; (to 27)
        (hail (seq-partition
               (mapcar #'string-to-number (string-split (buffer-string) "[\n @,]" t))
               6)))
    (cl-labels
        ((intersectsp (line1 line2)
           (pcase-let ((`((,x1 ,y1) (,x2 ,y2)) line1)
                       (`((,x3 ,y3) (,x4 ,y4)) line2))
             (let ((d
                    (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4))))
                   (xn
                    (- (* (- (* x1 y2) (* y1 x2))
                          (- x3 x4))
                       (* (- x1 x2) (- (* x3 y4) (* y3 x4)))))
                   (yn
                    (- (* (- (* x1 y2) (* y1 x2))
                          (- y3 y4))
                       (* (- y1 y2) (- (* x3 y4) (* y3 x4))))))
               (unless (= 0 d)
                 (list (/ (float xn) d)
                       (/ (float yn) d)))))))
      (loop for ((x y _ dx dy _) . rest) on hail
            sum (loop for (xx yy _ dxx dyy _) in rest
                      count (pcase (intersectsp (list (list x y) (list (+ x dx) (+ y dy)))
                                                (list (list xx yy) (list (+ xx dxx) (+ yy dyy))))
                              (`(,px ,py)
                               ;; (pri x y xx yy px py)
                               (and (<= from px to) (<= from py to)
                                    (if (< 0 dx) (> px x) (< px x))
                                    (if (< 0 dxx) (> px xx) (< px xx))
                                    (if (< 0 dy) (> py y) (< py y))
                                    (if (< 0 dyy) (> py yy) (< py yy)))))))))) ;; 20434

;; Hailstone A: 19, 13, 30 @ -2, 1, -2
;; Hailstone B: 18, 19, 22 @ -1, -1, -2
;; Hailstones' paths will cross inside the test area (at x=14.333, y=15.333).

;; x_i*dy_i - x*dy_i - x_i*dy + y*dx_i - y_i*dx_i + y_i*dx = y*dx - x*dy

;; x_0*dy_0 - x*dy_0 - x_0*dy + y*dx_0 - y_0*dx_0 + y_0*dx = y*dx - x*dy
;; x_1*dy_1 - x*dy_1 - x_1*dy + y*dx_1 - y_1*dx_1 + y_1*dx = y*dx - x*dy

;; x                y                z                 dx  dy  dz
;; 260346828765750, 357833641339849, 229809969824403 @ 64, -114, 106
;; 340220726383465, 393110064924024, 226146987100003 @ -79, -61, 158
;; 11361697274707, 101596061919750, 46099495948720 @ 328, 162, 333

(with-puzzle "day24.txt"
  (let ((numbers (mapcar #'string-to-number (string-split (buffer-string) "[\n @,]" t))))
    (cl-destructuring-bind (x y z dx dy dz)
        (loop for (xn yn zn dxn dyn dzn) in (seq-partition numbers 6)
              collect xn into x
              collect yn into y
              collect zn into z
              collect dxn into dx
              collect dyn into dy
              collect dzn into dz
              finally (return (list x y z dx dy dz)))

      ;; x y dx dy

      ;; x_i*dy_i - x_i*dy - x*dy_i + y*dx_i + y_i*dx - y_i*dx_i
      ;; - x_j*dy_j + x*dy_j + x_j*dy - y*dx_j + y_j*dx_j - y_j*dx
      ;; =
      ;; 0

      ;; - x_i*dy (- x_j x_i)
      ;; + x_j*dy
      ;; - x*dy_i (- dy_j dy_i)
      ;; + x*dy_j
      ;; + y*dx_i (- dx_i dx_j)
      ;; - y*dx_j
      ;; + y_i*dx (- y_i y_j)
      ;; - y_j*dx
      ;; =
      ;; - x_i*dy_i
      ;; + y_i*dx_i
      ;; + x_j*dy_j
      ;; - y_j*dx_j

      (cl-destructuring-bind (m1 m2)
          (collecting (m1 m2)
            (loop repeat 4 for i from 0 do
                  (let* ((j (1+ i))
                         (x_i (elt x i))
                         (y_i (elt y i))
                         (dx_i (elt dx i))
                         (dy_i (elt dy i))
                         (x_j (elt x j))
                         (y_j (elt y j))
                         (dx_j (elt dx j))
                         (dy_j (elt dy j)))
                    (m1 (- dy_j dy_i) (- dx_i dx_j) (- y_i y_j) (- x_j x_i)
                        (+ (* x_j dy_j) (- (* y_j dx_j)) (- (* x_i dy_i)) (* y_i dx_i)))))

            (loop repeat 4 for i from 0 do
                  (let* ((j (1+ i))
                         (x_i (elt x i))
                         (z_i (elt z i))
                         (dx_i (elt dx i))
                         (dz_i (elt dz i))
                         (x_j (elt x j))
                         (z_j (elt z j))
                         (dx_j (elt dx j))
                         (dz_j (elt dz j)))
                    (m2 (- dz_j dz_i) (- dx_i dx_j) (- z_i z_j) (- x_j x_i)
                        (+ (* x_j dz_j) (- (* z_j dx_j)) (- (* x_i dz_i)) (* z_i dx_i))))))

        (let ((mat1 (matrix 4 5 (seq-into m1 'vector)))
              (mat2 (matrix 4 5 (seq-into m2 'vector))))

          (gaussian-elimination mat1)
          (gaussian-elimination mat2)

          (+ (truncate (elt (matrix-data mat1) 4))
             (truncate (elt (matrix-data mat1) 9))
             (truncate (elt (matrix-data mat2) 9)))))))) ;; 1025127405449117
