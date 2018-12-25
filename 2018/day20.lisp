;; (ql:quickload '(:rutils :optima :fset))

(cl:defpackage #:aoc201820
  (:use #:cl #:rutils #:optima)
  (:import-from #:alexandria #:rcurry #:curry #:compose))

(cl:in-package #:aoc201820)

(defstruct stack
  (elements (make-array 15000 :element-type 'character :fill-pointer 0) :type vector))

(defun copy-vector (vector)
  (map-into (make-array (array-dimensions vector)
                        :adjustable (adjustable-array-p vector)
                        :fill-pointer (fill-pointer vector))
            #'copy-tree
            vector))

(defun stack-copy (stack)
  (let ((new-stack (copy-stack stack)))
    (setf (stack-elements new-stack)
          (copy-vector (stack-elements stack)))
    new-stack))

(defun stack-empty-p (stack)
  (zerop (length (stack-elements stack))))

(defun stack-pop (stack)
  (unless (stack-empty-p stack)
    (vector-pop (stack-elements stack))))

(defun stack-push (stack element)
  (vector-push element (stack-elements stack)))

(defun prepare (input)
  (make-stack :elements (make-array (length input)
                                    :initial-contents (nreverse (copy-seq input))
                                    :fill-pointer (length input)
                                    :adjustable t)))

(defun choice-p (c)
  (char= c #\|))

(defun open-p (c)
  (char= c #\())

(defun close-p (c)
  (char= c #\)))

(defun direction-p (c)
  (member c '(#\N #\E #\W #\S)))

(defun parse-group (stack)
  (loop
    :with groups := nil :and group := nil
    :for c := (stack-pop stack) :then (stack-pop stack)
    :do
       (cond
         ((open-p c)
          (when-it (parse-group stack)
            (pushnew it group)))
         ((direction-p c)
          (push c group))
         ((choice-p c)
          (pushnew (nreverse group) groups)
          (setf group nil))
         ((close-p c)
          (pushnew (nreverse group) groups)
          (return (nreverse groups))))))

(defun parse-regex (stack)
  (loop
    :with result := nil
    :for c := (stack-pop stack) :then (stack-pop stack)
    :while c
    :do
       (cond
         ((direction-p c) (push c result))
         ((open-p c)
          (when-it (parse-group stack)
            (push it result))))
    :finally
       (return (nreverse result))))

(defun parse (input)
  (parse-regex (prepare input)))

(defun choices (path choice)
  (cond ((characterp choice)
         (stack-push path choice)
         (list path))
        ((consp choice)
         (concat (mapcar (lambda (group)
                           (loop
                             :with paths := (list (stack-copy path))
                             :for e :in group :do
                               (setf paths (concat (mapcar (rcurry #'choices e) paths)))
                             :finally
                                (return paths)))
                         choice)))))

(defun concat (lists)
  (apply #'concatenate 'list lists))

(defun -> (x1 y1 x2 y2)
  (list x1 y1 x2 y2))

(defun edges->neighbour-map (edges)
  (loop
    :with neighbours := (fset:map)
    :for (x1 y1 x2 y2) :in (fset:convert 'list edges) :do
      (let* ((from (cons x1 y1))
             (nodes (fset:lookup neighbours from)))
        (setf neighbours (fset:with neighbours from (cons (cons x2 y2) nodes))))
    :finally
       (return neighbours)))

(defun edges->nodes (edges)
  (loop
    :for (x1 y1 x2 y2) :in (fset:convert 'list edges)
    :collect (cons x1 y1) :into nodes
    :collect (cons x2 y2) :into nodes
    :finally
       (return (fset:convert 'fset:set nodes :from-type 'list))))

(defun edges->distances-from (edges source)
  (let* ((path (fset:empty-map))
         (neighbours (edges->neighbour-map edges))
         (unvisited (edges->nodes edges))
         (distances (make-hash-table :test 'equal)))
    (setf (gethash source distances) 0)
    (labels ((pop-closest-node ()
               (loop
                 :with min-distance :and min-node
                 :for node :being :the :hash-keys :of distances :using (:hash-value distance) :do
                   (when (and (fset:contains? unvisited node)
                              (or (not min-distance) (< distance min-distance)))
                     (setf min-distance distance
                           min-node node))
                 :finally
                    (setf unvisited (fset:less unvisited min-node))
                    (return (values min-node min-distance)))))
      (loop :until (fset:empty? unvisited) :do
        (multiple-value-bind (node distance) (pop-closest-node)
          (loop :for neighbour :in (fset:lookup neighbours node) :do
            (let ((new-distance-to-neighbour (1+ distance))
                  (old-distance-to-neighbour (gethash neighbour distances)))
              (unless (and old-distance-to-neighbour
                           (< old-distance-to-neighbour new-distance-to-neighbour))
                (setf (gethash neighbour distances) new-distance-to-neighbour
                      path (fset:with path neighbour node))))))))
    (values distances path)))

(defun distance-to (distances target)
  (gethash target distances))

(defun adjacent (x y)
  (let (adjacent)
    (loop :for j :from (1- y) :to (1+ y) :do
      (loop :for i :from (1- x) :to (1+ x) :do
        (when (not (and (= i x) (= j y)))
          (push (cons i j) adjacent))))
    adjacent))

(defun print-maze (rx)
  (let ((edges (all-edges rx)))
    (multiple-value-bind (width height dx dy)
        (loop :for (x . y) :in (fset:convert 'list (edges->nodes edges))
              :maximize x :into right
              :minimize x :into left
              :maximize y :into bottom
              :minimize y :into top
              :finally (return (values (+ 3 (* 2 (- right left)))
                                       (+ 3 (* 2 (- bottom top)))
                                       (* -2 left)
                                       (* -2 top)
                                       right bottom)))
      ;; (format t "~a~%" (list width height dx dy right bottom))
      (let ((board (make-array (list height width) :initial-element #\█)))
        (loop :for (x1 y1 x2 y2) :in (fset:convert 'list edges) :do
          (let ((tx1 (1+ (+ dx (* 2 x1))))
                (ty1 (1+ (+ dy (* 2 y1))))
                (ty2 (1+ (+ dy (* 2 y2))))
                (tx2 (1+ (+ dx (* 2 x2)))))
            ;; (format t "~a ~a~%" (list tx1 ty1 '-> tx2 ty2)
            ;;         (list (+ (/ (- tx2 tx1) 2) tx1)
            ;;               (+ (/ (- ty2 ty1) 2) ty1)))
            (setf (aref board ty1 tx1) #\space)
            (setf (aref board
                        (+ (/ (- ty2 ty1) 2) ty1)
                        (+ (/ (- tx2 tx1) 2) tx1))
                  (if (= ty2 ty1) #\| #\-))
            (setf (aref board ty2 tx2) #\space)))
        (loop :for y :from 0 :below height :do
          (loop :for x :from 0 :below width :do
            (format t "~a" (aref board y x)))
          (format t "~%"))))))

;; (print-maze (parse "ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN"))
;; ███████████
;; █...█...█.█
;; █.███.█.█.█
;; █.....█.█.█
;; █.█████.█.█
;; █.█.█...█.█
;; █.█.█████.█
;; █.█.......█
;; █.███.███.█
;; █.....█...█
;; ███████████

;; (print-maze (parse "ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))"))
;; █████████████
;; █...........█
;; █.█████.███.█
;; █.█...█.█.█.█
;; █.█.███.█.█.█
;; █.█.█...█...█
;; █.█.█.█████.█
;; █.█.█.█...█.█
;; █.█.█.███.█.█
;; █...█...█.█.█
;; ███.█.███.█.█
;; █...█.....█.█
;; █████████████

;; (print-maze (parse "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))"))
;; ███████████████
;; █.......█.....█
;; █.███.███.█.█.█
;; █...█.....█.█.█
;; █.█████████.█.█
;; █.█.........█.█
;; █.█.█████████.█
;; █.█.█...█...█.█
;; ███.█.███.█.█.█
;; █...█.█...█...█
;; █.███.█████.███
;; █...█.....█.█.█
;; █.█.█████.█.█.█
;; █.█.......█...█
;; ███████████████

(defun oposite-p (a b)
  (match (cons a b)
    ((cons #\E #\W) t)
    ((cons #\W #\E) t)
    ((cons #\N #\S) t)
    ((cons #\S #\N) t)
    (otherwise nil)))

(defun longest-path (rx)
  (cond ((characterp (car rx))
         (if (and (second rx)
                  (characterp (second rx))
                  (oposite-p (first rx) (second rx)))
             nil
             (when-it (longest-path (cdr rx))
               (1+ it))))
        ((consp (car rx))
         (let ((paths (remove-if #'null (mapcar #'longest-path (car rx)))))
           (+ (apply #'max paths) (longest-path (cdr rx)))))
        (t 0)))

;; Why did this not work?
(defun door-count (rx &optional (skip 0))
  (cond ((characterp (car rx))
         (if (and (second rx)
                  (characterp (second rx))
                  (oposite-p (first rx) (second rx)))
             (if (> skip 0) 0 1)
             (+ (if (> skip 0) 0 1) (door-count (cdr rx) (1- skip)))))
        ((consp (car rx))
         (+ (apply #'+ (mapcar (rcurry #'door-count skip) (car rx)))
             (door-count (cdr rx) skip)))
        (t 0)))

(defun move (point direction)
  (let* ((x (car point))
         (y (cdr point)))
    (ecase direction
      (#\W (decf x))
      (#\E (incf x))
      (#\N (decf y))
      (#\S (incf y)))
    (cons x y)))

;; Can draw loops.
(defun all-edges-1 (rx)
  (let ((queue (list (list (cons 0 0) rx)))
        (edges (fset:empty-set))
        (times 0))
    (loop :while queue :do
      (incf times)
      (destructuring-bind (point rules) (pop queue)
        ;; (format t "queue size: ~a, point: ~a~%" (length queue) point)
        (let ((rule (car rules))
              (rest (cdr rules)))
          (cond
            ((characterp rule)
             (let ((next-point (move point rule)))
               (destructuring-bind ((x1 . y1) (x2 . y2)) (list point next-point)
                 (setf edges (fset:with edges (-> x1 y1 x2 y2))))
               (when rest (push (list next-point rest) queue))))
            ((consp rule)
             (loop :for choice :in rule :do
               (if-let (next-rules (if choice (append choice rest) rest))
                 (push (list point next-rules) queue)))))
          ;; (format t "~a~%" (mapcar (lambda (q) (subseq q 0 (min 3 (length q)))) queue))
          ;; (break)
          )))
    (values edges times)))

(defun all-edges (rules)
  (all-edges-aux (cons 0 0) rules))

(defun all-edges-aux (point rules)
  (let ((rule (car rules))
        (rest (cdr rules)))
    (cond
      ((characterp rule)
       (let ((next-point (move point rule)))
         (destructuring-bind ((x1 . y1) (x2 . y2)) (list point next-point)
           (fset:with (all-edges-aux next-point rest)
                      (-> x1 y1 x2 y2)))))
      ((consp rule)
       (reduce #'fset:union (mapcar (curry #'all-edges-aux point) rule)
               :initial-value (all-edges-aux point rest)))
      (t
       (fset:empty-set)))))

;; (defparameter *maze* (string-trim '(#\$ #\^ #\newline) (slurp "day20.txt")))

(loop :for (node distance) :in (let* ((edges (all-edges (parse *maze*)))
                                      (distances (edges->distances-from edges (cons 0 0))))
                                 (loop :for node :in (fset:convert 'list (edges->nodes edges))
                                       :collect (list node (distance-to distances node))))
      :maximize distance :into dist
      :count (> distance 999) :into count
      :finally (return (list dist count))) ;; => (4108 8366)

(longest-path (parse *maze*)) ;; => 4108
