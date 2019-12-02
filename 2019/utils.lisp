(in-package :advent-of-code-utils)

(defparameter *cookies*
  (list
   (make-instance
    'cookie :name "session"
            :value ""
            :domain ".adventofcode.com")))

(defparameter *cookie-jar*
  (make-instance 'cookie-jar :cookies *cookies*))

(defun get-input-for-day (day)
  (declare (type integer day))
  (let ((day-input-filename (format nil "day~2,'0D.txt" day)))
    (unless (uiop:file-exists-p day-input-filename)
      (multiple-value-bind (body status)
          (http-request (format nil "https://adventofcode.com/2019/day/~D/input" day)
                        :want-stream t
                        :cookie-jar *cookie-jar*)
        (if (= status 200)
            (overwrite-file day-input-filename body)
            (throw :request-failed status))))
    (rutils:slurp day-input-filename)))

(defun overwrite-file (filename stream)
  (alexandria:with-output-to-file (out filename :if-exists :supersede :if-does-not-exist :create)
    (pipe-stream stream out)))

(defun pipe-stream (in out &optional (buffer-size 1024))
  (let ((buffer (make-array buffer-size :element-type 'character)))
    (loop :for chars-read := (read-sequence buffer in)
          :do (write-sequence buffer out :end chars-read)
          :while (plusp chars-read))))

(defun array-of-list (list)
  (make-array (length list) :initial-contents list))

(defun rotate (seq &optional (n 1))
  "Safe rotate. Does not modify original sequence. Takes items from beginning of
sequence and pushes to the back. If N is negative, takes items from the end and
puts then in the front."
  (when seq
    (let ((l (length seq)))
      (if (or (zerop n) (zerop l))
          seq
          (let ((k (rem n l)))
            (cond
              ((zerop k) seq)
              ((plusp k)
               (concatenate (type-of seq)
                            (subseq seq k)
                            (subseq seq 0 k)))
              ((minusp k)
               (concatenate (type-of seq)
                            (subseq seq (+ l k))
                            (subseq seq 0 (+ l k))))))))))

(defun first-key (dimensions)
  (make-list (length dimensions) :initial-element 0))

(defun next-key (current-key dimensions)
  (let ((next-key (copy-seq current-key)))
    (loop :for i :from (1- (length dimensions)) :downto 0
          :do (if (< (elt current-key i) (1- (elt dimensions i)))
                  (progn
                    (incf (elt next-key i))
                    (return next-key))
                  (setf (elt next-key i) 0)))))

(defun each (arr fn)
  "Calls FN with every index of ARR. FN must accepts as many arguments as ARR
has dimensions."
  (loop
    :with dimensions := (array-dimensions arr)
    :for current-key := (first-key dimensions) :then (next-key current-key dimensions)
    :while current-key
    :do (apply fn current-key)))

(defun amapi (arr fn)
  "Calls FN with every index of ARR, creates and returns new array with values returned by FN."
  (let ((arr2 (make-array (array-dimensions arr))))
    (each arr (lambda (&rest args)
                (setf (apply #'aref arr2 args) (apply fn args))))
    arr2))

(defun namapi (arr fn)
  "Calls FN with every index of ARR, overwrites original array cells with values returned by FN."
  (each arr (lambda (&rest args)
              (setf (apply #'aref arr args) (apply fn args))))
  arr)

(defun namap (arr fn)
  "Destructively maps FN over ARR."
  (each arr (lambda (&rest args)
              (setf (apply #'aref arr args)
                    (funcall fn (apply #'aref arr args)))))
  arr)

(defun amap (arr fn)
  "Maps FN over ARR, returns new array."
  (let ((arr2 (make-array (array-dimensions arr))))
    (each arr (lambda (&rest args)
                (setf (apply #'aref arr2 args)
                      (funcall fn (apply #'aref arr args)))))
    arr2))
