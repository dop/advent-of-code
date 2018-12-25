(defconstant *stats*
  '((13   4147    642)
    (12   6989    976)
    (11   9234    809)
    (10  10248     39)
    (9  10223    825)
    (8  12027    647)
    (7  12531   2904)
    (6  15665    686)
    (5  22953    860)
    (4  22880    805)
    (3  31795   1696)
    (2  41887   4727)
    (1  53012  13830)))

(sort (loop
        :for (day gold silver) :in *stats*
        :collect (list day (* 1.0 (/ gold (+ gold silver)))))
      #'< :key #'second)

((1 0.79309416)
 (7 0.81185615)
 (13 0.8659428)
 (12 0.8774639)
 (2 0.8985927)
 (11 0.9194464)
 (9 0.9253259)
 (8 0.9489506)
 (3 0.94935954)
 (6 0.95804536)
 (5 0.96388525)
 (4 0.96601224)
 (10 0.9962088))

((1 0.79328156)
 (7 0.81194866)
 (12 0.8124183)
 (2 0.8981657)
 (11 0.9101314)
 (9 0.92212576)
 (8 0.94850415)
 (3 0.9497183)
 (6 0.95875704)
 (5 0.963584)
 (4 0.9661171)
 (10 0.9963096))


((1 0.7932)
 (7 0.81025606)
 (11 0.8362903)
 (2 0.8979619)
 (9 0.9199638)
 (8 0.9447637)
 (3 0.9489176)
 (6 0.9577145)
 (5 0.9638299)
 (4 0.96504736)
 (10 0.9961913))

((9 0.7606004)
 (1 0.7932658)
 (7 0.79434466)
 (2 0.8979486)
 (8 0.94222665)
 (3 0.94857806)
 (6 0.9538728)
 (5 0.9620643)
 (4 0.9654843))

((9 0.6940534)
 (7 0.79292846)
 (1 0.79296124)
 (2 0.89797795)
 (8 0.9398314)
 (3 0.9482834)
 (6 0.95398253)
 (5 0.9624009)
 (4 0.9655985))
