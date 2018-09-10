

(defmethod enqueue (val (q queue))
  (make-queue :q (cons val (queue-q q)) :d (queue-d q)))