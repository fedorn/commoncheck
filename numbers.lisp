(defun gen-number (start end)
  (+ start (random (- (1+ end) start))))

(deftest failtest ()
  (check-for-all ((a (gen-number 5 10))
		  (b (gen-number -5 5)))
    (= (+ a b) (+ b a))
    (= (- a b) (- b a))
    ))