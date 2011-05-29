(defvar *default-max-size* 20)

(defun gen-number (&key (start 0) (end 100))
  "Generate random number from start to end"
  (+ start (random (- (1+ end) start))))

(deftest fail-num-test ()
  "Test basic number operations properties"
  (check-for-all ((a (gen-number 5 10))
		  (b (gen-number -5 5)))
    (= (+ a b) (+ b a))
    (= (- a b) (- b a))
    ))

(defun gen-list (generator &optional (max-size *default-max-size*))
  "Generate random list containig values from generator.
Usage examples:
(gen-list #'gen-number)
(gen-list (lambda () (gen-number :start -10 :end 10)))"
  (loop repeat (random max-size)
     collect (funcall generator)))

(defun shrink-list (l)
  "Return list, containing list l without last element"
  (list (reverse (cdr (reverse l)))))

(defun gen-char (&key (start #\a) (end #\z))
  "Generate random character"
  (code-char (gen-number :start (char-code start) :end (char-code end))))

(defun gen-string (&optional (max-size *default-max-size*))
  "Generate random string"
  (coerce (loop repeat (random max-size)
                collect (gen-char))
          'string))

(defun shrink-string (s)
  "Return list, containing string s without first character and s without last character"
  (list (subseq s 0 (1- (length s))) (subseq s 1)))

(defun reverse-list-fail (l)
  "Reverse function, that fails on lists containing more than tho elements"
  (append (cdr l) (list (car l))))

(deftest fail-list-test ()
  (check-for-all ((l (gen-list #'gen-number) (shrink-list)))
    (equalp l (reverse-list-fail (reverse-list-fail l)))))