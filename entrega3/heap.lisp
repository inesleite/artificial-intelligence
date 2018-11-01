(defun heapsort (lst)
	(let length (list-length lst)
	(buildminheap(lst))
	(loop for i from length to 2 do
		(setf (nth 1 lst) (nth i lst))
		(setf heapsize (- heapsize 1))
		(min-heapify lst 1)
	)
))


(defun buildminheap (lst)
  (let ((middle (/ heapsize 2)))
  (loop for i from middle to 1 do
      (min-heapify lst i)
  )
))

(defun min-heapify (lst i)
  (let* ((left (nth (i-1) lst)))
  		(right (nth (i+1) lst))
  		(heapsize (list-length lst))
  		(smallest 0)

  (if (and (< left heapsize) (< ((nth left lst) (nth i lst)))) do
  	(setf smallest left)
  	(setf smallest i)
  )

  (if (and (< right heapsize) (< ((nth right lst) (nth smallest lst)))) do
  	(setf smallest right)
  )
  (cond (not (eq smallest i))
  	(setf (nth i lst) (nth smallest lst))
	(min-heapify lst smallest)
  )

))