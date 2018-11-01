(load "datastructures.fas")
(load "auxfuncs.fas")
;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))
;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))
;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution of phase 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st) 
  "check if st is a goal state"
    (let* ((track (state-track st))
           (goalPositions (track-endpositions track))
           (statePos (state-pos st))
    )
    (loop for pos in goalPositions         ;looping through all the the list of all goal positions 
        when (equal pos statePos)          ;when we find the desired position
            return t
    )
    ;if we don't find the desired position returns NIL
))

;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act"
    (let* ((position (state-pos st))
           (velocity (state-vel st))
           (track (state-track st))
           (newPosX (+ (nth 0 position) (nth 0 velocity) (nth 0 act)))      ;calculating new position in x
           (newPosY (+ (nth 1 position) (nth 1 velocity) (nth 1 act)))      ;calculating new position in y
           (newVelX (+ (nth 0 velocity) (nth 0 act)))                       ;calculating new velocity in x
           (newVelY (+ (nth 1 velocity) (nth 1 act)))                       ;calculating new velocity in y
           (newState (make-state                                            ;creation of new state
                :pos (list newPosX newPosY)      ;creating a list with the new position
                :vel (list newVelX newVelY)      ;creating a list with the new velocity
                :action act
                :cost 1                          ;starting the default cost as if we only moved
                :track track
           ))
    ) 
    (cond 
        ((isGoalp newState) 						;if we reach the finish line
        	(setf (state-cost newState) -100)) 
        ((isObstaclep (state-pos newState) track)   ;if we get out of the racing field
        	(setf (state-cost newState) 20)) 
    )
    newState
))   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution of phase 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution of phase 2

;;; Pedir 
(defun nextStates (st)
  "generate all possible next states"
  (let ((successors nil))
    (dolist (act (possible-actions) successors)
      (let ((new-state (nextState st act)))
  (if (not (member new-state successors :test #'equalp))
      (push new-state successors))))))

;;; Solucao e uma seq ordenada de estados
(defun solution (node)
  (let ((seq-states nil))
    (loop 
      (when (null node)
  (return))
      (push (node-state node) seq-states)
      (setf node (node-parent node)))
    (values seq-states)))

;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim &key cutoff?)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit"
  (labels ((limdepthfirstsearch-aux (node problem lim)
       (if (isGoalp (node-state node))
     (solution node)
     (if (zerop lim)
         :cutoff
         (let ((cutoff? nil))
           (dolist (new-state (nextStates (node-state node)))
       (let* ((new-node (make-node :parent node :state new-state))
        (res (limdepthfirstsearch-aux new-node problem (1- lim))))
         (if (eq res :cutoff)
             (setf cutoff? :cutoff)
             (if (not (null res))
           (return-from limdepthfirstsearch-aux res)))))
           (values cutoff?))))))
    (let ((res (limdepthfirstsearch-aux (make-node :parent nil :state (problem-initial-state problem))
          problem
          lim)))
      (if (eq res :cutoff)
    (if cutoff?
        :cutoff
        nil)
    res))))
              
;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
  "limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations"
  (let ((i 0))
    (loop
      (let ((res (limdepthfirstsearch problem i :cutoff? T)))
  (when (and res (not (eq res :cutoff)))
    (return res))
  (incf i)
  (if (> i lim)
      (return nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Solution of phase 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Heuristic - Euclidean distance
(defun compute-heuristic (st)
	(let*  ((Q '())
			   (visited '())
			   (current_state st))
	(setf (state-other st) 0)
	(setf Q (cons st Q))
	(setf visited (cons st visited))
	(loop while (not (null Q)) do
		(setf current_state (pop Q))
		(loop for pos in (adjacent-pos current_state visited) do
			(let* ((new_state (make-state :pos pos 
						:track (state-track current_state)
			  			:other (+ 1 (state-other current_state)))))
			(setf Q (append Q (list new_state)))
			(setf visited (cons new_state visited))
			(if (isGoalp new_state)
				(return-from compute-heuristic (state-other new_state)))
	)))
	(return-from compute-heuristic most-positive-fixnum)
))
(defun pos-visited (pos list_states)
	(loop for state in list_states do
		(if (equal pos (state-pos state))
			(return-from pos-visited t))
	)
	(return-from pos-visited nil)
)
(defun adjacent-pos (state visited)
	(let* ((lst '())
			(pos (state-pos state))
			(l (nth 0 pos))
			(c (nth 1 pos)))
	(loop for action in (possible-actions) do
		(let ((new_pos (list (+ l (nth 0 action)) (+ c (nth 1 action)))))
		(if (and (not (isObstaclep new_pos (state-track state)))
			     (not (pos-visited new_pos visited)))
					(setf lst (cons new_pos lst)))
	))
	(return-from adjacent-pos lst)
))

;;; A*
(defun a* (problem)
	;(print (list-length (best-search problem)))
	;(print (best-search problem))
	(let*  ((state (problem-initial-state problem))
			(h (funcall (problem-fn-h problem) state))
		    (node (make-node :parent nil :state state
							 :g 0 :h h :f h))
			(list_nodes (list node)))
	(loop do
		(if (null list_nodes)
			(return-from a* nil))
		(setf node (pop list_nodes))
		(setf state (node-state node))
		(if (funcall (problem-fn-isGoal problem) state)
			(return-from a* (solution node))
		)
		(loop for new_state in (nextStates state) do
			(let*  ((g (+ (node-g node) (state-cost new_state)))
					(h (funcall (problem-fn-h problem) new_state))
				    (next_node (make-node :parent node :state new_state
									 	  :g g :h h :f (+ g h))))
			(setf list_nodes (cons next_node list_nodes))
        ))
        (setf list_nodes (oderer list_nodes))
)))
(defun oderer (lst)
	(cond 	((null lst) nil)
			(t (put-in-place (car lst) (oderer (cdr lst)))))
)
(defun put-in-place (elem lst)
	(cond	((null lst) (cons elem '()))
		  	((<= (node-f elem) (node-f (car lst))) (cons elem lst))
		  	(t (cons (car lst) (put-in-place elem (cdr lst)))))
)

;;solucao mais eficiente
(defun best-search (problem)
    (let*  ((state (problem-initial-state problem))
    		(h (funcall (problem-fn-h problem) state))
    		(result (bs-aux problem (make-node :state state :g 0 :h h :f h) most-positive-fixnum)))
	;(print (solution result))
    (return-from best-search (solution result))
))
(defun bs-aux (problem node max_f)
    (let* ((state (node-state node ))
           (next_states (funcall (problem-fn-nextStates problem) state))
           (successors '())
           (best) (alternative) (result))
    (if (funcall (problem-fn-isGoal problem) state)
    	(return-from bs-aux node))
    (dolist (new_state next_states)
        (let*  ((g (+ (state-cost new_state) (node-g node)))
        		(h (funcall (problem-fn-h problem) new_state) )
        		(new_node (make-node :parent node :state new_state 
                                	 :g g :h h :f (max (+ g h) (node-f node)))))
        (setf successors (cons new_node successors))
    ))
    (if (null successors) (return-from bs-aux nil))
    (setf successors (oderer successors))
    (loop while (not (null successors)) do
        (setf best (pop successors))
        (if (> (node-f best) max_f)
            (return-from bs-aux nil))
  	    (setf alternative (node-f (car successors)))
        (setf result (bs-aux problem best (min max_f alternative)))
        (if (not (eq result nil))
			(return-from bs-aux result)
        )
)))