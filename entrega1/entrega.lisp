(defun isObstaclep (pos track)
	"check if there is an obstacle at position pos of the track"
	(if searchPosInList (cdr pos) (searchPosInList (car pos)track ))
	NIL t)
)
;t)


(defun searchPosInList (pos lst)
	"returns element at position pis of the list lst"
	(if (zerop (- pos 1))
		(setq lst (car lst))
		(searchPosInList ( 1- pos) (cdr lst))
	)
)

(defun isGoal (state)
	"check if a state is a goal"
	)


(defun nextState (state action)
	"given a state and an action returns the state that results from applying the action to the state provided"
	)