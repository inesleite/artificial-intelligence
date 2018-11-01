
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
(load "datastructures.fas")
(load "auxfuncs.fas")

(defun isObstaclep (pos track)
	"check if there is an obstacle at position pos of the track"
	(let* ((l (nth 0 pos))                     ;getting the number of the line in the track 'matrix'
		   (c (nth 1 pos))                     ;getting the number of the column in the track 'matrix'
		   (mytrack (track-env track))         ;getting the track
	)	
	(if 
    	(nth c (nth l mytrack))                ;checking whats in the position received (nth gives the -nth element of a list)
    NIL t)
))


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
        ((isGoalp newState) (setf (state-cost newState) -100)) ;if we reach the finish line
        ((isObstaclep (state-pos newState) track) (setf (state-cost newState) 20)) ; if we get out of the racing field
    )
    newState
))   
