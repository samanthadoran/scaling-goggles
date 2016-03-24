(in-package :cl-user)

(defpackage #:scaling-goggles
  (:nicknames #:soggles)
  (:use :cl :cl-user)
  (:export #:print-pretty #:player #:make-player #:player-scores #:player-feats
    #:player-inventory #:player-equipment #:make-feat-expression #:make-default-player))

(in-package :scaling-goggles)

(defstruct player
  scores
  feats
  inventory
  equipment)

(defun print-pretty ())

(defun make-default-player ()
  (let ((s (make-player)))
    (setf (player-scores s)(pairlis
      (list "strength" "dexterity" "constitution" "intelligence" "wisdom" "charisma")
      (list 10 10 10 10 10 10)))
  s))

(defun make-feat-expression (score bonus class-skill player)
  "Creates a quoted expression representing a feat for a given player"
  `(if (and ,class-skill (> ,score 0))
    (+ ,score (cdr (assoc ,bonus (player-scores ,player) :test #'string=)) 3)
    (+ ,score (cdr (assoc ,bonus (player-scores ,player) :test #'string=)))))
