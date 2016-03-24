(in-package :cl-user)

(defpackage #:scaling-goggles
  (:nicknames #:soggles)
  (:use :cl :cl-user)
  (:export #:print-pretty #:player #:make-player #:player-scores #:player-feats
    #:player-inventory #:player-equipment #:make-feat-expression #:make-default-player
    #:give-feat #:get-feat-val))

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

(defun make-feat-expression (name bonus score class-skill player)
  "Creates a quoted expression representing a feat for a given player"
  (pairlis (list "name" "class-skill" "bonus" "score" "value") (list name
  class-skill bonus score
  `(if (and
    (cdr(assoc "class-skill" (cdr(assoc ,name (player-feats ,player) :test #'string=)) :test #'string=))
    (>
      (cdr (assoc "score" (cdr (assoc ,name (player-feats ,player) :test #'string=)) :test #'string=))
      0))
      (+
        (cdr (assoc "score" (cdr (assoc ,name (player-feats ,player) :test #'string=)) :test #'string=))
        (floor (- (cdr (assoc ,bonus (player-scores ,player) :test #'string=)) 10) 2)
        3)
      (+
        (cdr (assoc "score" (cdr (assoc ,name (player-feats ,player) :test #'string=)) :test #'string=))
        (floor (- (cdr (assoc ,bonus (player-scores ,player) :test #'string=)) 10) 2))))))

(defun give-feat (feat player)
  (setf (player-feats player) (acons (cdr (assoc "name" feat :test #'string=)) feat (player-feats player))))

(defun get-feat-val (feat player)
  (cdr (assoc "value" (cdr (assoc feat (player-feats player) :test #'string=)) :test #'string=)))
