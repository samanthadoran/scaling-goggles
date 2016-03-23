(in-package :cl-user)

(defpackage #:scaling-goggles
  (:nicknames #:soggles)
  (:use :cl :cl-user)
  (:export #:print-pretty #:player #:make-player #:player-scores #:player-feats
    #:player-inventory #:player-equipment #:make-feat-expression))

(in-package :scaling-goggles)

(defstruct player
  scores
  feats
  inventory
  equipment)

(defun print-pretty ())

(defun make-feat-expression (score bonus class-skill)
  "Creates a quoted expression representing a feat for a given character"
  `(if ,class-skill
    (if (> ,score 0)
      (+ ,score ,bonus 3)
      (+ ,score ,bonus))
    (+ ,score ,bonus)))

(defvar climb (soggles:make-feat-expression 1 `(nth 0 (soggles:player-scores sam)) T))
