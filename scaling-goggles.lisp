(in-package :cl-user)

(defpackage #:scaling-goggles
  (:nicknames #:soggles)
  (:use :cl :cl-user)
  (:export #:print-pretty #:player #:make-player #:player-scores #:player-feats
    #:player-inventory #:player-equipment #:make-skill-expression #:make-default-player
    #:give-skill #:get-skill-val #:player-skills #:make-weight-expression #:give-stat
    #:player-stats))

(in-package :scaling-goggles)

(defstruct player
  scores
  skills
  feats
  inventory
  equipment
  stats)

(defun print-pretty ())

(defun make-default-player ()
  (let ((s (make-player)))
    (setf (player-scores s)
          (pairlis
           (list "strength" "dexterity" "constitution" "intelligence" "wisdom" "charisma")
           (list 10 10 10 10 10 10)))
    (setf (player-stats s)
          (pairlis (list) (list)))
    s))

(defun make-weight-expression (player)
  "Creates an expression to define a player's weight"
  `(+
    (reduce #'+
            (map 'list
                 (lambda (x)
                         (cdr
                          (assoc "weight" x :test #'string=)))
                 (player-equipment ,player)))
    (reduce #'+
            (map 'list
                 (lambda (x)
                         (cdr
                          (assoc "weight" x :test #'string=)))
                 (player-inventory ,player)))
    (cdr
     (assoc "weight" (player-stats ,player) :test #'string=))))

(defun give-stat (stat expr player)
  "Gives a stat to a player"
  (setf (player-stats player) (acons stat expr (player-stats player))))

(defun make-skill-expression (name bonus score class-skill player)
  "Creates a quoted expression representing a feat for a given player"
  (pairlis
   (list "name" "class-skill" "bonus" "score" "value")
   (list name class-skill bonus score
         `(if (and
               (cdr
                 (assoc "class-skill"
                    (cdr
                     (assoc ,name (player-skills ,player) :test #'string=))
                    :test #'string=))
               (>
                (cdr
                 (assoc "score"
                        (cdr
                         (assoc ,name (player-skills ,player) :test #'string=))
                        :test #'string=))
                0))
            (+
             (cdr
              (assoc "score"
                     (cdr
                      (assoc ,name (player-skills ,player) :test #'string=))
                     :test #'string=))
             (floor
              (-
               (cdr
                (assoc ,bonus (player-scores ,player) :test #'string=))
               10)
              2)
             3)
            (+
             (cdr
              (assoc "score"
                     (cdr
                      (assoc ,name (player-skills ,player) :test #'string=))
                     :test #'string=))
             (floor
              (-
               (cdr
                (assoc ,bonus (player-scores ,player) :test #'string=))
               10)
              2))))))

(defun give-skill (skill player)
  "Place the feat into the player"
  (setf (player-skills player)
        (acons
         (cdr
          (assoc "name" skill :test #'string=))
         skill
         (player-skills player))))

(defun get-skill-val (skill player)
  "Return the quoted value associated with a given feat"
  (cdr
   (assoc "value"
          (cdr
           (assoc skill (player-skills player) :test #'string=))
          :test #'string=)))
