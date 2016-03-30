(in-package :cl-user)

(defpackage #:scaling-goggles
  (:nicknames #:soggles)
  (:use :cl :cl-user)
  (:export #:print-pretty #:player #:make-player #:player-scores #:player-feats
    #:player-inventory #:player-equipment #:make-skill-expression #:make-default-player
    #:give-skill #:get-skill-val #:player-skills #:make-weight-expression #:give-stat
    #:player-stats #:make-item #:give-item))

(in-package :scaling-goggles)

(defstruct player
  scores
  skills
  feats
  inventory
  equipment
  stats)

(defun print-section (name section is-cons)
  "Helper function to print items from a section of player"
  (print name)
  (print "***********")
  (loop for item in section
    do
    (progn
     (print (eval (car item)))
     (princ ": ")
     ;If it is of type cons, don't try to eval that, it doesn't make much sense.
     (if is-cons
       ;Instead, loop over the associations and print them tabbed over
       (loop for assoc in (cdr item)
         do
         (progn
          (princ #\linefeed)
          (princ #\tab)
          (princ (eval (car assoc)))
          (princ ": ")
          (princ (eval (cdr assoc)))))
       (princ (eval (cdr item))))))
  (princ #\linefeed))

(defun print-pretty (player)
  "Print a human readable version of player"
  (print-section "Stats" (player-stats player) nil)
  (print-section "Skills" (player-skills player) T)
  (print-section "Scores" (player-scores player) nil)
  (print-section "Feats" (player-feats player) nil)
  (print-section "Inventory" (player-inventory player) T)
  (print-section "Equipment" (player-equipment player) T))

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
            (if (null (player-inventory ,player))
              (list)
              (map 'cons
                (lambda (x)
                        (cdr
                         (assoc "weight" x :test #'string=)))
                         (player-equipment ,player))))
    (reduce #'+
            (if (null (player-inventory ,player))
              (list)
              (map 'cons
                   (lambda (x)
                           (cdr
                            (assoc "weight" (cdr x) :test #'string=)))
                   (player-inventory ,player))))
    (cdr
     (assoc "weight" (player-stats ,player) :test #'string=))))

(defun give-stat (stat expr player)
  "Gives a stat to a player"
  (setf (player-stats player) (acons stat expr (player-stats player))))

(defun make-skill-expression (name bonus score class-skill player)
  "Creates a quoted expression representing a skill for a given player"
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
  "Place the skill into the player"
  (setf (player-skills player)
        (acons
         (cdr
          (assoc "name" skill :test #'string=))
         skill
         (player-skills player))))

(defun get-skill-val (skill player)
  "Return the quoted value associated with a given skill"
  (cdr
   (assoc "value"
          (cdr
           (assoc skill (player-skills player) :test #'string=))
          :test #'string=)))

(defun make-item (name weight)
  (pairlis (list "name" "weight") (list name weight)))

(defun give-item (item player)
  "Place the item into the player's inventory"
  (setf (player-inventory player)
        (acons
         (cdr
          (assoc "name" item :test #'string=))
         item
         (player-inventory player))))
