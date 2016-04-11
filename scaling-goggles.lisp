(in-package :cl-user)

(defpackage #:scaling-goggles
  (:nicknames #:soggles)
  (:use :cl :cl-user)
  (:export #:print-pretty #:player #:make-player #:player-scores #:player-feats
    #:player-inventory #:player-equipment #:make-skill-expression
    #:make-default-player #:give-skill #:get-skill-modifier #:player-skills
    #:make-weight-expression #:give-stat #:player-stats #:make-item #:give-item
    #:make-encumberance-expression #:update-skill-modifier #:equip-item
    #:give-feat #:make-feat-expression))

(in-package :scaling-goggles)
(ql:quickload "split-sequence")
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

(defun make-encumberance-expression (player)
  "Creates an encumberance expression"
  `(labels
    ;Spooky recursive function to calculate heavy load max
    ((carry-capacity (str)
                     (if
                       (and
                        (< str 11)
                        (> str 0))
                       (* str 10)
                       (if (> str 14)
                         (* 2 (carry-capacity (- str 5)))
                         (aref
                           (make-array '(4) :initial-contents
                                       '(115, 130, 150, 175))
                           (- str 11))))))
    ;If the it's greater than 1/3, it means we are above light load
    (if
      (>
       (-
        (eval
         (cdr
          (assoc "total weight" (player-stats ,player) :test #'string=)))
        (eval
         (cdr
          (assoc "weight" (player-stats ,player) :test #'string=))))
       (float
        (*
         (carry-capacity
          (eval
           (cdr
            (assoc "strength" (player-scores ,player) :test #'string=))))
         (float 1/3))))
      ;If we are above 2/3 Heavy load, we are above medium load
      (if
        (>
         (-
          (eval
           (cdr
            (assoc "total weight" (player-stats ,player) :test #'string=)))
          (eval
           (cdr
            (assoc "weight" (player-stats ,player) :test #'string=))))
         (float
          (*
           (carry-capacity
            (eval
             (cdr
              (assoc "strength" (player-scores ,player) :test #'string=))))
           (float 2/3))))
        "Heavy"
        "Medium")
      "Light")))

(defun make-weight-expression (player)
  "Creates an expression to define a player's weight"
  `(+
    (reduce #'+
            (if (null (player-equipment ,player))
              (list)
              (map 'cons
                (lambda (x)
                        (cdr
                         (assoc "weight" (cdr x) :test #'string=)))
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

(defun make-feat-expression (name expression mod-path)
  (pairlis (list "name" "value" "modifies") (list name expression (split-sequence:split-sequence #\/ mod-path))))

(defun register-feat (curr path expr)
  "Properly register all of the feat changes"
  ;If the next part of the path is null...
  (if (null (cdr path))
    ;Set the next item down. We do this instead of cheking if path is nil to
    ;prevent trying to setf a nil function argument
    (setf
     (cdr
      (assoc
       (car path)
       curr
       :test #'string=))
     ;Append the expression
     (append
      (cdr
       (assoc
        (car path)
        curr
        :test #'string=))
      (list expr)))
    ;Otherwise, continue down recursively
    (register-feat
     (cdr
      (assoc
       (car path)
       curr
       :test #'string=))
     (cdr path)
     expr)))

(defun give-feat (feat player)
  "Places a feat within a player"
  (setf
   (player-feats player)
   (acons
    (cdr
     (assoc "name" feat :test #'string=))
    (cdr
     (assoc "value" feat :test #'string=))
    (player-feats player)))

  ;When the path given isn't null, we should register the bonuses
  (when
    (not
     (null
      (cdr
       (assoc "modifies" feat :test #'string=))))
    (register-feat
     ;Time to get just a bit hacky

     ;We need to access the first field... but it is from a struct, not an alist
     (eval
      (read-from-string
       (concatenate
        'string
        "(soggles:player-"
        (car
         (cdr
          (assoc "modifies" feat :test #'string=)))
        " "
        (symbol-name 'sam)
        ")")))
     ;Grab the rest of the path
      (cddr
       (assoc "modifies" feat :test #'string=))
     ;And the expression representing the value of the feat
     (cdr
      (assoc "value" feat :test #'string=)))))

(defun make-skill-expression (name bonus score class-skill player)
  "Creates a quoted expression representing a skill for a given player"
  (pairlis
   (list "name" "class-skill" "bonus" "score" "misc" "value")
   (list name class-skill bonus score '(+)
         ;If it's a class skill and the player has some ranks in it
         `(+
           (eval
            (cdr
             (assoc
              "misc"
              (cdr
               (assoc
                ,name
                (player-skills ,player)
                :test #'string=))
              :test #'string=)))
           (if (and
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
            ;The value is 3 + rank + modifier
            (+
             (cdr
              (assoc "score"
                     (cdr
                      (assoc ,name (player-skills ,player) :test #'string=))
                     :test #'string=))
             ;Modifier is (floor(mod - 10)/2)
             (floor
              (-
               (cdr
                (assoc ,bonus (player-scores ,player) :test #'string=))
               10)
              2)
             3)
            ;Otherwise, rank + mod
            (+
             (cdr
              (assoc "score"
                     (cdr
                      (assoc ,name (player-skills ,player) :test #'string=))
                     :test #'string=))
             ;Modifier is (floor(mod - 10)/2)
             (floor
              (-
               (cdr
                (assoc ,bonus (player-scores ,player) :test #'string=))
               10)
              2)))))))

(defun give-skill (skill player)
  "Place the skill into the player"
  (setf (player-skills player)
        (acons
         (cdr
          (assoc "name" skill :test #'string=))
         skill
         (player-skills player))))

(defun get-skill-modifier (skill modifier player)
  "Return the quoted modifier associated with a given skill"
  (cdr
   (assoc modifier
          (cdr
           (assoc skill (player-skills player) :test #'string=))
          :test #'string=)))

(defun update-skill-modifier (skill modifier new-modifier player)
  "Grabs a specific modifier for a specific skill and updates it."
  (setf
   (cdr
    (assoc modifier
           (cdr
            (assoc skill (player-skills player) :test #'string=))
           :test #'string=))
   new-modifier))

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

(defun equip-item (item player)
 "Function to move add items to the equipment."
 (let ((slot-of-item (cdr(assoc "slot" item :test #'string=))))
   ;If this slot doesn't exist yet...
   (when
     (not
      (car
       (assoc
        slot-of-item
        (player-equipment player)
        :test #'string=)))
     (setf
      (player-equipment player)
      (acons
       slot-of-item
       nil
       (player-equipment player))))
   ;If there is something in this slot in this slot...
   (when
     (cdr
      (assoc
       slot-of-item
       (player-equipment player)
       :test #'string=))
     ;Put it in the inventory
     (give-item
      (cdr
       (assoc
        slot-of-item
        (player-equipment player)
        :test #'string=))
      player))
   ;Finally, put the new item into equipment
   (setf
    (cdr
     (assoc
      slot-of-item
      (player-equipment player)
      :test #'string=))
    item)))

(defun make-default-player (hacky)
  "Creates a player with some default valuess"
 (let ((s (make-player)))
   (setf (player-scores s)
         (pairlis
          (list "strength" "dexterity" "constitution" "intelligence" "wisdom" "charisma")
          (list 10 10 10 10 10 10)))
   (setf (player-stats s)
         (pairlis (list) (list)))
   (give-stat "total weight" (make-weight-expression hacky) s)
   (give-stat "weight" 130 s)
   (give-stat "encumbrance" (make-encumberance-expression hacky) s)
   (let ((in (open "skills.txt" :if-does-not-exist nil)))
     (when in
       ;Loop over the lines and split on ':' until EOF
       (loop for line = (split-sequence:split-sequence #\: (read-line in nil))
         while (equal (null (car line)) nil) do
         ;Give the skill to the player
         (give-skill
          (make-skill-expression (nth 0 line) (nth 1 line) 0 nil hacky)
          s))
       (close in)))
   s))
