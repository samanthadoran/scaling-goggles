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
;I think this might work better as an a-list with up references...
;If I change it, this would work in scopes other than where the variable is
(defstruct player
  scores
  skills
  feats
  inventory
  equipment
  stats)

(defun assoc-helper (key data)
  "I wrote this way too many times in code"
  (cdr
   (assoc key data :test #'string=)))

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
                       ;if str element of [1,10]
                       (and
                        (< str 11)
                        (> str 0))
                       ;Multiply it by 10 to figure heavy load
                       (* str 10)
                       ;If str element of [15..infinity)
                       (if (> str 14)
                         ;Heavy load is two * carry-capacity with strength
                         ;reduced by 5
                         (* 2 (carry-capacity (- str 5)))
                         ;Otherwise, it is one of these four values
                         (aref
                           (make-array '(4) :initial-contents
                                       '(115, 130, 150, 175))
                           (- str 11))))))
    ;If the it's greater than 1/3, it means we are above light load
    (if
      (>
       (-
        (eval (assoc-helper "total weight" (player-stats ,player)))
        (eval (assoc-helper "weight" (player-stats ,player))))
       (float
        (*
         (carry-capacity
          (eval (assoc-helper "strength" (player-scores ,player))))
         (float 1/3))))
      ;If we are above 2/3 Heavy load, we are above medium load
      (if
        (>
         (-
          (eval (assoc-helper "total weight" (player-stats ,player)))
          (eval (assoc-helper "weight" (player-stats ,player))))
         (float
          (*
           (carry-capacity
            (eval (assoc-helper "strength" (player-scores ,player))))
           (float 2/3))))
        "Heavy"
        "Medium")
      "Light")))

(defun make-weight-expression (player)
  "Creates an expression to define a player's total weight"
  `(+
    ;Sum all items in equipment
    (reduce #'+
            ;Don't try to map on nil, it cries
            (if (null (player-equipment ,player))
              (list)
              ;Grab the weight of every piece of equipment to be summed
              (map 'cons
                   (lambda (x)
                           (assoc-helper "weight" (cdr x)))
                   (player-equipment ,player))))
    ;Sum all items in inventory
    (reduce #'+
            ;Don't try to map on nil, it cries.
            (if (null (player-inventory ,player))
              (list)
              ;Grab the weight of every item to be summed
              (map 'cons
                   (lambda (x)
                           (assoc-helper "weight" (cdr x)))
                   (player-inventory ,player))))
    ;Finally add the player's own weight to this expression
    (assoc-helper "weight" (player-stats ,player))))

(defun give-stat (stat expr player)
  "Gives a stat to a player"
  (setf (player-stats player) (acons stat expr (player-stats player))))

(defun make-feat-expression (name expression mod-path)
  (pairlis
   (list "name" "value" "modifies")
   (list name expression (split-sequence:split-sequence #\/ mod-path))))

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
      (assoc-helper (car path) curr)
      (list expr)))
    ;Otherwise, continue down recursively
    (register-feat
     (assoc-helper (car path) curr)
     (cdr path)
     expr)))


;EX: A feat that adds double the ayer's acrobatics score as a bonus
;(soggles:give-feat
; (soggles:make-feat-expression
;  "Gymnast"
;  '(*
;    2
;    (soggles:get-skill-modifier "Acrobatics" "score" sam))
;  "skills/Acrobatics/misc")
; sam)
(defun give-feat (feat player)
  "Places a feat within a player"
  (setf
   (player-feats player)
   (acons
    (assoc-helper "name" feat)
    (assoc-helper "value" feat)
    (player-feats player)))

  ;When the path given isn't null, we should register the bonuses
  (when
    (not
     (null
      (assoc-helper "modifies" feat)))
    (register-feat
     ;Time to get just a bit hacky

     ;We need to access the first field... but it is from a struct, not an alist
     (eval
      (read-from-string
       (concatenate
        'string
        "(soggles:player-"
        (car (assoc-helper "modifies" feat))
        " "
        (symbol-name 'sam)
        ")")))
     ;Grab the rest of the path
      (cddr
       (assoc "modifies" feat :test #'string=))
     ;And the expression representing the value of the feat
     (assoc-helper "value" feat))))

(defun make-skill-expression (name bonus score class-skill player)
  "Creates a quoted expression representing a skill for a given player"
  (pairlis
   (list "name" "class-skill" "bonus" "score" "misc" "value")
   (list name class-skill bonus score '(+)
         ;If it's a class skill and the player has some ranks in it
         `(+
           (eval
            (assoc-helper
             "misc"
             (assoc-helper ,name (player-skills ,player))))
           (if (and
                (assoc-helper
                 "class-skill"
                 (assoc-helper ,name (player-skills ,player)))
               (>
                (assoc-helper
                 "score"
                 (assoc-helper ,name (player-skills ,player)))
                0))
            ;The value is 3 + rank + modifier
            (+
             (assoc-helper
              "score"
              (assoc-helper ,name (player-skills ,player)))
             ;Modifier is (floor(mod - 10)/2)
             (floor
              (-
               (assoc-helper ,bonus (player-scores ,player))
               10)
              2)
             3)
            ;Otherwise, rank + mod
            (+
             (assoc-helper
              "score"
              (assoc-helper ,name (player-skills ,player)))
             ;Modifier is (floor(mod - 10)/2)
             (floor
              (-
               (assoc-helper ,bonus (player-scores ,player))
               10)
              2)))))))

(defun give-skill (skill player)
  "Place the skill into the player"
  (setf (player-skills player)
        (acons
         (assoc-helper "name" skill)
         skill
         (player-skills player))))

(defun get-skill-modifier (skill modifier player)
  "Return the quoted modifier associated with a given skill"
  (assoc-helper
   modifier
   (assoc-helper skill (player-skills player))))

(defun update-skill-modifier (skill modifier new-modifier player)
  "Grabs a specific modifier for a specific skill and updates it."
  ;TODO: Write a setf for assoc-helper
  (setf
   (cdr
    (assoc
     modifier
     (assoc-helper skill (player-skills player))
     :test #'string=))
   new-modifier))

(defun make-item (name weight)
  (pairlis (list "name" "weight") (list name weight)))

(defun give-item (item player)
  "Place the item into the player's inventory"
  (setf (player-inventory player)
        (acons
         (assoc-helper "name" item)
         item
         (player-inventory player))))

(defun equip-item (item player)
 "Function to move add items to the equipment."
 (let ((slot-of-item (assoc-helper "slot" item)))
   ;If this slot doesn't exist yet...
   (when
     (not
      (assoc-helper slot-of-item (player-equipment player)))
     (setf
      (player-equipment player)
      (acons
       slot-of-item
       nil
       (player-equipment player))))
   ;If there is something in this slot in this slot...
   (when
     (assoc-helper slot-of-item (player-equipment player))
     ;Put it in the inventory
     (give-item
      (assoc-helper slot-of-item (player-equipment player))
      player))
   ;Finally, put the new item into equipment
   ;TODO: Write a setf for assoc-helper
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
           (list "strength" "dexterity" "constitution"
                 "intelligence" "wisdom" "charisma")
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
