(in-package :cl-user)

(defpackage #:scaling-goggles
  (:nicknames #:soggles)
  (:use :cl :cl-user)
  (:export #:print-pretty #:give-entry
    #:make-skill-expression
    #:make-default-player #:get-skill-modifier
    #:make-weight-expression #:make-item #:give-item
    #:make-encumberance-expression #:update-skill-modifier #:equip-item
    #:give-feat #:make-feat-expression))

(in-package :scaling-goggles)
(ql:quickload "split-sequence")

(defun assoc-helper (key data)
  "I wrote this way too many times in code"
  (cdr
   (assoc key data :test #'string=)))

(defun print-section (name section is-cons)
  "Helper function to print items from a section of player"
  (print name)
  (print "***********")
  (loop for key being the hash-keys of section
        using (hash-value value)
    do
    (progn
     (print key)
     (princ ": ")
     ;If it is of type cons, don't try to eval that, it doesn't make much sense.
     (if is-cons
       ;Instead, loop over the associations and print them tabbed over
       (loop for assoc in value
         do
         (progn
          (princ #\linefeed)
          (princ #\tab)
          (princ (eval (car assoc)))
          (princ ": ")
          (princ (eval (cdr assoc)))))
       (princ (eval value)))
    (princ #\linefeed))))

(defun print-pretty (player)
  "Print a human readable version of player"
  (print-section "Stats" (gethash "stats" player) nil)
  (print-section "Skills" (gethash "skills" player) T)
  (print-section "Scores" (gethash "scores" player) nil)
  (print-section "Feats" (gethash "feats" player) T)
  (print-section "Inventory" (gethash "inventory" player) T)
  (print-section "Equipment" (gethash "equipment" player) T))

(defun give-entry (section name entry player)
  (setf
   (gethash
    name
    (gethash section player))
   entry))

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
        (eval (gethash "total weight" (gethash "stats" ,player)))
        (eval (gethash "weight" (gethash "stats" ,player))))
       (float
        (*
         (carry-capacity
          (eval (gethash "strength" (gethash "scores" ,player))))
         (float 1/3))))
      ;If we are above 2/3 Heavy load, we are above medium load
      (if
        (>
         (-
          (eval (gethash "total weight" (gethash "stats" ,player)))
          (eval (gethash "weight" (gethash "stats" ,player))))
         (float
          (*
           (carry-capacity
            (eval (gethash "strength" (gethash "scores" ,player))))
           (float 2/3))))
        "Heavy"
        "Medium")
      "Light")))

(defun make-weight-expression (player)
  "Creates an expression to define a player's total weight"
  `(+
    ;Sum all items in inventory
    (reduce #'+
            ;Why doesn't maphash return a list???
            (let ((a (list)))
              (maphash
               #'(lambda(key value)
                        (declare (ignore key))
                        (setf
                         a
                         (cons (assoc-helper "weight" value) a)))
               (gethash "inventory" ,player))
              a))
    ;Sum all items in equipment
    (reduce #'+
            ;Why doesn't maphash return a list???
            (let ((a (list)))
              (maphash
               #'(lambda(key value)
                        (declare (ignore key))
                        (setf
                         a
                         (cons (assoc-helper "weight" value) a)))
               (gethash "equipment" ,player))
              a))
    (gethash "weight" (gethash "stats" ,player))))

(defun make-feat-expression (name expression mod-path)
  (pairlis
   (list "name" "value" "modifies")
   (list name expression mod-path)))

(defun register-feat (curr path expr)
  "Properly register all of the feat changes"
  ;If the next part of the path is null...
  (if (null (cdr path))
    ;Set the next item down. We do this instead of cheking if path is nil to
    ;prevent trying to setf a nil function argument
    (if (listp curr)
      (setf
       (cdr
        (assoc (car path) curr :test #'string=))
       (append
        (assoc-helper (car path) curr)
        (list expr)))
      (setf
       (gethash (car path) curr)
       (append
        (gethash (car path) curr)
        (list expr))))
    ;Otherwise, continue down recursively
    (register-feat
     (if (listp curr)
       (assoc-helper (car path) curr)
       (gethash (car path) curr))
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
  (give-entry "feats" (assoc-helper "name" feat) feat player)

  ;When the path given isn't null, we should register the bonuses
  (when
    (not
     (null
      (assoc-helper "modifies" feat)))
    (register-feat
     player
     ;Get the path, make it into a list
     (split-sequence:split-sequence #\/ (assoc-helper "modifies" feat))
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
             (gethash ,name (gethash "skills" ,player))))
           (if (and
                (assoc-helper
                 "class-skill"
                 (gethash ,name (gethash "skills" ,player)))
               (>
                (assoc-helper
                 "score"
                 (gethash ,name (gethash "skills" ,player)))
                0))
            ;The value is 3 + rank + modifier
            (+
             (assoc-helper
              "score"
              (gethash ,name (gethash "skills" ,player)))
             ;Modifier is (floor(mod - 10)/2)
             (floor
              (-
               (gethash ,bonus (gethash "scores" ,player))
               10)
              2)
             3)
            ;Otherwise, rank + mod
            (+
             (assoc-helper
              "score"
              (gethash ,name (gethash "skills" ,player)))
             ;Modifier is (floor(mod - 10)/2)
             (floor
              (-
               (gethash ,bonus (gethash "scores" ,player))
               10)
              2)))))))

(defun get-skill-modifier (skill modifier player)
  "Return the quoted modifier associated with a given skill"
  (assoc-helper
   modifier
   (gethash skill (gethash "skills" player))))

(defun update-skill-modifier (skill modifier new-modifier player)
  "Grabs a specific modifier for a specific skill and updates it."
  ;TODO: Write a setf for gethash
  (setf
   (cdr
    (assoc
     modifier
     (gethash skill (gethash "skills" player))
     :test #'string=))
   new-modifier))

(defun make-item (name weight)
  (pairlis (list "name" "weight") (list name weight)))

(defun give-item (item player)
  "Place the item into the player's inventory"
  (give-entry
   "inventory"
   (assoc-helper "name" item)
   item
   player))

(defun equip-item (item player)
 "Function to move add items to the equipment."
  (let ((slot-of-item (assoc-helper "slot" item)))
    ;When there is an item in the slot...
    (when
      (gethash slot-of-item (gethash "equipment" player))
      ;Put the item in the inventory
      (give-item
       (gethash slot-of-item (gethash "equipment" player))
       player))
    ;Place the new item in the equipment
    (setf
     (gethash
      slot-of-item
      (gethash "equipment" player))
     item)))

(defun make-default-player ()
  "Creates a player with some default valuess"
  (let ((s (make-hash-table :test 'equal)))
    (setf (gethash "scores" s) (make-hash-table :test 'equal))
    (loop for name in (list "strength" "dexterity" "constitution"
                            "intelligence" "wisdom" "charisma")
      for value in (list 10 10 10 10 10 10)
      do
      (setf (gethash name (gethash "scores" s)) value))
    (setf
     (gethash "stats" s)
     (make-hash-table :test 'equal))
    (setf
     (gethash "skills" s)
     (make-hash-table :test 'equal))
    (setf
     (gethash "feats" s)
     (make-hash-table :test 'equal))
    (setf
     (gethash "inventory" s)
     (make-hash-table :test 'equal))
    (setf
     (gethash "equipment" s)
     (make-hash-table :test 'equal))

    (give-entry "stats" "weight" 130 s)
    (give-entry "stats" "total weight" (make-weight-expression s) s)
    (give-entry "stats" "encumbrance" (make-encumberance-expression s) s)
    (let ((in (open "skills.txt" :if-does-not-exist nil)))
      (when in
        ;Loop over the lines and split on ':' until EOF
        (loop for line = (split-sequence:split-sequence #\: (read-line in nil))
          while (equal (null (car line)) nil) do
          ;Give the skill to the player
          (give-entry
           "skills"
           (nth 0 line)
           (make-skill-expression (nth 0 line) (nth 1 line) 0 nil s)
           s))
        (close in)))
    s))
