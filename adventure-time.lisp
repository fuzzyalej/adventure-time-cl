(defvar *current-room* nil) ; the current room of the adventurer
(defvar *world* nil) ; out current world, where the adventure develops

;; helpers
(defun prompt-read (prompt)
  "Read from the console"
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun puts (msg)
  "Pretty print ala ruby, with a newline"
  (format t "~A~%" msg)
  (force-output))

;; about rooms
(defun get-room (room)
  "Return the named room"
  (assoc room *world*))

(defun id (room)
  "Return the id of the room"
  (first (get-room room)))

(defun description (room)
  "Return the description of the room"
  (second (get-room room)))

(defun exits (room)
  "Return the exits of the room"
  (third (get-room room)))

(defun peek (room)
  "Return how the room is seen from far away"
  (fourth (get-room room)))

(defun describe-exits (room)
  "Describes the exits of a room"
  (let ((exits (exits room)))
    (puts "Exits:")
    (cond ((eql exits ())(puts "This room has no exits"))
          (t (dolist (x exits) (puts (peek x)))))))

;; predicates
(defun room-exists-p (room)
  "Predicate, checks if the given room exists in the current world"
  (get-room room))

(defun room-valid-from-here-p (room)
  "Predicate, checks if the given exit room is accessible from the current room"
  (member room (exits *current-room*)))

(defun dead-end-p (room)
  "Predicate, checks if the room is a dead end (no exists)"
  (endp (exits room)))

;; about the world
(defun world-description ()
  "Describes the current view of the world"
  (puts (description *current-room*))
  (describe-exits *current-room*))

(defun ask-adventurer ()
  "Placeholder needed because we have a double recursion here and I don't know how to solve that in CL right now"
  )

(defun use-room (room)
  "Try to switch to the given room from the current one"
  (cond ((dead-end-p room)(progn (puts (description room))(puts "GAME OVER")(exit))) ; if you go to 'end, you lose automatically
        ((room-valid-from-here-p room) (progn
                                         (setf *current-room* room)
                                         (ask-adventurer)))
        (t (progn
             (puts "That room is not valid from here")
             (puts "You have been penalized for cheating, back to the start")
             (setf *current-room* 'start)
             (ask-adventurer)))))

;; prompt
(defun ask-adventurer ()
  "Prompts and asks and adventurer what he wants to do"
  (puts "")
  (world-description)
  (let* ((raw-room (prompt-read "Where to? "))
         (room (intern (string-upcase raw-room))))
    (cond ((room-exists-p room)(use-room room))
          (t (progn
               (puts "That is not a valid room!")
               (ask-adventurer))))))

;; entry point
(defun start (adventure)
  "Start an adventure!"
  (load adventure)
  (setf *current-room* 'start)
  (ask-adventurer))
