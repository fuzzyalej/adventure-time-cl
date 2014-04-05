(defvar *current-stage* nil)
(defvar *world* nil)

;; helpers
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun puts (msg)
  (format t "~A~%" msg)
  (force-output))

(defun get-stage (stage)
  (assoc stage *world*))

;; accessors for a stage
(defun id (stage)
  (first (get-stage stage)))

(defun description (stage)
  (second (get-stage stage)))

(defun exits (stage)
  (third (get-stage stage)))

(defun peek (stage)
  (fourth (get-stage stage)))

(defun describe-exits (stage)
  (let ((exits (exits stage)))
    (puts "Exits:")
    (cond ((eql exits ())(puts "This room has no exits"))
          (t (dolist (x exits) (puts (peek x)))))))

(defun stage-exists-p (stage)
  (get-stage stage))

(defun stage-valid-from-here-p (stage)
  (member stage (exits *current-stage*)))

(defun world-description ()
  (puts (description *current-stage*))
  (describe-exits *current-stage*))

(defun ask-adventurer ()
  (world-description)
  (let* ((raw-stage (prompt-read "Where to? "))
         (stage (intern (string-upcase raw-stage))))
    (cond ((and (stage-exists-p stage)(stage-valid-from-here-p stage))
           (progn
             (setf *current-stage* stage)
             (ask-adventurer)))
          (t (progn
               (puts "That is not a valud stage!")
               (ask-adventurer))))))

(defun start (adventure)
  "Start an adventure!"
  (load adventure)
  (setf *current-stage* 'start)
  (ask-adventurer))
