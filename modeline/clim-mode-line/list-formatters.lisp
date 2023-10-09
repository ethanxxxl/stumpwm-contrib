;;; List formatting is a display function for the CLIM modeline. This display
;;; function loops through a regular list and puts the formatting items it sees
;;; on the modeline (there is special processing done to allow for spacers,
;;; right, and center alignment). This list formatter list is tracked by the
;;; formatters slot in the application frame.
;;;
;;; Functions and macros are defined that provide both a consistent style and
;;; convenient process for adding functionality to the modeline. Existing
;;; formatting items can be configured to suit your preferences; if no
;;; formatting item exists for your use case, adding a new one is
;;; straghtforward, only requiring 2-3 steps.
;;;
;;; 1. (define-formatting-item (name slots) body)
;;;
;;;    this macro defines a new formatting item. Internally, a structure is
;;;    defined, along with its constructure, make-/name/. Any data necessary for
;;;    configuring the item or for tracking state may be defined as zero or more
;;;    slot forms succeeding the name.
;;;
;;;    The body is the function which is called every time the modeline is
;;;    updated. (with-default-style) is another useful macro that can be used to
;;;    set the background color and interactivity of the output text/graphics.
;;;
;;; 2. (define-modeline-command ((name from-type) obj body))
;;;
;;;    This step is only necessary if the item needs to be interactive. It
;;;    defines a CLIM command and presentation translator. In the display
;;;    funciton (defined above), an object can be associated with the output
;;;    text, graphic, etc. via either the with-default-style or
;;;    with-output-as-presentation macros (former is more abstracted and domain
;;;    specific). If a modeline-command is defined with the above macro, then
;;;    body will be executed whenever a presentation with the same time as obj
;;;    is clicked on.
;;;
;;; 3. Update the formatters list
;;;
;;;    If any changes are made to the order of format items, the change needs to
;;;    be reflected in the formatters slot in the application frame. An example
;;;    of this is shown in the set-default-modeline function at the bottom of
;;;    this file. also note, that the redisp function may need to be called
;;;    after making a change.

(in-package :clim-mode-line)
(load "~/.stumpwm.d/modules/modeline/clim-mode-line/list-formatters-macros.lisp")

(defparameter *modeline-font* (make-text-style "UbuntuMono Nerd Font Propo" "Regular" 18))

(defclass formatting-item nil
  ((refresh :allocation :class :initform '(:redisplay :timout)))
  (:documentation
   "parent class for all formatting items, used in the define-formatting-item
macro. update-method determines whether an item is updated with the
update-timer, redisp, etc."))

;; spacer dummy item
;; spacer item must return something other than nil to not signal an error
(define-formatting-item (spacer-item :slots ((weight :accessor weight))) t)

(define-formatting-item (brightness-item)
  (let ((brightness (parse-integer (stumpwm::run-shell-command "brightnessctl g" t)))
        (max (parse-integer (stumpwm::run-shell-command "brightnessctl m" t))))

    (with-default-style (pane :bg +violet+)
      (format t " br"))
    (formatting-cell (pane :align-y :center :align-x :center)
      (clim-percentage-bar pane (/ brightness max) 100 30))))

(defun clim-percentage-bar (stream percent width height
                            &key (fg +gray40+) (bg +gray60+))
  (let ((x2 (* percent width)))
    (draw-rectangle* stream 0 0 width height :ink fg)
    (draw-rectangle* stream 0 0 x2 height :ink bg)))

(define-presentation-command change-brightness (shell-cmd)
  (stumpwm::run-shell-command (format nil "brightnessctl s ~A" shell-cmd)))


(defun run-program (shell-cmd &key error)
  "runs shell-cmd, returning the results as a string. The result of the program
is returned as a string, whether that be results or an error

`error-case' may be a function, a string, or nil. If it is a string, then the
string is printed in the case of an error, and the error object is not
processed. If it is a function, the function must take a single argument, which
is the error. if it is NIL, then the error is returned without processing."
  (handler-case
      (string-right-trim '(#\newline)
                         (uiop:run-program shell-cmd :output :string))
    (error (e) (cond ((typep error 'string) error)
                     ((typep error 'compiled-function)
                      (funcall error e))
                     (t e)))))

;; TODO create for matter functions for these items
;; TODO groups
;; TODO media
;; TODO volume
;; TODO battery
;; TODO weather

(defun set-default-modeline ()
  "sets the default format of the modeline."
  (set-mode-line-format (list (make-instance 'windows-item)
                              ;; (make-brightness-item)
                              (let ((s (make-instance 'spacer-item))) (setf (slot-value s 'weight) 1) s)
                              (make-instance 'media-item)
                              (make-instance 'bluetooth-item)
                              (make-instance 'network-item)
                              (make-instance 'date-time-item)
                              )))
