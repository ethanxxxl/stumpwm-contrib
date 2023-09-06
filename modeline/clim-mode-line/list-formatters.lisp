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

(define-presentation-command window-picker (win)
  (stumpwm:raise-window win))

;; spacer dummy item, no
(define-formatting-item (spacer-item (weight)))

(ql:quickload "local-time")
(define-formatting-item (date-time-item)
  "Displays the local time and date on the modeline."
  (formatting-table (pane :x-spacing 0)
    (formatting-row (pane)
      (with-default-style (pane :bg +light-blue+)
        (local-time::format-timestring
         t (local-time:now) :format '(" " (:hour 2) ":" (:min 2) " ")))
      (with-default-style (pane :bg +light-coral+)
        (local-time::format-timestring
         t (local-time:now)
         :format '(" " :day " " :short-month ", " :year " "))))))

;; FIXME Some windows don't show up until they become the active window.
(define-formatting-item (windows-item)
  "displays all windows in the current group.

the selected window has a cyan background color, while marked windows have
yellow background colors."
  (formatting-table-row (pane :x-spacing 0)
    (loop for win in (stumpwm::sort-windows (stumpwm::current-group))
          for x from 1 do
            (let* ((base-color (mapcar #'/ '(183 237 130) '(255 255 255)))
                   (num-windows (length (stumpwm::group-windows
                                         (stumpwm::current-group))))
                   (color-sweep 0.15)
                   (opacity (+ (- 1.0 color-sweep)
                               (* color-sweep (/ x num-windows))))
                   (shaded-color (apply #'make-rgb-color
                                        (mapcar #'* base-color
                                                (list opacity
                                                      opacity
                                                      opacity))))

                   ;; determine whether window is selected or or marked
                   (background-color (cond ((stumpwm::window-marked win)
                                            +yellow+)
                                           ((eq (stumpwm::current-window) win)
                                            +cyan+)
                                           (t
                                            shaded-color))))

              ;; print window name and number, associate the window object with
              ;; the text
              (with-default-style (pane :bg background-color :obj win
                                        :presentation-type 'window-picker)
                (format pane " ~D: ~A "
                        (stumpwm::window-class win)
                        (stumpwm::window-number win)))))))

(define-formatting-item (test-item (size))
  "dummy items to take up space in the modeline"
  (formatting-table (pane :x-spacing 0)
    (formatting-row (pane)
      (loop for x below (test-item-size item) do
        (with-default-style (pane)
          (format t " [TEST ~D] " x))))))

(define-formatting-item (brightness-item)
  (with-default-style (pane :bg +dodger-blue+
                            :obj "+10%"
                            :presentation-type 'change-brightness)
    (format pane " + "))
  (with-default-style (pane :bg +dark-salmon+
                            :obj "10%-"
                            :presentation-type 'change-brightness)
    (format pane " - ")))

(define-presentation-command change-brightness (shell-cmd)
  (stumpwm::run-shell-command (format nil "brightnessctl s ~A" shell-cmd)))

;; TODO create for matter functions for these items
(defstruct groups)
(defstruct media)
(defstruct wifi)
(defstruct volume)
(defstruct bluetooth)
(defstruct battery)
(defstruct weather)

(defun set-default-modeline ()
  "sets the default format of the modeline."
  (set-mode-line-format (list (make-windows-item)
                              (make-brightness-item)
                              (make-spacer-item :weight 1)
                              (make-test-item :size 4)
                              (make-spacer-item :weight 1)
                              (make-date-time-item))))
