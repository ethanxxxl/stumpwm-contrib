;; this addition to clim-mode-line adds a format parameter to the mode-line
;; structure. This will allow for multiple mode lines (on different screens) to
;; display different information.
;;
;; the format-list parameter is a list of format items. These items will be
;; placed on the mode line from left to right. every format item must implement
;; format-item-display.
;;
;; format-item-display is responsible for generating a CLIM table cell(s) which
;; will display any information and handle all interaction.

;; TODO during next refactor, create a more consistent naming convention for the
;; formatting items.

(in-package :clim-mode-line)

(defgeneric format-item-display (item frame pane))

(defun cell-height (pane)
  "returns the height of the application frame, ie the height of the modeline."
  (slot-value (pane-frame pane) 'clim-internals::geometry-height))

(defmacro formatting-table-row ((stream &rest table-args) &body body)
  "executes body in the context of a table with a single row."
  `(formatting-table (,stream ,@table-args)
     (formatting-row (,stream)
       ,@body)))

(defmacro singleton-table ((stream &rest cell-args) &body body)
  "creates a table composed of a single cell.
This macro can be used to set the alignment of content within another
containter. `cell-args' are passed directly to the formatting-cell."
  `(formatting-table-row (,stream)
     (formatting-cell (,stream ,@cell-args)
       ,@body)))

(defmacro with-background ((&optional (stream t) &key (bg +gray+)) &body body)
  "Draws a rectangle with color `bg' behind `body'.
`body' is drawn on top of a colored rectangle to apply a background color to it.
The rectangle will be no larger than the bounding-rectangle of `body'. The
background and `body' are written to the stream specified by `stream'"
  `(let* ((output-record (with-output-to-output-record (,stream)
                           ,@body))
          (min-point (rectangle-min-point output-record))
          (max-point (rectangle-max-point output-record)))

     (draw-rectangle ,stream min-point max-point :ink ,bg)
     (stream-add-output-record ,stream output-record)))

;; TODO add more styling options to this macro to make element shapes more
;; visually interesting.
(defmacro with-default-style ((stream &key (bg +grey+) obj)
                              &body body)
  "`body' is formatted and drawn to `stream'.
Every element of the modeline is stored in a cell in a table. This macro creates
a cell which is placed in that table. `body' is centered vertically and
horizontally in the cell. A background color specified by `bg' is applied to the
entire cell."
  `(formatting-cell (,stream :align-x :left :align-y :top)
     (with-background (,stream :bg ,bg)
       ;; NOTE rectangle is a workaround due to a bug with :min-height in mcclim
       (draw-rectangle* ,stream 0 0 0 (cell-height ,stream)
                        :ink +transparent-ink+)
       (singleton-table (,stream :align-x :center :align-y :center
                                 :min-height (cell-height ,stream))
         (if ,obj
             (with-output-as-presentation (,stream ,obj (type-of ,obj))
               ,@body)
             ,@body)))))

;; FIXME if for some reason the modeline isn't initialized when this macro is
;; called, then it will not be able to find the command table "mode-line"
(defmacro define-modeline-command ((name from-type) obj &body body)
  "defines a CLIM command command that will be used on the modeline.
this command will take a single argument, `obj'. Commands defined with this
macro will accible through output formatted through `with-default-style' that
have the same associated object type.

This macro defines a CLIM command and presentation-to-command translator. The
command performs an arbitrary operation on or based on the object. The
translator allows presentations of the specified data-type to be clicked on, and
their associated commands run."
  `(progn
     (define-command (,name :command-table mode-line) ((,obj))
       ,@body)
     (define-presentation-to-command-translator
         ,(intern (format nil "~A-TRANSLATOR" name))
         (,from-type ,name mode-line) (,obj)
       (list ,obj))))

(define-modeline-command (window-picker stumpwm::window) win
  (stumpwm:raise-window win))

(defmacro define-formatting-item ((name &rest slots) &body body)
  "declares a formatting item and all of includes `frame' and `pane' bindings
to the display pane in the application frame."
  `(progn
     (defstruct ,name ,@slots)
     (defmethod format-item-display ((item ,name) frame pane)
       (formatting-table (pane)
         (formatting-row (pane)
           ,@body)))))

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
              (with-default-style (pane :bg background-color :obj win)
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
                              (make-spacer-item :weight 1)
                              (make-test-item :size 4)
                              (make-spacer-item :weight 1)
                              (make-date-time-item))))
