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

(defmacro singleton-table ((stream &rest cell-args) &body body)
  "creates a table composed of a single cell.
This macro can be used to set the alignment of content within another
containter. `cell-args' are passed directly to the formatting-cell."
  `(formatting-table (,stream)
     (formatting-row (,stream)
       (formatting-cell (,stream ,@cell-args)
         ,@body))))

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
(defmacro with-default-style ((stream &key (bg +grey+)) &body body)
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
         ,@body))))

(defstruct spacer
  (size 0))

(ql:quickload "local-time")
(defstruct date-time)
(defmethod format-item-display ((item date-time) frame pane)
  "Displays the local time and date on the modeline."
  (with-default-style (pane :bg +light-blue+)
    (local-time::format-timestring
     t (local-time:now) :format '(" " (:hour 2) ":" (:min 2) " ")))
  (with-default-style (pane :bg +light-coral+)
    (local-time::format-timestring
     t (local-time:now)
     :format '(" " :day " " :short-month ", " :year " "))))

;; FIXME Some windows don't show up until they become the active window.
(defstruct windows)
(defmethod format-item-display ((item windows) frame pane)
  "displays all windows in the current group.

the selected window has a cyan background color, while marked windows have
yellow background colors."
  (dolist (win (sort (stumpwm::group-windows (stumpwm::current-group))
                     (lambda (&rest wins)
                       (apply #'< (mapcar #'stumpwm::window-number wins)))))

    (with-default-style (pane :bg
                         (cond ((stumpwm::window-marked win)
                                +yellow+)
                               ((eq (stumpwm::current-window) win)
                                +cyan+)
                               (t
                                +gray80+)))
      (format pane " ~D: ~A "
              (stumpwm::window-class win)
              (stumpwm::window-number win)))))

(defstruct test-item
  (size))
(defmethod format-item-display ((item test-item) frame pane)
  "dummy items to take up space in the modeline"
  (loop for x below (test-item-size item) do
    (with-default-style (pane)
      (format t " [TEST ~D] " x))))

;; TODO create for matter functions for these items
(defstruct groups)
(defstruct media)
(defstruct wifi)
(defstruct volume)
(defstruct bluetooth)
(defstruct battery)

(defun set-default-modeline ()
  "sets the default format of the modeline."
  (set-mode-line-format (list (make-windows)
                              (make-test-item :size 4)
                              (make-date-time))))
