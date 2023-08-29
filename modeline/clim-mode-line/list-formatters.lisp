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
  (slot-value (pane-frame pane) 'clim-internals::geometry-height))

(defmacro singleton-table ((stream &rest cell-args) &body body)
  `(formatting-table (,stream)
     (formatting-row (,stream)
       (formatting-cell (,stream ,@cell-args)
         ,@body))))

;; TODO implement this macro, so that formatting items can have a default and
;; configurable style.
(defmacro with-default-style ((pane &rest args &key (bg +grey+)) &body body)
  `(let ((output-record
           (with-output-to-output-record (,pane)
             (surrounding-output-with-border (,pane)
                 (formatting-cell (,pane :align-x :left :align-y :top
                                         :min-height (cell-height ,pane))
                   (singleton-table (,pane :align-y :center)
                     ,@body)))))
         (height (cell-height ,pane)))

     ;; (stream-add-output-record ,pane output-record)
     (formatting-cell (,pane :align-x :left :align-y :top
                             :min-height height)

       (draw-rectangle* ,pane 0 0
                        (bounding-rectangle-width output-record) height :ink ,bg)
       (singleton-table (,pane :align-y :center :min-height height)
         ,@body))))

(defstruct spacer
  (size 0))

(ql:quickload "local-time")
(defstruct date-time)
(defmethod format-item-display ((item date-time) frame pane)
  (formatting-cell (pane :align-y :center)
    (local-time::format-timestring
     t (local-time:now)
     :format '((:hour 2) ":" (:min 2) " | " :day " " :short-month ", " :year))))

;; BUG Some windows don't show up until they become the active window.
(defstruct windows)
(defmethod format-item-display ((item windows) frame pane)
  (dolist (win (sort (stumpwm::group-windows (stumpwm::current-group))
                     (lambda (&rest wins)
                       (apply #'< (mapcar #'stumpwm::window-number wins)))))

    (formatting-cell (pane :align-x :center :align-y :top
                           :min-height (cell-height pane))

      (let* ((name (string-upcase (format nil " ~D: ~A "
                                           (stumpwm::window-number win)
                                           (stumpwm::window-class win))))
             (width (stream-string-width pane name))
             (height (cell-height pane)))

        (draw-rectangle* pane 0 0 width height :ink
                         (cond ((stumpwm::window-marked win)
                                +yellow+)
                               ((eq (stumpwm::current-window) win)
                                +cyan+)
                               (t
                                +gray80+)))

        (formatting-table (pane)
          (formatting-row (pane)
            (formatting-cell (pane :align-y :center :min-height (cell-height pane))
              (format pane "~A" name))))))))

(defstruct test-item
  (size))
(defmethod format-item-display ((item test-item) frame pane)
  (loop for x upto (test-item-size item) do
    (with-default-style (pane)
      (format t "[TEST ~D]" x))))

;; TODO create for matter functions for these items
(defstruct groups)
(defstruct media)
(defstruct wifi)
(defstruct volume)
(defstruct bluetooth)
(defstruct battery)

(defun set-default-modeline ()
  (set-mode-line-format (list (make-windows)
                              (make-test-item :size 4)
                              (make-date-time))))
