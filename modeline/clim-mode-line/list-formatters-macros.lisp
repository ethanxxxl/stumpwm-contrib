;;; Macros used in the list formatter display function.
;;;
;;; these macros provide an easy way to define a formatting item along with any
;;; functionality that goes along with it. See comments at top of
;;; list-formatters.lisp for details about how to create new formatting items.

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
(defmacro with-default-style ((stream &key (bg +grey+) obj presentation-type)
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
             (with-output-as-presentation (,stream ,obj (or ,presentation-type
                                                            (type-of ,obj)))
               ,@body)
             ,@body)))))

;; FIXME if for some reason the modeline isn't initialized when this macro is
;; called, then it will not be able to find the command table "mode-line"
(defmacro define-presentation-command (presentation (obj) &body body)
  "defines a CLIM command command that will be used on the modeline.
this command will take a single argument, `obj'. Commands defined with this
macro will accible through output formatted through `with-default-style' that
have the same associated object type.

This macro defines a CLIM command and presentation-to-command translator. The
command performs an arbitrary operation on or based on the object. The
translator allows presentations of the specified data-type to be clicked on, and
their associated commands run."
  (let ((command-name (intern (format nil "~A-COMMAND" presentation)))
        (translator-name (intern (format nil "~A-TRANSLATOR" presentation))))
    `(progn
       (define-presentation-type ,presentation ())
       (define-command (,command-name :command-table mode-line)
           ((,obj))
         ,@body)
       (define-presentation-to-command-translator ,translator-name
           (,presentation ,command-name mode-line) (,obj)
         (list ,obj)))))

(defmacro define-formatting-item ((name &rest slots) &body body)
  "declares a formatting item and all of includes `frame' and `pane' bindings
to the display pane in the application frame."
  `(progn
     (defstruct ,name ,@slots)
     (defmethod format-item-display ((item ,name) frame pane)
       (formatting-table-row (pane :x-spacing 0)
         ,@body))))
