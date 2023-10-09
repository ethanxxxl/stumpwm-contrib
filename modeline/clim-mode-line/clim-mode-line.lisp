(in-package #:clim-mode-line)

;; Uncomment for debugging
(declaim (optimize (speed 0)
                   (safety 3)
                   (debug 3)))

;; load fonts, because they aren't loaded by default
(mcclim-truetype::register-all-ttf-fonts (find-port))

(defvar *align-x* :left)

(defvar *highlight-drop* nil)

(defvar *display-as-table* nil)

(defvar *display-style* :text
  "Should be one of either :table or :text.")

(defvar *text-display-formatter-intermix* " "
  "Spacing between each formatters output when in text mode. This string is 
formatted to the output stream in between each formatter when in text mode.")

(defvar *stumpwm-modeline-frame* nil
  "Hold the single mode line")

(define-application-frame mode-line ()
  ((formatters :initform (list (list 'format-groups
                                     'format-align-right
                                     'format-windows))
               :initarg :formatters
               :accessor mode-line-formatters)
   (highlight-color :initform +red+
                    :initarg :highlight-color
                    :accessor mode-line-highlight-color)
   (foreground-color :initform +black+
                     :initarg :foreground-color
                     :accessor mode-line-foreground-color)
   (mutex :initform (sb-thread:make-mutex :name "clim-modeline-mutex")
          :accessor mode-line-mutex)
   (head-width :initarg :head-width
               :accessor mode-line-head-width))
  (:panes (display :application
                   :display-function 'display-mode-line
                   :background +gray10+
                   :scroll-bars nil
                   :borders nil))
  (:layouts
   (default display)))

(defmethod (setf mode-line-formatters) (new (frame mode-line))
  (sb-thread:with-mutex ((mode-line-mutex frame))
    (setf (slot-value frame 'formatters) new)
    (redisplay-frame-panes frame :force-p t)
    (stumpwm:call-in-main-thread
     (lambda ()
       (sb-thread:with-mutex ((mode-line-mutex frame))
         (let* ((sheet (frame-top-level-sheet frame))
                (space (compose-space sheet))
                (width (space-requirement-width space))
                (height (space-requirement-height space)))
           (move-and-resize-sheet sheet 0 0 width height))
         (mapcar 'stumpwm::resize-mode-line
                 stumpwm::*mode-lines*)
         (mapcar (lambda (group)
                   (mapcar (lambda (head)
                             (stumpwm::group-sync-head
                              group head))
                           (stumpwm::group-heads group)))
                 (stumpwm::screen-groups
                  (stumpwm:current-screen))))))))

(defun mode-line-format ()
  (mode-line-formatters *stumpwm-modeline-frame*))

(defun set-mode-line-format (list)
  (setf (mode-line-formatters *stumpwm-modeline-frame*) list))

(defmethod clime:find-frame-type ((frame mode-line)) 
  "this method sets the window type via clim-clx::adopt-frame as defined below."
  :dock)

(defvar *mode-line-display-function* 'display-mode-line-as-list)

(defun display-mode-line (frame pane)
  (with-text-style (pane ;; (frame-text-style frame)
                    ;; TODO: We may need to 
                    ;; (make-text-style "DejaVu Sans Mono" "Book" 14)
                    (make-text-style nil nil nil) ; use default text style
                    )
    (funcall *mode-line-display-function* frame pane)))

;; TODO this display function should only update certain items under certain
;; conditions. ie, don't update the network/time module when switching windows.
;; there is a framework to track this state. each format-item is now a subclass
;; of formatting-item, and has a field called `refresh' that will contain a list
;; of the conditions it should be refreshed under.
(defun display-mode-line-as-list (frame pane)
  "display function that treats`mode-line-formatters' as a list.
The list is comprised of structures that define a method for the
`format-item-display' generic. These methods contain draw instructions which are
displayed on the modeline. Flex-box style spacing is also supported through
spacer items.

Spacers take up all available space. The width of an individual spacer is the
ratio between the weight of that spacer and the total weight of all spacers."
  (let ((space-used
          (reduce #'+ (mode-line-format)
                  :key (lambda (item)
                         (if (find-method #'format-item-display '()
                                          (list (type-of item) t t) nil)
                             (rectangle-width
                              (with-output-to-output-record (pane)
                                (format-item-display item frame pane)))
                             0))))
        (spacers-weight
          (reduce #'+ (remove-if-not (lambda (item) (typep item 'spacer-item))
                                     (mode-line-format))
                  :key #'weight))
        ;; FIXME multiple heads could cause problems
        (head-width (stumpwm::head-width (stumpwm::current-head))))

    (loop for item in (mode-line-format)
          with start = 0 do
            (cond ((typep item 'spacer-item)
                   (incf start (* (- head-width space-used)
                                  (/ (weight item)
                                     spacers-weight)))
                   (incf start))        ; HACK for some reason this is off by
                                        ; a single pixel.
                  ((find-method #'format-item-display '()
                                (list (type-of item) t t) nil)
                   (let ((record (with-output-to-output-record (pane)
                                   (format-item-display item frame pane))))
                     (setf (output-record-position record) (values start 0))
                     (stream-add-output-record pane record)
                     (incf start (rectangle-width record))))))))


(defun display-mode-line-as-table (frame pane)
  (with-table (pane)
    (dolist (line (mode-line-formatters frame))
      (with-table-row ()
        (funcall (car line) frame pane (cdr line))))))

(defun display-mode-line-as-text (frame pane)
  (dolist (line (mode-line-formatters frame))
    (funcall (car line) frame pane (cdr line))))

;; Glue between this and StumpWM

(defun update-mode-line ()
  (when *stumpwm-modeline-frame*
    (execute-frame-command *stumpwm-modeline-frame* '(com-refresh))))

(defun update-mode-line-hanger (&rest ignore)
  (declare (ignore ignore))
  (update-mode-line))

(defun app-main (&optional (head (stumpwm:current-head)))
  (let* ((total-width (stumpwm::head-width head))
         (frame (make-application-frame 'mode-line
                                        :left 0
                                        :top 0
                                        :height 20
                                        :width total-width
                                        :head-width total-width)))
    (setf *stumpwm-modeline-frame* frame)
    (stumpwm:add-hook stumpwm:*post-command-hook* 'update-mode-line-hanger)

    ;; hold the modeline mutex while clim initializes
    (sb-thread:with-mutex ((mode-line-mutex frame))
      (sb-thread:make-thread
       (lambda ()
         (run-frame-top-level frame))
       :name "CLIM-MODE-LINE")

      ;; don't release the mutex until clim is done initializing
      (loop until (frame-top-level-sheet frame)))

    ;; periodically updates the modeline to sure things like the time are
    ;; updated.
    (sb-thread:make-thread
     (lambda ()
       (loop
         (sb-thread:with-mutex ((mode-line-mutex frame))
           (update-mode-line))
         (sleep 5)))
     :name "CLIM-MODE-LINE")))          ; keep names the same for easy clean up.

(defun kill-rogue-threads ()
  "sometimes restarting doesn't kill its thread, this makes sure it does."
  (mapc #'sb-thread:terminate-thread
        (remove-if-not
         (lambda (thr)
           (equal (slot-value thr 'sb-thread::name) "CLIM-MODE-LINE"))
         (sb-thread:list-all-threads)))

  (loop while (find "CLIM-MODE-LINE" (sb-thread:list-all-threads)
                    :key (lambda (thread) (slot-value thread 'sb-thread::name)))))


(defun debug-kill-modeline ()
  (when *stumpwm-modeline-frame*
    (execute-frame-command *stumpwm-modeline-frame* '(com-quit)))
  (setf *stumpwm-modeline-frame* nil)
  (setf stumpwm::*mode-lines* nil)
  (kill-rogue-threads))

(defun debug-kill-restart-hard ()
  (when *stumpwm-modeline-frame*
    (execute-frame-command *stumpwm-modeline-frame* '(com-quit)))
  (setf *stumpwm-modeline-frame* nil)
  (setf stumpwm::*mode-lines* nil)
  (kill-rogue-threads)
  (stumpwm:run-commands "restart-hard"))

(defun redisp (&optional (frame *stumpwm-modeline-frame*))
  (sb-thread:with-mutex ((mode-line-mutex frame))
    (redisplay-frame-panes frame :force-p t)
    (stumpwm:call-in-main-thread
     (lambda ()
       (sb-thread:with-mutex ((mode-line-mutex frame))
         (let* ((sheet (frame-top-level-sheet frame))
                (space (compose-space sheet))
                (width (slot-value frame 'head-width))

                ;; NOTE the height should be whatever the user wants.
                ;; `space-requirement' seems to default to 100 no matter what
                ;; the frame height is set to.
                (height (slot-value frame 'clim-internals::geometry-height)))
           (move-and-resize-sheet sheet 0 0 width height))
         (mapcar 'stumpwm::resize-mode-line
                 stumpwm::*mode-lines*)
         (mapcar (lambda (group)
                   (mapcar (lambda (head)
                             (stumpwm::group-sync-head
                              group head))
                           (stumpwm::group-heads group)))
                 (stumpwm::screen-groups
                  (stumpwm:current-screen)))

         ;; redisplay by sending it an update once finished.
         (update-mode-line))))))

(defun modeline-width (&optional (frame *stumpwm-modeline-frame*))
  (sb-thread:with-mutex ((mode-line-mutex frame))
    (let* ((sheet (frame-top-level-sheet frame))
           (space (compose-space sheet))
           (width (space-requirement-width space)))
      (format t "width: ~S" width))))

;; BUG there seems to be either a race conditoin or some other odd behavior in
;; (redisp). The function usually needs to be called twice, but can't be called
;; "too quickly" after itself.
(defun reset-modeline ()
  "convenience function to delete the modeline and start it again."
  (debug-kill-modeline)
  (app-main)
  (set-default-modeline)
  (redisp))
