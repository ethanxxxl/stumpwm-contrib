(in-package :clim-mode-line)

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

(define-presentation-command window-picker (win)
  (stumpwm:focus-window win t))
