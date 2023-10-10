(in-package :clim-mode-line)

(define-formatting-item (media-item :refresh '(:timeout))
  (with-default-style (pane :bg +green3+)
    (format pane " ~A " (track-artist)))
  (with-default-style (pane :bg +green2+)
    (format pane " ~A " (track-title)))
  (with-default-style (pane :bg +green3+
                            :obj :prev
                            :presentation-type 'playerctl-track)
    (format pane " 󰒮 "))
  (with-default-style (pane :bg +green3+
                            :obj :play-pause
                            :presentation-type 'playerctl-track)
    (format pane " ~A " (play-status "" "" "")))
  (with-default-style (pane :bg +green3+
                            :obj :next
                            :presentation-type 'playerctl-track)
    (format pane " 󰒭 ")))

(define-presentation-command playerctl-track (cmd)
  (cond ((eql cmd :next)
         (stumpwm::run-shell-command "playerctl next"))
        ((eql cmd :prev)
         (stumpwm::run-shell-command "playerctl previous"))
        ((eql cmd :play-pause)
         (stumpwm::run-shell-command "playerctl play-pause"))))

(defun track-artist ()
  "return the track artist as reported by playerctl"
  (string-trim (string #\newline)
               (run-program "playerctl metadata xesam:artist")))

(defun track-title ()
  "return the track title as reported by playerctl"
  (string-trim (string #\newline)
               (run-program "playerctl metadata xesam:title")))

(defun play-status (&optional play pause stop)
  "returns the current state of the player.

If play, pause, or stop are set, then those characters will be returned intead
of the default output of playerctl"
  (let ((output (string-right-trim '(#\newline)
                                   (run-program "playerctl status"))))
    (cond ((equal output "Playing")
           (or play output))
          ((equal output "Paused")
           (or pause output))
          (t
           (or stop output)))))
