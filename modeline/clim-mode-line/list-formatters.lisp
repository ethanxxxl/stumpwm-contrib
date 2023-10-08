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

;; spacer dummy item
;; spacer item must return something other than nil to not signal an error
(define-formatting-item (spacer-item (weight)) t)

(ql:quickload "local-time")
(define-formatting-item (date-time-item)
  "Displays the local time and date on the modeline."
  (formatting-table (pane :x-spacing 0)
    (formatting-row (pane)
      (with-default-style (pane :bg +light-blue+)
        (local-time::format-timestring
         t (local-time:now) :format '(" " (:hour 2) "" (:min 2) " ")))
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

(define-presentation-command window-picker (win)
  (stumpwm:raise-window win))

(define-formatting-item (brightness-item)
  (let ((brightness (parse-integer (stumpwm::run-shell-command "brightnessctl g" t)))
        (max (parse-integer (stumpwm::run-shell-command "brightnessctl m" t))))

    (with-default-style (pane :bg +violet+)
      (format t " br"))
    (formatting-cell (pane :align-y :center :align-x :center)
      (clim-percentage-bar pane (/ brightness max) 100 30))))

(defun clim-percentage-bar (stream percent width height &key (fg +gray40+) (bg +gray60+))
  (let ((x2 (* percent width)))
    (draw-rectangle* stream 0 0 width height :ink fg)
    (draw-rectangle* stream 0 0 x2 height :ink bg)))

(define-presentation-command change-brightness (shell-cmd)
  (stumpwm::run-shell-command (format nil "brightnessctl s ~A" shell-cmd)))

(define-formatting-item (network-item)
  (with-default-style (pane)
    (let ((type (network-type (network-iface)))
          (strength (wifi-strength)))   ; only valid if connected to wifi
      (cond
        ((equalp type "ethernet")
         (format pane " 󰈀 ~A "
                 (network-addr (network-iface))))

        ((equalp type "wifi")
         (format pane " ~A  ~A "
                 (cond ((>= strength 80) "󰤨")
                       ((>= strength 60) "󰤥")
                       ((>= strength 40) "󰤢")
                       ((>= strength 20) "󰤟")
                       ((>= strength 00) "󰤯"))
                 (wifi-ssid)))

        (t (format pane " 󱞐 "))))))

(defun run-program (shell-cmd)
  "runs shell-cmd, returning the results as a string. The result of the program
is returned as a string, whether that be results or an error"
  (handler-case (string-right-trim '(#\newline)
                                   (uiop:run-program shell-cmd :output :string))
    (error (e) (format nil "~A" e))))

(defun network-iface (&optional (probe-addr "8.8.8.8"))
  "returns the interface that is being used to route traffic to the internet.

this works by seeing what route the kernel would choose to send a packet to
`probe-addr'. The man page for ip-route says that no packets are actually sent.
`probe-addr' can therefore be arbitrary, but google's IP was chosen for
simplicity"
  (run-program (format nil "ip route get ~A | grep -oP '(?<=dev )\\S+'"
                       probe-addr)))

(defun network-type (iface)
  "returns a symbol representing the interface type of `iface'"
  (run-program
   (format nil "nmcli -f GENERAL.TYPE -t -m tabular device show ~A" iface)))

(defun network-addr (iface)
  "returns the IPV4 address of the interface"
  (run-program
   (format nil "nmcli -f IP4.ADDRESS -t -m tabular device show ~A" iface)))

(defun wifi-ssid ()
  "returns the SSID of the current wifi interface if connected. Otherwise, a text
error is returned."
  (run-program
   "nmcli -t -f in-use,ssid device wifi list | grep -oP '(?<=\\*:).*'"))

(defun wifi-strength ()
  "returns the strength of the wifi connection, if connected. Otherwise, a text
error is returned."
  (parse-integer
   (run-program
    "nmcli -t -f in-use,signal device wifi list | grep -oP '(?<=\\*:).*'")
   :junk-allowed t))

(define-formatting-item (media-item)
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

(defun track-artist ()
  "return the track artist as reported by playerctl"
  (string-trim (string #\newline)
               (stumpwm::run-shell-command "playerctl metadata xesam:artist" t)))

(defun track-title ()
  "return the track title as reported by playerctl"
  (string-trim (string #\newline)
               (stumpwm::run-shell-command "playerctl metadata xesam:title" t)))

(defun play-status (&optional play pause stop)
  "returns the current state of the player.

If play, pause, or stop are set, then those characters will be returned intead
of the default output of playerctl"

  (let ((output (string-right-trim '(#\newline)
                                   (stumpwm::run-shell-command "playerctl status" t))))
    (cond ((equal output "Playing")
           (or play output)
           )
          ((equal output "Paused")
           (or pause output))
          (t
           (or stop output)))))

(define-presentation-command playerctl-track (cmd)
  (cond ((eql cmd :next)
         (stumpwm::run-shell-command "playerctl next"))
        ((eql cmd :prev)
         (stumpwm::run-shell-command "playerctl previous"))
        ((eql cmd :play-pause)
         (stumpwm::run-shell-command "playerctl play-pause"))))

;; TODO create for matter functions for these items
(defstruct groups)
(defstruct media)
(defstruct volume)
(defstruct bluetooth)
(defstruct battery)
(defstruct weather)

(defun set-default-modeline ()
  "sets the default format of the modeline."
  (set-mode-line-format (list (make-windows-item)
                              ;; (make-brightness-item)
                              (make-spacer-item :weight 1)
                              (make-spacer-item :weight 1)
                              (make-media-item)
                              (make-network-item)
                              (make-date-time-item)
                              )))
