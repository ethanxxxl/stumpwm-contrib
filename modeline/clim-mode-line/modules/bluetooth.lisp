(in-package :clim-mode-line)

;; TODO the name of the bluetooth device to be hid when clicked on.
;; TODO make foreground color blue when bluetooth is on, grayed when it is off
;; TODO when you click the bluetooth symbol, you toggle between on off, and
;;      scanning
;; TODO when you right click on the bluetooth symbol when scanning, it should
;;      pull up a list of devices
(define-formatting-item (bluetooth-item :slots (show-name) :refresh '(:timeout))
  (with-default-style (pane :bg +white+)
    (format pane " ~A "
            (cond ((and (bluetooth-power) (bluetooth-scanning))
                   "󰂳")
                  ((and (bluetooth-power) (bluetooth-device-name))
                   "󰂱")
                  ((bluetooth-power)
                   "󰂯")
                  ((not (bluetooth-power))
                   "󰂲")))

    (when (typep (bluetooth-device-name) 'string)
      (format pane "~A " (bluetooth-device-name)))))

(defparameter *bluetooth-device-icons*
  '(("audio-card" . "󰤽")
    ("audio-headphones" . "󰋋")
    ("audio-headset" . "󰋎")
    ("camera-photo" . "")
    ("camera-video" . "")
    ("computer" . "󰇄")
    ("input-gaming" . "󰊴")
    ("input-keyboard" . "󰌌")
    ("input-mouse" . "󰍽")
    ("input-tablet" . "󰓷")
    ("modem" . "󱕙")
    ("multimedia-player" . "")
    ("network-wireless" . "󰑩")
    ("phone" . "")
    ("printer" . "󰐪")
    ("scanner" . "󰚫")
    ("unknown" . "")
    ("video-display" . "󱒃"))
  "alist of icons that correspond to potential bluetooth devices that may be
connected to the system")

(defun bluetooth-power ()
  "returns t if bluetooth is powered, nil if it isn't."
  (equalp "yes"
          (run-program "bluetoothctl show | grep -oP '(?<=Powered: )\\S+'")))

(defun bluetooth-scanning ()
  "returns t if bluetooth is in 'discovering' mode"
  (equalp "yes"
          (run-program
           "bluetoothctl show | grep -oP '(?<=Discovering: )\\S+'")))

(defun percentage-icon (val icon-string)
  "icons stored in a string (ex: \"ABCDEFG\"). The nth icon will be returned,
depending on val"
  (char icon-string (floor (* (/ val 101) (length icon-string)))))

(defun bluetooth-device-info (field)
  "returns the device value specified by field. If the field doesn't exist, an
error will be returned instead of a string."
  (run-program (format nil "bluetoothctl info | grep -oP '(?<=~A: )\\S+'"
                       field)))


(defun bluetooth-battery-icon (device level &key show-percentage)
  "returns the battery level icon of the bluetooth device if it is available"
  (format nil "~A~A"
          (cond ((equalp device "input-gaming")
                 (percentage-icon level "󰝌󰝎󰝏󰝍"))
                (t
                 (percentage-icon level "󰤾󰤿󰥀󰥁󰥂󰥃󰥄󰥅󰥆󰥈")))
          (if show-percentage
              (format nil " ~D%" level)
              "")))

(defun bluetooth-battery-level ()
  "returns the battery percentage of the connected bluetooth device as decimal
percentage. If the connected bluetooth device doesn't provide battery
information, then an error is returned."
  (let ((level (bluetooth-device-info "Battery Percentage")))
    (if (typep level 'string)
        (parse-integer level :junk-allowed t :radix 16 :start 2)
        level)))

(defun bluetooth-device-name (&optional (icon-alist *bluetooth-device-icons*))
  "tries to return the name of the bluetooth item, along with an icon that
represents that icon. If the device is battery powered, then the appropriate
battery percentage is included as well."
  (let ((alias (bluetooth-device-info "Alias"))
        (icon (bluetooth-device-info "Icon")))
    (when (typep alias 'string)
      (format nil "~A ~A"
              (format nil "~A~A"
                      (cdr (assoc icon icon-alist :test #'equalp))
                      (if (typep (bluetooth-battery-level) 'integer)
                          (bluetooth-battery-icon icon
                                                  (bluetooth-battery-level))
                          ""))

              alias))))
