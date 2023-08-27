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

(in-package :clim-mode-line)

(defgeneric format-item-display (item frame pane))

(defstruct spacer
  (size 0))

(defstruct groups)

(defstruct windows)
(defmethod format-item-display ((item windows) frame pane)
  (dolist (win (stumpwm::group-windows (stumpwm::current-group)))
    (with-cell (pane) (format nil "~S" (stumpwm::window-class win)))))
