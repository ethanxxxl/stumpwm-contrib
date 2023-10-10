(in-package :clim-mode-line)

(ql:quickload "local-time")
(define-formatting-item (date-time-item :refresh '(:timeout))
  "Displays the local time and date on the modeline."
  (formatting-table (pane :x-spacing 0)
    (formatting-row (pane)
      (with-default-style (pane :bg +light-blue+)
        (local-time::format-timestring
         pane (local-time:now) :format '(" " (:hour 2) "" (:min 2) " ")))
      (with-default-style (pane :bg +light-coral+)
        (local-time::format-timestring
         pane (local-time:now)
         :format '(" " :day " " :short-month ", " :year " "))))))
