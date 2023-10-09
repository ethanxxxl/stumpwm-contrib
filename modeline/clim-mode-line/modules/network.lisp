(in-package :clim-mode-line)

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

(defun network-iface (&optional (probe-addr "8.8.8.8"))
  "returns the interface that is being used to route traffic to the internet.

this works by seeing what route the kernel would choose to send a packet to
`probe-addr'. The man page for ip-route says that no packets are actually sent.
`probe-addr' can therefore be arbitrary, but google's IP was chosen for
simplicity"
  (run-program (format nil "ip route get ~A | grep -oP '(?<=dev )\\S+'"
                       probe-addr)
               :error "ERROR: couldn't find an active interface"))

(defun network-type (iface)
  "returns a symbol representing the interface type of `iface'"
  (run-program
   (format nil "nmcli -f GENERAL.TYPE -t -m tabular device show ~A" iface)
   :error "ERROR: couldn't determine interface type"))

(defun network-addr (iface)
  "returns the IPV4 address of the interface"
  (run-program
   (format nil "nmcli -f IP4.ADDRESS -t -m tabular device show ~A" iface)
   :error "ERROR: Couldn't find ipv4 address"))

(defun wifi-ssid ()
  "returns the SSID of the current wifi interface if connected. Otherwise, a text
error is returned."
  (run-program
   "nmcli -t -f in-use,ssid device wifi list | grep -oP '(?<=\\*:).*'"
   :error "ERROR: couldn't find SSID"))

(defun wifi-strength ()
  "returns the strength of the wifi connection, if connected. Otherwise, a text
error is returned."
  (parse-integer
   (run-program
    "nmcli -t -f in-use,signal device wifi list | grep -oP '(?<=\\*:).*'"
    :error "ERROR: couldn't find wifi strength")
   :junk-allowed t))
