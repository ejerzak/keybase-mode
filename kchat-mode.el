;;;; This is a major mode for interacting with Keybase Chat. It
;;;; requires that you have the Keybase app installed, and interaction
;;;; with the filesystem enabled.


(require 'json)

(defvar keybase-program "keybase"
  "The name of the keybase executable")

(defun keybase-cmd (cmd)
  "Prepend `keybase-program' to `cmd' to generate a keybase command string"
  (format "%s %s" keybase-program cmd))

(defun kchat-list ()
  "List the active conversations"
  (async-shell-command
   (keybase-cmd
    (format "chat api -p -m '%s'"
		(json-encode
		 `(:method "list"))))
   (get-buffer-create "keybase:list")))

;; Show the conversation that self is having with user.
;; TODO: This dumps the /entire/ conversation---how can we truncate it?
(defun kchat-conversation-show (self &rest users)
  "Listen to the conversation with user(s), print the output in JSON to buffer"
  (let ((everyone (string-join (cons self users) ",")))
    (async-shell-command
     (keybase-cmd
      (format "chat api -m '%s'"
             (json-encode
              `(:method "read"
                :params
                  (:options
                    (:channel
                      (:name ,everyone)))))))
     (get-buffer-create (format "keybase:%s" everyone)))))

;; Continuously monitor conversation with user(s), print new output to buffer
;; TODO: Probably this should be in a temp buffer eventually, then added to the proper conversation one?
(defun kchat-conversation-listen (self &rest users)
  "Listen to the conversation with user(s), print the output in JSON to buffer"
  (let ((everyone (string-join (cons self users) ",")))
    (async-shell-command
     (keybase-cmd
      (format "chat api-listen --filter-channel '%s'"
	      (json-encode
	       `(:name ,everyone))))
    (get-buffer-create (format "keybase:%s" everyone)))))

;; Sends message to recipient via keybase chat send
(defun kchat-send-message (recipient message)
  "Send a message to a keybase user."
  (start-process-shell-command
   "keybase-send"
   nil
   (keybase-cmd (format "chat send %s '%s'" recipient message))))

;; An interface for sending Keybase messages from the minibuffer
(defun kchat-message-prompt ()
  "Send a message from the mini-buffer."
  (interactive)
  (setq recipient (read-from-minibuffer "Enter the username of the fool you wanna hit up: "))
  (setq message (read-from-minibuffer "What you wanna tell that fool? "))
  (kchat-send-message recipient message))


