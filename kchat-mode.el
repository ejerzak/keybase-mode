;;;; This is a major mode for interacting with Keybase Chat. It
;;;; requires that you have the Keybase app installed, and interaction
;;;; with the filesystem enabled.


(require 'json)

;; Tells Emacs what keybase command to use
(setq keybase-program "keybase")

(defun kchat-list ()
  "List the active conversations"
  (async-shell-command "keybase chat api -m '{\"method\": \"list\"}' -p" (get-buffer-create "keybase:list")))

;; Show the conversation that self is having with user.
;; TODO: Allow for group chats with multiple users
;; TODO: This dumps the /entire/ conversation--maybe it needs to be truncated?
(defun kchat-conversation-show (self user options)
  "Listen to the conversation with user, print the output in JSON to buffer"
  (async-shell-command (concat "keybase chat api -m '{\"method\": \"read\", \"params\": {\"options\": {\"channel\": {\"name\": \""
			       self
			       ","
			       user
			       "\"}}}}'")
		       (get-buffer-create (concat "\"keybase:\"" user "\""))))

;; Continuously monitor conversation with user, print new output to buffer
;; TODO: Probably this should be in a temp buffer eventually, then added to the proper conversation one?
(defun kchat-conversation-listen (user)
  "Listen to the conversation with user, print the output in JSON to buffer"
  (async-shell-command (concat "keybase chat api-listen --filter-channel '{\"name\":\""
			       user
			       "\"}'")
		       (get-buffer-create (concat "\"keybase:" user "\""))))


;; Sends message to recipient via keybase chat send
(defun kchat-send-message (recipient message)
  "Send a message to a keybase user."
  (start-process-shell-command "keybase-send" nil (concat "keybase chat send " recipient " \"" message "\"")))

;; An interface for sending Keybase messages from the minibuffer
(defun kchat-message-prompt ()
  "Send a message from the mini-buffer."
  (interactive)
  (setq recipient (read-from-minibuffer "Enter the username of the fool you wanna hit up: "))
  (setq message (read-from-minibuffer "What you wanna tell that fool? "))
  (kchat-send-message recipient message))


