;;; scripty-client.el --- Run scripty from emacs

(require 'dash)
(require 's)

;;;;;;;;;;;;;;;;;;;
;;; PUBLIC VARS ;;;
;;;;;;;;;;;;;;;;;;;

(defvar scripty/executable "scripty"
  "The executable used to run scripty.

You can customize this var if you don't have`scripty`
available on your path")

(defcustom scripty/confirm-buffer-name nil
  "Prompt the user to edit buffer names when set to `t`")

;;;;;;;;;;;;;;;;;;;;
;;; INTERNAL API ;;;
;;;;;;;;;;;;;;;;;;;;

(defvar scripty/last-script "" "do not hand-edit")
(put 'scripty/last-script 'risky-local-variable t)

(defvar scripty/last-args nil "do not hand-edit")
(put 'scripty/last-args 'risky-local-variable t)

(defun scripty/get-choices! ()
  (interactive)
  (let* ((list-command (concat scripty/executable " -l"))
         (cmd-results (shell-command-to-string list-command))
         (lines (if (equal "" cmd-results) '() (s-split "\n" cmd-results))))
    (-map 's-trim lines)))

(defun scripty/prompt-for-command! ()
  (let ((choices (scripty/get-choices!)))
    (unless (null choices)
      (if (-contains? choices scripty/last-script)
          (setq choices (-uniq (cons scripty/last-script choices))))
      (let ((completion
             (ido-completing-read
              (concat "command: ") choices nil t)))
        completion))))

(defun scripty/get-buffer-name! (command)
  (generate-new-buffer-name (concat "*scripty-" command "*") (buffer-name (current-buffer))))

(defun scripty/run! (command args buffer-name)
  (async-shell-command (concat scripty/executable " " command " " args) buffer-name))

;;;;;;;;;;;;;;;;;;
;;; PUBLIC API ;;;
;;;;;;;;;;;;;;;;;;

(defun scripty/rerun ()
  (interactive)
  (let ((c scripty/last-script))
    (scripty/run! c (lax-plist-get scripty/last-args c) (scripty/get-buffer-name! c))))

(defun scripty ()
  ;; populate command
  (interactive)

  (let ((command (scripty/prompt-for-command!)))

    ;; process command, prompt for arg, execute, save last values
    (if (null command)
        (message "No scripty dir found")
      (let ((args (read-string "args: " (lax-plist-get scripty/last-args command)))
            (buffer-name (scripty/get-buffer-name! command)))
        (setq scripty/last-script command)
        (setq scripty/last-args (lax-plist-put scripty/last-args command args))
        (scripty/run! command args buffer-name)))))

(provide 'scripty-client)
