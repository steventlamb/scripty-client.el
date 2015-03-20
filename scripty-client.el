;;; scripty-client.el --- Run scripty from emacs

(require 'dash)
(require 's)

(defvar scripty-executable "scripty")

(defvar scripty-last-script "" "do not hand-edit")
(put 'scripty-last-script 'risky-local-variable t)

(defvar scripty-last-args nil "do not hand-edit")
(put 'scripty-last-args 'risky-local-variable t)

(defun scripty-get-choices ()
  (interactive)
  (let* ((list-command (concat scripty-executable " -l"))
         (cmd-results (shell-command-to-string list-command))
         (lines (if (equal "" cmd-results) '() (s-split "\n" cmd-results))))
    (-map 's-trim lines)))

(defun scripty-prompt-for-command ()
  (let ((choices (scripty-get-choices)))
    (unless (null choices)
      (let ((completion
             (ido-completing-read
              (concat "command (default: \"" scripty-last-script "\"): ") choices nil t)))
        (if (s-blank? completion) scripty-last-script completion)))))

(defun scripty ()
  ;; populate command
  (interactive)

  (let ((command (scripty-prompt-for-command)))

    ;; process command, prompt for arg, execute, save last values
    (if (null command)
        (message "No scripty dir found")
      (let ((args (read-string "args: " (lax-plist-get scripty-last-args command))))
        (setq scripty-last-script command)
        (setq scripty-last-args (lax-plist-put scripty-last-args command args))
        (async-shell-command (concat scripty-executable " " command " " args)
                             (concat "*scripty-" command "*"))))))

(provide 'scripty-client)
