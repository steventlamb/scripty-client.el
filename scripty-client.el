;;; scripty-client.el --- Run scripty from emacs

(require 'dash)
(require 's)

(defvar scripty-executable "scripty")

(defvar scripty-last-script "" "do not hand-edit")
(put 'scripty-last-script 'risky-local-variable t)

(defvar scripty-last-args "" "do not hand-edit")
(put 'scripty-last-args 'risky-local-variable t)

(defun scripty-get-choices ()
  (interactive)
  (let* ((list-command (concat scripty-executable " -l"))
         (cmd-results (shell-command-to-string list-command))
         (lines (if (equal "" cmd-results) '() (s-split "\n" cmd-results))))
    (-map 's-trim lines)))

(defun scripty (command)
  ;; populate command
  (interactive 
   (let ((choices (scripty-get-choices)))
     (if (null choices)
         '("")
       (let ((completion
              (ido-completing-read
               (concat "command (default: \"" scripty-last-script "\"): ") choices nil t)))
         (list (if (s-blank? completion) scripty-last-script completion))))))

  ;; process command, prompt for arg, execute, save last values
  (if (equal command "")
      (message "No scripty dir found")
    (let ((args (read-string "args: " scripty-last-args)))
      (setq scripty-last-script command)
      (setq scripty-last-args args)
      (async-shell-command (concat scripty-executable " " command " " args)
                           (concat "*scripty-" command "*")))))

(provide 'scripty-client)
