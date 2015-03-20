;;; scripty-client.el --- Run scripty from emacs

(require 'dash)
(require 's)

(defvar scripty-executable "scripty")

(defvar scripty-last-script "" "do not hand-edit")
(put 'scripty-last-script 'risky-local-variable t)

(defvar scripty-last-args nil "do not hand-edit")
(put 'scripty-last-args 'risky-local-variable t)

(defcustom scripty-confirm-buffer-name nil
  "Prompt the user to edit buffer names when set to `t`")

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

(defun scripty-get-buffer-name (command)
  "given the name of a command, build a suggested buffer name.

The default syntax is '*scripty-<command>*', but we must first look
for existing buffers with that name and add an increasing unique id,
for example, '*scripty-<command>*<1>'

Finally, prompt to confirm the choice of name if
`scripty-confirm-buffer-name` is `t`.
"
  (let* ((base-name (concat "*scripty-" command "*"))
         (suffix "")
         (id 1)
         (name base-name))
    (while (get-buffer name)
      (setq id (+ 1 id))
      (setq suffix (concat "<" (number-to-string id) ">"))
      (setq name (concat base-name suffix)))
    (if scripty-confirm-buffer-name
        (setq name (read-string "buffer name: " name)))
    name))

(defun scripty ()
  ;; populate command
  (interactive)

  (let ((command (scripty-prompt-for-command)))

    ;; process command, prompt for arg, execute, save last values
    (if (null command)
        (message "No scripty dir found")
      (let ((args (read-string "args: " (lax-plist-get scripty-last-args command)))
            (buffer-name (scripty-get-buffer-name command)))
        (setq scripty-last-script command)
        (setq scripty-last-args (lax-plist-put scripty-last-args command args))
        (async-shell-command (concat scripty-executable " " command " " args) buffer-name)))))

(provide 'scripty-client)
