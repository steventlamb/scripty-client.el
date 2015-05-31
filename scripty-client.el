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
  "given the name of a command, build a suggested buffer name.

The default syntax is '*scripty-<command>*', but we must first look
for existing buffers with that name and add an increasing unique id,
for example, '*scripty-<command>*<1>'

If the current buffer is a scripty buffer, try hard to reuse that one.

Finally, prompt to confirm the choice of name if
`scripty/confirm-buffer-name` is `t`.
"
  (let*
      ((base-buffer-name (concat "*scripty-" command "*"))
       (candidate (if (equal (buffer-name (current-buffer)) base-buffer-name)
                      base-buffer-name
                    (let ((suffix "")
                          (id 1)
                          (name base-buffer-name))
                      (while (get-buffer name)
                        (setq id (+ 1 id))
                        (setq suffix (concat "<" (number-to-string id) ">"))
                        (setq name (concat base-buffer-name suffix)))
                      name))))
    ;; always prompt if the var is set, even if we're sure we want to reuse
    (if scripty/confirm-buffer-name (read-string "buffer name: " candidate) candidate)))

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
