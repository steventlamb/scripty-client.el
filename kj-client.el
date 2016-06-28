;;; kj-client.el --- Run kj from emacs

(require 'dash)
(require 's)

;;;;;;;;;;;;;;;;;;;
;;; PUBLIC VARS ;;;
;;;;;;;;;;;;;;;;;;;

(defvar kj/executable "kj"
  "The executable used to run kj.

You can customize this var if you don't have `kj`
available on your path")

(defcustom kj/confirm-buffer-name nil
  "Prompt the user to edit buffer names when set to `t`")

;;;;;;;;;;;;;;;;;;;;
;;; INTERNAL API ;;;
;;;;;;;;;;;;;;;;;;;;

(defvar kj/last-script "" "do not hand-edit")
(put 'kj/last-script 'risky-local-variable t)

(defvar kj/last-args nil "do not hand-edit")
(put 'kj/last-args 'risky-local-variable t)

(defun kj/get-choices! ()
  (interactive)
  (let* ((list-command (concat kj/executable " -l"))
         (cmd-results (shell-command-to-string list-command))
         (lines (if (equal "" cmd-results) '() (s-split "\n" cmd-results))))
    (--filter (not (equal it "")) (-map 's-trim lines))))

(defun kj/prompt-for-command! ()
  (let ((choices (kj/get-choices!)))
    (unless (null choices)
      (if (-contains? choices kj/last-script)
          (setq choices (-uniq (cons kj/last-script choices))))
      (let ((completion
             (ido-completing-read
              (concat "command: ") choices nil t)))
        completion))))

(defun kj/get-buffer-name! (command)
  (generate-new-buffer-name (concat "*kj-" command "*") (buffer-name (current-buffer))))

(defun kj/run! (command args buffer-name)
  (async-shell-command (concat kj/executable " " command " " args) buffer-name))

;;;;;;;;;;;;;;;;;;
;;; PUBLIC API ;;;
;;;;;;;;;;;;;;;;;;

(defun kj/rerun ()
  (interactive)
  (let ((c kj/last-script))
    (kj/run! c (lax-plist-get kj/last-args c) (kj/get-buffer-name! c))))

(defun kj ()
  ;; populate command
  (interactive)

  (let ((command (kj/prompt-for-command!)))

    ;; process command, prompt for arg, execute, save last values
    (if (null command)
        (message "No kj dir found")
      (let ((args (read-string "args: " (lax-plist-get kj/last-args command)))
            (buffer-name (kj/get-buffer-name! command)))
        (setq kj/last-script command)
        (setq kj/last-args (lax-plist-put kj/last-args command args))
        (kj/run! command args buffer-name)))))

(provide 'kj-client)
