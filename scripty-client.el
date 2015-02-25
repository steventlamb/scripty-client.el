(require 'dash)
(require 's)

(defvar scripty-executable "scripty")

(defun scripty-get-choices ()
  (interactive)
  (let* ((list-command (concat scripty-executable " -l"))
         (cmd-results (shell-command-to-string list-command))
         (lines (if (equal "" cmd-results) '() (s-split "\n" cmd-results))))
    (-map 's-trim lines)))

(defun scripty (arg)
  (interactive 
   (let ((choices (scripty-get-choices)))
     (if (null choices)
         '("")
       (list (ido-completing-read "command: " (scripty-get-choices))))))
  (if (equal arg "")
      (message "No scripty dir found")
    (let ((additional-args (read-string "args: ")))
      (async-shell-command (concat scripty-executable " " arg " " additional-args)
                           (concat "*scripty-" arg "*")))))

(provide 'scripty-client)
