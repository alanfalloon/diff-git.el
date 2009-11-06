;;; diff-git.el --- Stage hunks in Git from the diff

;;; Commentary:

;; This package adds commands for working with the Git index from
;; within `diff-mode' which has much better diff manipulation features
;; than most Git integrations, such as `magit-status'.

;;; History:

;;; Code:

(eval-and-compile
  (defvar diff-mode-map)
  (autoload 'diff-mode-map                   "diff-mode")
  (autoload 'diff-beginning-of-hunk          "diff-mode")
  (autoload 'diff-end-of-hunk                "diff-mode")
  (autoload 'diff-beginning-of-file-and-junk "diff-mode")
  (autoload 'diff-hunk-next                  "diff-mode")
  (autoload 'vc-deduce-fileset               "vc")
  (autoload 'vc-setup-buffer                 "vc-dispatcher")
  (autoload 'magit-run-git-with-input        "magit"))

(defvar diff-git-status-changed-hook nil
  "List of functions to be called after the git status is changed.
It is only triggered by diff-git commands that affect the status in some way.")

(defvar diff-git-update-fn nil
  "The function to call to update the current buffer.")
(make-variable-buffer-local 'diff-git-update-fn)

(defvar diff-git-update-buffers-list nil
  "Buffers that need updating when the Git repository changes.")

;;;###autoload
(defun diff-git-hunk-stage ()
  "Stage the current hunk in the index using 'git apply --cached'."
  (interactive)
  (let ((diff
         (let* ((hunk-beg (save-excursion (diff-beginning-of-hunk 'try-harder) (point)))
                (hunk-end (save-excursion (diff-end-of-hunk) (point)))
                (file-beg (save-excursion (diff-beginning-of-file-and-junk) (point)))
                (file-end (save-excursion (goto-char file-beg) (diff-hunk-next) (point))))
           (concat
            (buffer-substring-no-properties file-beg file-end)
            (buffer-substring-no-properties hunk-beg hunk-end))))
        (tmp (get-buffer-create "*magit-tmp*")))
    (with-current-buffer tmp
      (erase-buffer)
      (insert diff))
    (prog1 (magit-run-git-with-input tmp "apply" "--unidiff-zero" "--cached" "-")
      (run-hooks 'diff-git-status-changed-hook))))

;;;###autoload
(defun diff-git-diff-staged (&optional buf)
  "Show the diff of the index and HEAD.
Optional argument BUF is the buffer to store the diff contents
in, otherwise *vc-diff-staged*."
  (interactive)
  (prog1
      (diff-git-do-diff-command (or buf "*vc-diff-staged*") (not buf)
                                "--no-color" "--exit-code" "--patience" "--cached" "--")
    (setq diff-git-update-fn 'diff-git-diff-staged)))

;;;###autoload
(defun diff-git-diff-unstaged (&optional buf)
  "Show the diff of the working tree and the index.
Optional argument BUF is the buffer to store the diff contents
in, otherwise *vc-diff-unstaged*."
  (interactive)
  (prog1
      (diff-git-do-diff-command (or buf "*vc-diff-unstaged*") (not buf)
                                "--no-color" "--exit-code" "--patience" "--")
    (setq diff-git-update-fn 'diff-git-diff-unstaged)))

(defun diff-git-do-diff-command (buffer pop &rest flags)
  "Run a git diff command in a `diff-mode' buffer.
BUFFER is the buffer that will hold the diff output.
POP determines if we should pop to the beffer after the command.
Optional argument FLAGS is the options to pass to git-diff."
  (let ((files (cadr (vc-deduce-fileset))))
    (vc-setup-buffer buffer)
    (apply 'vc-git-command buffer 1 files "diff" flags)
    (set-buffer buffer)
    (if (zerop (buffer-size))
        (progn (message "nothing staged") nil)
      (diff-mode)
      (setq buffer-read-only t)
      (add-hook 'diff-git-status-changed-hook 'diff-git-update-buffers)
      (add-hook 'kill-buffer-hook 'diff-git-prune-update-buffers-list)
      (add-to-list 'diff-git-update-buffers-list (current-buffer))
      (dolist (file files)
        (let ((fbuf (get-file-buffer file)))
          (when fbuf
            (with-current-buffer fbuf
              (add-hook 'after-save-hook 'diff-git-update-buffers nil t)))))
      (when pop (pop-to-buffer (current-buffer)))
      t)))

(defun diff-git-update-buffers ()
  "Update all the buffers in `diff-git-update-buffers-list'."
  (dolist (buf diff-git-update-buffers-list)
    (with-current-buffer buf (apply diff-git-update-fn (list buf)))))

(defun diff-git-prune-update-buffers-list ()
  "Remove the current buffer from `diff-git-update-buffers-list'."
  (setq diff-git-update-buffers-list
        (delq (current-buffer) diff-git-update-buffers-list)))

;;;###autoload
(defun diff-git-default-bindings ()
  "Add bindings to the `diff-mode' keymap."
  (define-key diff-mode-map "\C-c\C-v" 'diff-git-hunk-stage))

;;;###autoload (eval-after-load 'diff-mode '(diff-git-default-bindings))

(provide 'diff-git)

;;; diff-git.el ends here
