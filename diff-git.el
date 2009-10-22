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
  (autoload 'magit-run-git-with-input        "magit"))

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
    (magit-run-git-with-input tmp "apply" "--cached" "-")))

;;;###autoload
(defun diff-git-default-bindings ()
  "Add bindings to the `diff-mode' keymap."
  (define-key diff-mode-map "\C-c\C-v" 'diff-git-hunk-stage))

;;;###autoload (eval-after-load 'diff-mode '(diff-git-default-bindings))

(provide 'diff-git)

;;; diff-git.el ends here
