;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'cl)

(horizontal-scroll-bar-mode 1)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun exec-lamb (&optional command)
  (interactive)
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (s (if command
                (format "%s %s" command line)
                line)))
    (goto-char (line-end-position))
    (process-send-string "lambproc" (format "%S\n" s))))

(defun lamb-run1 ()
  (interactive)
  (exec-lamb ":run1"))
(defun lamb-run1000 ()
  (interactive)
  (exec-lamb ":run1000"))
(defun lamb-rename ()
  (interactive)
  (exec-lamb ":rename"))

(defun stop-lamb ()
  (interactive)
  (kill-process "lambproc"))

(defun start-lamb (imgdir)

  (setq reststring "")

  (start-process "lambproc" (current-buffer) "racket" "repl.rkt" imgdir)
  (set-process-filter (get-process "lambproc")
                      (lambda (p s)
                        (rpl s)))
  (global-set-key (kbd "<C-return>") 'exec-lamb)
  (global-set-key (kbd "C-e") 'lamb-run1)
  (global-set-key (kbd "C-S-e") 'lamb-run1000)
  (global-set-key (kbd "C-r") 'lamb-rename)
  (global-set-key (kbd "<M-up>")
                  (lambda ()
                    (interactive)
                    (process-send-string "lambproc" (format "%S\n" ":it"))))
  (insert "\n"))
