;;; -*- lexical-binding: t -*-

(defun my-read (str)
  (pcase (condition-case nil (read-from-string str) (end-of-file nil))
    ('() '())
    (`(,a . ,d) `(,a . ,(substring str d nil)))))

(setq reststring "")

(defun rpl (str)
  (setq reststring (concat reststring str))
  (let ((continue 't))
    (while continue
      (let ((res (my-read reststring)))
        (if res
            (progn
              (insert "\n")
              (pcase (car res)
                (`(img ,s)
                 (progn
                   (insert (format "(insert-pic %S)" s))
                   (insert-pic s)))
                (s
                 (progn
                   (insert s))))
              (setq reststring (cdr res)))
          (setq continue '()))))))

(defun insert-pic (filename)
  (insert "\n\n")
  (backward-char)
  (insert-image (create-image (format "%slambs/%s" lambda-homedir filename)))
  (forward-char))
