;; This file contains useful things for participating to batteries
;; Right now, this consists of
;; * colorizing specially test comments (in orange)
;;
;; To use this file, simply add the following line to your .emacs:
;; (load-file "path/to/batteries/batteries_dev.el")
;;

(defface test-comment-face
  '((t :foreground "orangered3"))
  "face for test comments")

(add-hook 'tuareg-mode-hook
          '(lambda ()
             (defun tuareg-font-lock-syntactic-face-function (state)
               (if (nth 3 state) font-lock-string-face
                 (let ((start (nth 8 state)))
                   (save-excursion
                     (goto-char start)
                     (if (looking-at-p "(\\*\\$[QTRE=]")
                         'test-comment-face
                       (if (looking-at-p "(\\*\\*[^*]")
                           tuareg-doc-face
                         font-lock-comment-face))))))))
