;;; isearch-mb.el --- Alternative Isearch UI based on the minibuffer -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: search
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'subr-x))

(defun isearch-mb--post-command-hook ()
  (let ((new-string (minibuffer-contents)))
    (unless (string= new-string isearch-string)
      (with-minibuffer-selected-window
        (setq isearch-string new-string)
        (setq isearch-message new-string)
        (isearch-update-from-string-properties new-string)
        (setq isearch-success t) ; Force isearch-search-and-update to try
        (if (memq this-command '(previous-history-element
                                 next-history-element))
            (isearch-update)
          (isearch-search-and-update)))))
  (set-text-properties (minibuffer-prompt-end) (point-max) nil)
  (when-let ((fail-pos (isearch-fail-pos)))
    (add-text-properties (+ (minibuffer-prompt-end) fail-pos)
                         (point-max)
                         '(face isearch-fail)))
  (cond (nil (message "> “%s”" isearch-string))
        (isearch-error
         (isearch-mb--message isearch-error))
        (isearch-lazy-count-current
         (isearch-mb--message (isearch-lazy-count-format t)))))

(defun isearch-mb--minibuffer-setup ()
  (add-hook 'post-command-hook 'isearch-mb--post-command-hook)
  (add-hook 'minibuffer-exit-hook 'isearch-mb--minibuffer-exit-hook))

(defun isearch-mb--minibuffer-exit-hook ()
  (remove-hook 'post-command-hook 'isearch-mb--post-command-hook)
  (remove-hook 'minibuffer-exit-hook 'isearch-mb--minibuffer-exit-hook))

(defun isearch-mb--message (message)
  (message (propertize (concat " [" message "]")
                       'face 'minibuffer-prompt)))

(defun isearch-mb--movement-command-advice (fn &rest args)
  (when (string-empty-p (minibuffer-contents))
    ;; TODO: Use isearch-cmds if possible
    (setq isearch-string (or (car (if isearch-regexp
                                      regexp-search-ring
                                    search-ring))
                             (user-error "No previous search string")))
    (insert isearch-string))
    (let ((inhibit-redisplay t))
      (with-minibuffer-selected-window
        (apply fn args))))

(defun isearch-mb-toggle-word ()
  (interactive)
  (with-minibuffer-selected-window
    (isearch-toggle-word)))

(defvar isearch-mb-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-s") 'isearch-repeat-forward)
    (define-key map (kbd "C-S-s") 'isearch-repeat-backward)
    (define-key map (kbd "<down>") 'isearch-repeat-forward)
    (define-key map (kbd "<up>") 'isearch-repeat-backward)
    (define-key map (kbd "C-r") 'consult-history)
    (define-key map (kbd "M-s w") 'isearch-mb-toggle-word)
    (define-key map (kbd "M-<") 'isearch-beginning-of-buffer)
    (define-key map (kbd "M->") 'isearch-end-of-buffer)
    map))

(defun isearch-mb-semiflex-regexp (string &optional lax)
  "Return a regexp which matches the words of STRING."
  (let ((words (split-string-and-unquote string)))
    (mapconcat 'regexp-quote words ".*?")))

(isearch-define-mode-toggle semiflex "f" isearch-mb-semiflex-regexp "\
Turning on word search turns off regexp mode.")

(defun isearch-mb-regexp-regexp (string &optional lax)
  "Return a regexp which matches the words of STRING."
  string)

(isearch-define-mode-toggle regexp "r" isearch-mb-regexp-regexp "\
Turning on word search turns off regexp mode.")

(defun isearch-mb-plain-regexp (string &optional lax)
  "Return a regexp which matches the words of STRING."
  (regexp-quote string))

(isearch-define-mode-toggle plain "p" isearch-mb-plain-regexp "\
Turning on word search turns off regexp mode.")

(defun isearch-mb--isearch-mode-advice (fn &rest args)
  "Advice to make `isearch-mode' read from the minibuffer."
  (interactive)
  (let ((isearch-mode-map nil)
        (enable-recursive-minibuffers nil)
        (lazy-highlight-interval nil)
        (lazy-highlight-initial-delay 0)
        (lazy-highlight-buffer-max-at-a-time nil)
        (lazy-count-suffix-format "%s of %s")
        ;; (isearch-message-function 'ignore)
        )
    (when (eq t (nth 1 args))
      (setq args `(,(car args) nil nil nil 'isearch-mb-regexp-regexp)))
    (when (eq t (nth 4 args))
      (setf (nth 4 args) 'word-search-regexp))
    (unwind-protect
        (progn
          ;(setq-local cursor-in-non-selected-windows 'always)
          (setq-local cursor-type 'hollow)
          (apply fn args)
          (minibuffer-with-setup-hook 'isearch-mb--minibuffer-setup
            (read-from-minibuffer "Search: " nil
                                  isearch-mb-minibuffer-map
                                  nil
                                  nil
                                  (thing-at-point 'symbol)))
          (isearch-exit))
      (when isearch-mode (ignore-error quit (isearch-cancel)))
      (kill-local-variable 'cursor-type))))

(defvar isearch-mb--advices
  '((isearch-mode :around isearch-mb--isearch-mode-advice)
    (isearch-message :override ignore)
    (isearch-repeat-forward :around isearch-mb--movement-command-advice)
    (isearch-repeat-backward :around isearch-mb--movement-command-advice)
    (isearch-beginning-of-buffer :around isearch-mb--movement-command-advice)
    (isearch-end-of-buffer :around isearch-mb--movement-command-advice)))

;;;###autoload
(define-minor-mode isearch-mb-mode
  "Control Isearch from minibuffer."
  :global t
  (if isearch-mb-mode
      (mapc (lambda (arg) (apply 'advice-add arg))
            isearch-mb--advices)
    (mapc (lambda (arg) (funcall 'advice-remove (car arg) (caddr arg)))
          isearch-mb--advices)))
