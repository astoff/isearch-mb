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

(put 'next-history-element 'isearch-mb-no-search t)
(put 'previous-history-element 'isearch-mb-no-search t)

(defvar isearch-mb--prompt-overlay nil
  "Overlay for minibuffer prompt updates.")

(defun isearch-mb--post-command-hook ()
  (let ((inhibit-redisplay t)
        (new-string (minibuffer-contents)))
    ;; We never update isearch-message.  If it's not empty, then
    ;; Isearch itself changed the search string, and we update it.
    (unless (string-empty-p isearch-message)
      (setq isearch-message "" new-string isearch-string)
      (delete-minibuffer-contents)
      (insert isearch-string))
    (unless (string= new-string isearch-string)
      (with-minibuffer-selected-window
        (setq isearch-string new-string)
        (isearch-update-from-string-properties new-string)
        (if (get this-command 'isearch-mb-no-search)
            (isearch-update)
          (goto-char isearch-barrier)
          (setq isearch-adjusted t isearch-yank-flag t)
        (isearch-search-and-update)))))
  (set-text-properties (minibuffer-prompt-end) (point-max) nil)
  (when-let ((fail-pos (isearch-fail-pos)))
    (add-text-properties (+ (minibuffer-prompt-end) fail-pos)
                         (point-max)
                         '(face isearch-fail)))
  (when isearch-error
    (isearch-mb--message isearch-error)))

(defun isearch-mb--minibuffer-setup ()
  (setq isearch-mb--prompt-overlay (make-overlay (point-min) (point-min)
                                                         (current-buffer) t t))
  (isearch-mb--prompt)
  (add-hook 'post-command-hook 'isearch-mb--post-command-hook nil 'local))

(defun isearch-mb--message (message &rest _)
  (message (propertize (concat " [" message "]")
                       'face 'minibuffer-prompt)))

(defun isearch-mb--prompt (&rest _)
  (when isearch-mb--prompt-overlay
    (overlay-put isearch-mb--prompt-overlay
                 'before-string
                 (concat
                  (isearch-lazy-count-format)
                  (capitalize
                   (isearch--describe-regexp-mode isearch-regexp-function))))))

(defvar isearch-mb-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-s") 'isearch-repeat-forward)
    (define-key map (kbd "C-r") 'isearch-repeat-backward)
    (define-key map (kbd "<down>") 'isearch-repeat-forward)
    (define-key map (kbd "<up>") 'isearch-repeat-backward)
    (define-key map (kbd "M-s r") 'isearch-toggle-regexp)
    (define-key map (kbd "M-s c") 'isearch-toggle-case-fold)
    (define-key map (kbd "M-s w") 'isearch-toggle-word)
    (define-key map (kbd "M-<") 'isearch-beginning-of-buffer)
    (define-key map (kbd "M->") 'isearch-end-of-buffer)
    map))

(defun isearch-mb--command-advice (fn &rest args)
  (if (eq (current-buffer) isearch--current-buffer)
      (apply fn args)
    (let ((inhibit-redisplay t))
      (with-minibuffer-selected-window
        (apply fn args)))))

(defun isearch-mb--activate (&rest _)
  "Activate `isearch-mode' read from the minibuffer."
  (when isearch-mb-mode
    (setq overriding-terminal-local-map nil)
    (advice-add 'isearch-message :override 'isearch-mb--prompt)
    (advice-add 'isearch--momentary-message :override 'isearch-mb--message)
    (let ((history-add-new-input nil)
          ;; We need to set `inhibit-redisplay' at certain points to
          ;; avoid flicker.  As a side effect, window-start/end in
          ;; `isearch-lazy-highlight-update' will have incorrect values,
          ;; so we need to lazy-highlight the whole buffer.
          (lazy-highlight-buffer (not (null isearch-lazy-highlight))))
      (unwind-protect
          (progn
            (minibuffer-with-setup-hook 'isearch-mb--minibuffer-setup
              (read-from-minibuffer
               "I-search: "
               isearch-string
               isearch-mb-minibuffer-map
               nil
               (if isearch-regexp 'regexp-search-ring 'search-ring)
               (when-let (thing (thing-at-point 'symbol))
                 (if isearch-regexp (regexp-quote thing) thing))
               t))
            (isearch-done))
        (when isearch-mode (ignore-error quit (isearch-cancel)))
        (advice-remove 'isearch-message 'isearch-mb--prompt)
        (advice-remove 'isearch--momentary-message 'isearch-mb--message)))))

(advice-add 'isearch-forward                 :after 'isearch-mb--activate)
(advice-add 'isearch-backward                :after 'isearch-mb--activate)
(advice-add 'isearch-forward-regexp          :after 'isearch-mb--activate)
(advice-add 'isearch-backward-regexp         :after 'isearch-mb--activate)
(advice-add 'isearch-forward-word            :after 'isearch-mb--activate)
;(advice-add 'isearch-forward-symbol          :after 'isearch-mb--activate)
(advice-add 'isearch-forward-symbol-at-point :after 'isearch-mb--activate)

(advice-add 'isearch-toggle-regexp       :around 'isearch-mb--command-advice)
(advice-add 'isearch-toggle-case-fold    :around 'isearch-mb--command-advice)
(advice-add 'isearch-toggle-word         :around 'isearch-mb--command-advice)
(advice-add 'isearch-repeat-forward      :around 'isearch-mb--command-advice)
(advice-add 'isearch-repeat-backward     :around 'isearch-mb--command-advice)
(advice-add 'isearch-beginning-of-buffer :around 'isearch-mb--command-advice)
(advice-add 'isearch-end-of-buffer       :around 'isearch-mb--command-advice)

;;;###autoload
(define-minor-mode isearch-mb-mode
  "Control Isearch from minibuffer.")

;;;###autoload
(define-globalized-minor-mode isearch-mb-global-mode
  isearch-mb-mode
  (lambda () (unless (minibufferp) (isearch-mb-mode))))

(provide 'isearch-mb)

;;; isearch-mb.el ends here
