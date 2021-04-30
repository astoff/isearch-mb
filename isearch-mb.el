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

(defvar isearch-mb--prompt-overlay nil
  "Overlay for minibuffer prompt updates.")

(defvar isearch-mb-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-j" 'newline)
    (define-key map "\C-s" 'isearch-repeat-forward)
    (define-key map [down] 'isearch-repeat-forward)
    (define-key map "\C-r" 'isearch-repeat-backward)
    (define-key map [up] 'isearch-repeat-backward)
    (define-key map "\M-%" 'isearch-query-replace)
    (define-key map [?\C-\M-%] 'isearch-query-replace-regexp)
    (define-key map "\M-<" 'isearch-beginning-of-buffer)
    (define-key map "\M->" 'isearch-end-of-buffer)
    (define-key map "\M-s'" 'isearch-toggle-char-fold)
    (define-key map "\M-s " 'isearch-toggle-lax-whitespace)
    (define-key map "\M-s_" 'isearch-toggle-symbol)
    (define-key map "\M-sc" 'isearch-toggle-case-fold)
    (define-key map "\M-shr" 'isearch-highlight-regexp)
    (define-key map "\M-shl" 'isearch-highlight-lines-matching-regexp)
    (define-key map "\M-si" 'isearch-toggle-invisible)
    (define-key map "\M-so" 'isearch-occur)
    (define-key map "\M-sr" 'isearch-toggle-regexp)
    (define-key map "\M-sw" 'isearch-toggle-word)
    map)
  "Minibuffer keymap used by Isearch-Mb.")

(defun isearch-mb--post-command-hook ()
  "Hook to run from the minibuffer to update the Isearch state."
  (let ((text (minibuffer-contents)))
    ;; We never update isearch-message.  If it's not empty, then
    ;; Isearch itself changed the search string, and we update it.
    (unless (string-empty-p isearch-message)
      (setq isearch-message ""
            text isearch-string)
      (delete-minibuffer-contents)
      (insert isearch-string))
    ;; Update search buffer
    (unless (string-equal text isearch-string)
      (let ((inhibit-redisplay t))
        (with-minibuffer-selected-window
          (setq isearch-string (substring-no-properties text))
          (isearch-update-from-string-properties text)
          ;; Backtrack to barrier and search, unless the `this-command'
          ;; is special or the search regexp is invalid.
          (if (or (get this-command 'isearch-mb--no-search)
                  (and isearch-regexp
                       (condition-case err
                           (prog1 nil (string-match-p isearch-string ""))
                         (invalid-regexp
                          (prog1 t (isearch-mb--message (cadr err)))))))
              (isearch-update)
            (goto-char isearch-barrier)
            (setq isearch-adjusted t isearch-success t)
            (isearch-search-and-update))))))
  (set-text-properties (minibuffer-prompt-end) (point-max) nil)
  (when-let ((fail-pos (isearch-fail-pos)))
    (add-text-properties (+ (minibuffer-prompt-end) fail-pos)
                         (point-max)
                         '(face isearch-fail)))
  (when isearch-error
    (isearch-mb--message isearch-error)))

(defun isearch-mb--message (message)
  "Display a momentary MESSAGE."
  (when isearch-mb-local-mode
    (message (propertize (concat " [" message "]")
                         'face 'minibuffer-prompt))))

(defun isearch-mb--prompt (&rest _)
  "Update the minibuffer prompt according to search status."
  (prog1 isearch-mb-local-mode
    (when isearch-mb--prompt-overlay
      (overlay-put isearch-mb--prompt-overlay
                   'before-string
                   (concat
                    (when isearch-lazy-count
                      (format "%-6s" (isearch-lazy-count-format)))
                    (capitalize
                     (isearch--describe-regexp-mode
                      isearch-regexp-function)))))))

(defun isearch-mb--with-buffer (fn &rest args)
  "Call FN with ARGS in the search buffer.
Intended as an advice for Isearch commands."
  (if (and (minibufferp)
           (not (eq (current-buffer) isearch--current-buffer)))
      (let ((enable-recursive-minibuffers t)
            (inhibit-redisplay t))
        (with-minibuffer-selected-window
          (apply fn args)
          (unless isearch-mode
            (throw 'isearch-mb--after-exit '(ignore)))))
    (apply fn args)))

(defun isearch-mb--after-exit (fn &rest args)
  "Call FN with ARGS, after quitting Isearch-Mb.
Intended as an advice for commands that quit Isearch and use the
minibuffer."
  (if (and (minibufferp)
             (not (eq (current-buffer) isearch--current-buffer)))
      (throw 'isearch-mb--after-exit (cons fn args))
    (apply fn args)))

(defun isearch-mb--session ()
  "Read search string from the minibuffer."
  (condition-case nil
      (apply
       (catch 'isearch-mb--after-exit
         (let (;; We need to set `inhibit-redisplay' at certain points to
               ;; avoid flicker.  As a side effect, window-start/end in
               ;; `isearch-lazy-highlight-update' will have incorrect values,
               ;; so we need to lazy-highlight the whole buffer.
               (lazy-highlight-buffer (not (null isearch-lazy-highlight))))
           (minibuffer-with-setup-hook
               (lambda ()
                 (add-hook 'post-command-hook 'isearch-mb--post-command-hook nil 'local)
                 (setq isearch-mb--prompt-overlay (make-overlay (point-min) (point-min)
                                                                (current-buffer) t t))
                 (isearch-mb--prompt)
                 (when isearch-error
                   (message (propertize (concat " [" isearch-error "]")
                                        'face 'minibuffer-prompt))))
             (read-from-minibuffer
              "I-search: "
              nil
              isearch-mb-minibuffer-map
              nil
              (if isearch-regexp 'regexp-search-ring 'search-ring)
              (thread-last '(region url symbol sexp line) ;; TODO: make customizable
                (mapcar 'thing-at-point)
                (delq nil)
                (delete-dups)
                (mapcar (if isearch-regexp 'regexp-quote 'identity)))
              t))
           (if isearch-mode '(isearch-done) '(ignore)))))
    (quit (if isearch-mode (isearch-cancel) (signal 'quit nil)))))

(defun isearch-mb--setup ()
  "Arrange to start Isearch-Mb after this command, if applicable."
  (when isearch-mb-local-mode
    (setq overriding-terminal-local-map nil)
    ;; When `with-isearch-suspended' is involved, this hook may run
    ;; more than once, hence the test for `isearch-mode'.
    (run-with-idle-timer 0 nil (lambda() (when isearch-mode (isearch-mb--session))))))

(add-hook 'isearch-mode-hook 'isearch-mb--setup)

(put 'next-history-element 'isearch-mb--no-search t)
(put 'previous-history-element 'isearch-mb--no-search t)

(advice-add 'isearch-message :before-until 'isearch-mb--prompt)
(advice-add 'isearch--momentary-message :before-until 'isearch-mb--message)
(advice-add 'isearch-pre-command-hook :before-until (lambda () isearch-mb-local-mode))
(advice-add 'isearch-post-command-hook :around 'isearch-mb--with-buffer)

(advice-add 'isearch-beginning-of-buffer   :around 'isearch-mb--with-buffer)
(advice-add 'isearch-end-of-buffer         :around 'isearch-mb--with-buffer)
(advice-add 'isearch-occur                 :around 'isearch-mb--with-buffer)
(advice-add 'isearch-repeat-backward       :around 'isearch-mb--with-buffer)
(advice-add 'isearch-repeat-forward        :around 'isearch-mb--with-buffer)
(advice-add 'isearch-toggle-case-fold      :around 'isearch-mb--with-buffer)
(advice-add 'isearch-toggle-char-fold      :around 'isearch-mb--with-buffer)
(advice-add 'isearch-toggle-invisible      :around 'isearch-mb--with-buffer)
(advice-add 'isearch-toggle-lax-whitespace :around 'isearch-mb--with-buffer)
(advice-add 'isearch-toggle-regexp         :around 'isearch-mb--with-buffer)
(advice-add 'isearch-toggle-symbol         :around 'isearch-mb--with-buffer)
(advice-add 'isearch-toggle-word           :around 'isearch-mb--with-buffer)

(advice-add 'isearch-query-replace                   :around 'isearch-mb--after-exit)
(advice-add 'isearch-query-replace-regexp            :around 'isearch-mb--after-exit)
(advice-add 'isearch-highlight-regexp                :around 'isearch-mb--after-exit)
(advice-add 'isearch-highlight-lines-matching-regexp :around 'isearch-mb--after-exit)

;;;###autoload
(define-minor-mode isearch-mb-local-mode
  "Control Isearch from the minibuffer.

During an Isearch-Mb session, the following keys are available:
\\{isearch-mb-minibuffer-map}")

;;;###autoload
(define-globalized-minor-mode isearch-mb-mode
  isearch-mb-local-mode
  (lambda () (unless (minibufferp) (isearch-mb-local-mode))))

(provide 'isearch-mb)
;;; isearch-mb.el ends here
