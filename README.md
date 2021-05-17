Isearch-Mb
==========

This Emacs package provides an alternative Isearch UI based on the
minibuffer.  This allows editing the search string in arbitrary ways
without any special maneuver; unlike standard Isearch, cursor motion
commands do not end the search.  Moreover, in comparison with standard
Isearch, the search status information in the echo area is simplified.

Isearch-Mb is part of [GNU ELPA] and can be installed via `M-x
list-packages`.  To activate it, type `M-x isearch-mb-mode`.


Keybindings
-----------

During a search, `isearch-mb-minibuffer-map` is active.  By default,
it includes the following commands:

- <kbd>C-s</kbd>, <kbd>↓</kbd>: `isearch-repeat-forward`
- <kbd>C-r</kbd>, <kbd>↑</kbd>: `isearch-repeat-backward`
- <kbd>M-<</kbd>: `isearch-beginning-of-buffer`
- <kbd>M-></kbd>: `isearch-end-of-buffer`
- <kbd>M-%</kbd>: `isearch-query-replace`
- <kbd>C-M-%</kbd>: `isearch-query-replace-regexp`
- <kbd>M-s</kbd> prefix: similar to standard Isearch

Everything else works as in a plain minibuffer, e.g. <kbd>RET</kbd>
ends the search normally and <kbd>C-g</kbd> cancels it.

Some customization ideas
------------------------

Isearch provides a myriad of customization options, and most of them
make just as much sense in Isearch-Mb mode.  The following are some
uncontroversial settings that would make sense for most people:

``` elisp
(setq-default
 ;; Match count next to minibuffer prompt
 isearch-lazy-count t
 ;; Don't be stingy with history; default is to keep just 16 entries
 search-ring-max 200
 regexp-search-ring-max 200)
```

Note that since Isearch-Mb relies on a regular minibuffer, you can use
you favorite tool to browse the history of previous search strings
(say, the `consult-history` command from the excellent [Consult]
package).

Using regexp search by default is a popular option as well:

```elisp
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
```

For a Swiper-style fuzzy search, where spaces match any sequence of
characters in a line, use the settings below.  You can still toggle
strict whitespace matching with <kbd>M-s SPC</kbd> during a search.

``` elisp
(setq-default
 isearch-regexp-lax-whitespace t
 search-whitespace-regexp ".*?")
```

Interaction with other Isearch extensions
-----------------------------------------

Some third-party Isearch extensions require a bit of configuration in
order to work with Isearch-Mb.  There are three cases to consider:

- **Commands that start a search** shouldn't require extra
  configuration.

- **Commands that operate during a search session** should be added to
  the list `isearch-mb--with-buffer`.  Examples of this case are
  [`loccur-isearch`][loccur] and [`consult-isearch`][consult].

  ``` elisp
  (add-to-list 'isearch-mb--with-buffer #'loccur-isearch)
  (define-key isearch-mb-minibuffer-map (kbd "C-o") #'loccur-isearch)

  (add-to-list 'isearch-mb--with-buffer #'consult-isearch)
  (define-key isearch-mb-minibuffer-map (kbd "M-r") #'consult-isearch)
  ```

  Most Isearch commands that are not made available by default in
  Isearch-Mb can also be used in this fashion:

  ``` elisp
  (add-to-list 'isearch-mb--with-buffer #'isearch-yank-word)
  (define-key isearch-mb-minibuffer-map (kbd "M-s C-w") #'isearch-yank-word)
  ```

- **Commands that end the Isearch session** should be added to the
  list `isearch-mb--after-exit`.  Examples of this case are
  [`anzu-isearch-query-replace`][anzu] and [`consult-line`][consult]:

  ``` elisp
  (add-to-list 'isearch-mb--after-exit #'anzu-isearch-query-replace)
  (define-key isearch-mb-minibuffer-map (kbd "M-%") 'anzu-isearch-query-replace)

  (add-to-list 'isearch-mb--after-exit #'consult-line)
  (define-key isearch-mb-minibuffer-map (kbd "M-s l") 'consult-line)
  ```

  Making motion commands quit the search as in standard Isearch is out
  of the scope of this package, but you can define your own commands
  to emulate that effect.  Here is one possibility:

  ```elisp
  (defun move-end-of-line-maybe-ending-isearch (arg)
  "End search and move to end of line, but only if already at the end of the minibuffer."
    (interactive "p")
    (if (eobp)
        (isearch-mb--after-exit
         (lambda ()
           (move-end-of-line arg)
           (isearch-done)))
      (move-end-of-line arg)))

  (define-key isearch-mb-minibuffer-map (kbd "C-e") 'move-end-of-line-maybe-ending-isearch)
  ```

[GNU ELPA]: https://elpa.gnu.org/
[consult]: https://github.com/minad/consult
[loccur]: https://github.com/fourier/loccur#isearch-integration
[anzu]: https://github.com/emacsorphanage/anzu
