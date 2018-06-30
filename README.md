# Night Owl for Emacs

[![MELPA](https://melpa.org/packages/night-owl-theme-badge.svg)](https://melpa.org/#/night-owl-theme)
[![MELPA Stable](https://stable.melpa.org/packages/night-owl-theme-badge.svg)](https://stable.melpa.org/#/night-owl-theme)

Based entirely on Sarah Drasner's amazing [Night Owl VSCode Theme][]. Built by
modifying Kelvin Smith's [monokai-emacs][]. Many thanks to both Sarah Drasner
and Kelvin Smith.

![Screenshot of Night Owl Theme for Emacs](https://user-images.githubusercontent.com/8588/41229702-dbc79340-6d31-11e8-9581-7c168b1fb693.png)

## Status

Initial development, but covers most of my use cases. PRs gladly accepted.

[night owl vscode theme]: https://github.com/sdras/night-owl-vscode-theme
[monokai-emacs]: https://github.com/oneKelvinSmith/monokai-emacs

## Tips

### Ivy

To style the non-selected ivy items, try this:

```elisp
(defun night-owl/ivy-format-function-line (cands)
  "Transform CANDS into a string for minibuffer."
  (let ((str (ivy-format-function-line cands)))
    (font-lock-append-text-property 0 (length str) 'face 'ivy-not-current str)
    str))

(setq ivy-format-function #'night-owl/ivy-format-function-line)
```

### Spacemacs/evil

To style spacemacs cursors, try this:

```elisp
(setq night-owl-evil-cursors
      '(("normal" night-owl-orange box)
        ("insert" night-owl-cursor (bar . 2))
        ("emacs" night-owl-cursor (bar . 2))
        ("hybrid" night-owl-cursor (bar . 2))
        ("replace" night-owl-gray (hbar . 2))
        ("evilified" night-owl-yellow box)
        ("visual" night-owl-gray (hbar . 2))
        ("motion" night-owl-violet box)
        ("lisp" night-owl-red box)
        ("iedit" night-owl-magenta box)
        ("iedit-insert" night-owl-magenta (bar . 2))))

(defun night-owl-set-evil-cursor (state color shape)
  (set (intern (format "evil-%s-state-cursor" state))
       (list color shape)))

(defun night-owl-update-evil-cursors ()
  (let ((current-theme (symbol-name (car custom-enabled-themes))))
    (if (string-prefix-p "night-owl" current-theme)
        (cl-loop for (state color style) in night-owl-evil-cursors
                 do
                 (night-owl-set-evil-cursor state (symbol-value color) style))
      ;; not night-owl theme, restore default spacemacs cursors
      (if (functionp (spacemacs/add-evil-cursor))
          (cl-loop for (state color shape) in spacemacs-evil-cursors
                   do (spacemacs/add-evil-cursor state color shape))))))

;; For spacemacs (comment this out if not using spacemacs):
(add-hook 'spacemacs-post-theme-change-hook #'night-owl-update-evil-cursors)

;; For Evil (uncomment this if not using spacemacs):
;; (defvar after-load-theme-hook nil
;;   "Hook run after a color theme is loaded using `load-theme'.")
;; 
;; (defadvice load-theme (after run-after-load-theme-hook activate)
;;   "Run `after-load-theme-hook'."
;;   (message (format "Loading theme %s" (ad-get-arg 0)))
;;   (run-hooks 'after-load-theme-hook))
;; 
;; (add-hook 'after-load-theme-hook #'night-owl-update-evil-cursors)
```
