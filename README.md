# Night Owl for Emacs

Based entirely on Sarah Drasner's amazing [Night Owl VSCode Theme][]. Built by
modifying Kelvin Smith's [monokai-emacs][]. Many thanks to both Sarah Drasner
and Kelvin Smith.

![Screenshot of Night Owl Theme for Emacs](https://user-images.githubusercontent.com/8588/41229702-dbc79340-6d31-11e8-9581-7c168b1fb693.png)

## Status

Very initial development. Still contains some of the monokai colors. PRs gladly accepted.

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
