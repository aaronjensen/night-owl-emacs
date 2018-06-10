;;; night-owl-theme.el --- A fruity color theme for Emacs.

;; Copyright (C) 2011-2016

;; Author: Kelvin Smith <oneKelvinSmith@gmail.com>
;; URL: http://github.com/oneKelvinSmith/night-owl-emacs
;; Package-Version: 20180402.221
;; Version: 3.5.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the popular Textmate theme Night-Owl for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Credits:
;;
;; Wimer Hazenberg created the original theme.
;; - http://www.night-owl.nl/blog/2006/07/15/textmate-color-theme/
;;
;; Bozhidar Batsov created zenburn-theme.el and solarized-theme.el
;;  on which this file is based.
;; - https://github.com/bbatsov/zenburn-emacs
;;
;; Color Scheme Designer 3 for complementary colours.
;; - http://colorschemedesigner.com/
;;
;; Xterm 256 Color Chart
;; - https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg
;;
;; K. Adam Christensen for his personal night-owl theme that addresses 256 colours.
;; - https://github.com/pope/personal/blob/master/etc/emacs.d/night-owl-theme.el
;;
;; Thomas Frössman for his work on solarized-emacs.
;; - http://github.com/bbatsov/solarized-emacs
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The night-owl theme requires Emacs 24 or later!"))

(deftheme night-owl "The Night-Owl colour theme")

(defgroup night-owl nil
  "Night-Owl theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom night-owl-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'night-owl)

(defcustom night-owl-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'night-owl)

(defcustom night-owl-doc-face-as-comment nil
  "Consider `font-lock-doc-face' as comment instead of a string."
  :type 'boolean
  :group 'night-owl
  :package-version "3.5.1")

(defcustom night-owl-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'night-owl)

(defcustom night-owl-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'night-owl)

(defcustom night-owl-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'night-owl)

(defcustom night-owl-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'night-owl)

(defcustom night-owl-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'night-owl)

;; Primary colors
(defcustom night-owl-yellow "#E6DB74"
  "Primary colors - yellow"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-orange "#FD971F"
  "Primary colors - orange"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-red "#F92672"
  "Primary colors - red"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-magenta "#FD5FF0"
  "Primary colors - magenta"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-blue "#66D9EF"
  "Primary colors - blue"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-green "#A6E22E"
  "Primary colors - green"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-cyan "#A1EFE4"
  "Primary colors - cyan"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-violet "#AE81FF"
  "Primary colors - violet"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-gray "#64645E"
  "Primary colors - gray"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-foreground "#D6DEEB"
  "Adaptive colors - foreground"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-background "#011627"
  "Adaptive colors - background"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-comments "#637777"
  "Adaptive colors - comments"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-emphasis "#F8F8F0"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-line-number "#8F908A"
  "Adaptive colors - line number"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-highlight "#49483E"
  "Adaptive colors - highlight"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-highlight-alt "#3E3D31"
  "Adaptive colors - highlight"
  :type 'string
  :group 'night-owl)

(defcustom night-owl-highlight-line "#3C3D37"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'night-owl)

(let* (;; Variable pitch
       (night-owl-pitch (if night-owl-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (night-owl-class '((class color) (min-colors 257)))

       ;; Darker and lighter accented colors
       (night-owl-yellow-d       "#BEB244")
       (night-owl-yellow-l       "#FFF7A8")
       (night-owl-orange-d       "#D47402")
       (night-owl-orange-l       "#FFAC4A")
       (night-owl-red-d          "#F70057")
       (night-owl-red-l          "#FA518D")
       (night-owl-magenta-d      "#FB35EA")
       (night-owl-magenta-l      "#FE8CF4")
       (night-owl-violet-d       "#945AFF")
       (night-owl-violet-l       "#C9ACFF")
       (night-owl-blue-d         "#40CAE4")
       (night-owl-blue-l         "#92E7F7")
       (night-owl-cyan-d         "#74DBCD")
       (night-owl-cyan-l         "#D3FBF6")
       (night-owl-green-d        "#86C30D")
       (night-owl-green-l        "#BBEF53")
       (night-owl-gray-d         "#35331D")
       (night-owl-gray-l         "#7B7962")
       ;; Adaptive higher/lower contrast accented colors
       (night-owl-foreground-hc  "#141414")
       (night-owl-foreground-lc  "#171A0B")
       ;; High contrast colors
       (night-owl-yellow-hc      "#FFFACE")
       (night-owl-yellow-lc      "#9A8F21")
       (night-owl-orange-hc      "#FFBE74")
       (night-owl-orange-lc      "#A75B00")
       (night-owl-red-hc         "#FEB0CC")
       (night-owl-red-lc         "#F20055")
       (night-owl-magenta-hc     "#FEC6F9")
       (night-owl-magenta-lc     "#F309DF")
       (night-owl-violet-hc      "#F0E7FF")
       (night-owl-violet-lc      "#7830FC")
       (night-owl-blue-hc        "#CAF5FD")
       (night-owl-blue-lc        "#1DB4D0")
       (night-owl-cyan-hc        "#D3FBF6")
       (night-owl-cyan-lc        "#4BBEAE")
       (night-owl-green-hc       "#CCF47C")
       (night-owl-green-lc       "#679A01")

       ;; Distinct fringe
       (night-owl-fringe-bg (if night-owl-distinct-fringe-background
                              night-owl-gray
                            night-owl-background)))

  ;; Define faces
  (custom-theme-set-faces
   'night-owl

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(font-lock-comment-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(font-lock-constant-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(font-lock-doc-face
     ((,night-owl-class (:foreground ,(if night-owl-doc-face-as-comment
                                        night-owl-comments
                                      night-owl-yellow)))))

   `(font-lock-function-name-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(font-lock-keyword-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight normal))))

   `(font-lock-negation-char-face
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold))))

   `(font-lock-preprocessor-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(font-lock-regexp-grouping-construct
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,night-owl-class (:foreground ,night-owl-violet
                                   :weight normal))))

   `(font-lock-string-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(font-lock-type-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :italic nil))))

   `(font-lock-variable-name-face
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(font-lock-warning-face
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :weight bold
                                   :italic t
                                   :underline t))))

   `(c-annotation-face
     ((,night-owl-class (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,night-owl-class (:foreground ,night-owl-foreground
                                    :background ,night-owl-background))))

   `(highlight
     ((,night-owl-class (:background ,night-owl-highlight))))

   `(lazy-highlight
     ((,night-owl-class (:inherit highlight
                                :background ,night-owl-highlight-alt))))

   `(region
     ((,night-owl-class (:inherit highlight
                                :background ,night-owl-highlight))))

   `(secondary-selection
     ((,night-owl-class (:inherit region
                                :background ,night-owl-highlight-alt))))

   `(shadow
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(match
     ((,night-owl-class (:background ,night-owl-green
                                   :foreground ,night-owl-background
                                   :weight bold))))

   `(cursor
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-foreground
                                   :inverse-video t))))

   `(mouse
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-foreground
                                   :inverse-video t))))

   `(escape-glyph
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(escape-glyph-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(fringe
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :background ,night-owl-fringe-bg))))

   `(link
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :underline t
                                   :weight bold))))

   `(link-visited
     ((,night-owl-class (:foreground ,night-owl-violet
                                   :underline t
                                   :weight normal))))

   `(success
     ((,night-owl-class (:foreground ,night-owl-green ))))

   `(warning
     ((,night-owl-class (:foreground ,night-owl-yellow ))))

   `(error
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(eval-sexp-fu-flash
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-green))))

   `(eval-sexp-fu-flash-error
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-red))))

   `(trailing-whitespace
     ((,night-owl-class (:background ,night-owl-red))))

   `(vertical-border
     ((,night-owl-class (:foreground ,night-owl-gray))))

   `(menu
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :background ,night-owl-background))))

   `(minibuffer-prompt
     ((,night-owl-class (:foreground ,night-owl-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,night-owl-class (:foreground ,night-owl-green
                                   :weight bold))))

   `(mode-line
     ((,night-owl-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,night-owl-emphasis
                                      :background ,night-owl-highlight
                                      :box (:line-width 1
                                                        :color ,night-owl-gray
                                                        :style unspecified)))))

   `(powerline-active1
     ((,night-owl-class (:background ,night-owl-gray-d))))

   `(powerline-active2
     ((,night-owl-class (:background ,night-owl-background))))


   `(mode-line-inactive
     ((,night-owl-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,night-owl-comments
                                      :background ,night-owl-background
                                      :box (:line-width 1
                                                        :color ,night-owl-gray
                                                        :style unspecified)))))

   `(powerline-inactive1
     ((,night-owl-class (:background ,night-owl-gray-d))))

   `(powerline-inactive2
     ((,night-owl-class (:background ,night-owl-background))))

   ;; header-line
   `(header-line
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-highlight
                                   :box (:color ,night-owl-gray
                                                :line-width 1
                                                :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,night-owl-class (:background ,night-owl-yellow
                                   :foreground ,night-owl-background))))

   `(cua-rectangle
     ((,night-owl-class (:inherit region))))

   `(cua-rectangle-noselect
     ((,night-owl-class (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   ;; dired
   `(dired-directory
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(dired-flagged
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(dired-header
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :background ,night-owl-background
                                   :inherit bold))))

   `(dired-ignored
     ((,night-owl-class (:inherit shadow))))

   `(dired-mark
     ((,night-owl-class (:foreground ,night-owl-green
                                   :weight bold))))

   `(dired-marked
     ((,night-owl-class (:foreground ,night-owl-violet
                                   :inherit bold))))

   `(dired-perm-write
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :underline t))))

   `(dired-symlink
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :slant italic))))

   `(dired-warning
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-blue))))

   `(dropdown-list-selection-face
     ((,night-owl-class (:background ,night-owl-green
                                   :foreground ,night-owl-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,night-owl-class (:inherit ecb-history-bucket-node-face
                                :foreground ,night-owl-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,night-owl-class (:inherit ecb-directories-general-face
                                :foreground ,night-owl-foreground))))

   `(ecb-history-dead-buffer-face
     ((,night-owl-class (:inherit ecb-history-general-face
                                :foreground ,night-owl-comments))))

   `(ecb-directory-not-accessible-face
     ((,night-owl-class (:inherit ecb-directories-general-face
                                :foreground ,night-owl-comments))))

   `(ecb-bucket-node-face
     ((,night-owl-class (:inherit ecb-default-general-face
                                :weight normal
                                :foreground ,night-owl-blue))))

   `(ecb-tag-header-face
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,night-owl-class (:inherit ecb-analyse-general-face
                                :foreground ,night-owl-green))))

   `(ecb-directories-general-face
     ((,night-owl-class (:inherit ecb-default-general-face
                                :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,night-owl-class (:inherit ecb-methods-general-face
                                :foreground ,night-owl-cyan))))

   `(ecb-mode-line-prefix-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(ecb-tree-guide-line-face
     ((,night-owl-class (:inherit ecb-default-general-face
                                :foreground ,night-owl-gray
                                :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,night-owl-class (:foreground ,night-owl-emphasis))))

   `(ee-category
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(ee-link
     ((,night-owl-class (:inherit link))))

   `(ee-link-visited
     ((,night-owl-class (:inherit link-visited))))

   `(ee-marked
     ((,night-owl-class (:foreground ,night-owl-magenta
                                   :weight bold))))

   `(ee-omitted
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(ee-shadow
     ((,night-owl-class (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(grep-error-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight bold
                                   :underline t))))

   `(grep-hit-face
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(grep-match-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :weight bold))))

   ;; isearch
   `(isearch
     ((,night-owl-class (:inherit region
                                :foreground ,night-owl-background
                                :background ,night-owl-yellow))))

   `(isearch-fail
     ((,night-owl-class (:inherit isearch
                                :foreground ,night-owl-red
                                :background ,night-owl-background
                                :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :background ,night-owl-background
                                   :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :background ,night-owl-background
                                   :inverse-video nil
                                   :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,night-owl-class (:inherit bold
                                :foreground ,night-owl-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,night-owl-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,night-owl-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,night-owl-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,night-owl-class (:inherit italic :foreground ,night-owl-emphasis))))

   `(font-latex-math-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(font-latex-sectioning-0-face
     ((,night-owl-class (:inherit font-latex-sectioning-1-face
                                :height ,night-owl-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,night-owl-class (:inherit font-latex-sectioning-2-face
                                :height ,night-owl-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,night-owl-class (:inherit font-latex-sectioning-3-face
                                :height ,night-owl-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,night-owl-class (:inherit font-latex-sectioning-4-face
                                :height ,night-owl-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,night-owl-class (:inherit font-latex-sectioning-5-face
                                :height ,night-owl-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-yellow
                                :weight bold))))

   `(font-latex-sedate-face
     ((,night-owl-class (:foreground ,night-owl-emphasis))))

   `(font-latex-slide-title-face
     ((,night-owl-class (:inherit (,night-owl-pitch font-lock-type-face)
                                :weight bold
                                :height ,night-owl-height-plus-3))))

   `(font-latex-string-face
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   `(font-latex-subscript-face
     ((,night-owl-class (:height ,night-owl-height-minus-1))))

   `(font-latex-superscript-face
     ((,night-owl-class (:height ,night-owl-height-minus-1))))

   `(font-latex-verbatim-face
     ((,night-owl-class (:inherit fixed-pitch
                                :foreground ,night-owl-foreground
                                :slant italic))))

   `(font-latex-warning-face
     ((,night-owl-class (:inherit bold
                                :foreground ,night-owl-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-blue))))

   `(ac-selection-face
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-background))))

   `(ac-candidate-mouse-face
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-background))))

   `(ac-completion-face
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :underline t))))

   `(ac-gtags-candidate-face
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-blue))))

   `(ac-gtags-selection-face
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-background))))

   `(ac-yasnippet-candidate-face
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-yellow))))

   `(ac-yasnippet-selection-face
     ((,night-owl-class (:background ,night-owl-yellow
                                   :foreground ,night-owl-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-blue))))

   `(ahs-edit-mode-face
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-highlight))))

   `(ahs-face
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-yellow))))

   `(ahs-plugin-bod-face
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-violet ))))

   `(ahs-plugin-defalt-face
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-green))))

   `(ahs-warning-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(android-mode-error-face
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :weight bold))))

   `(android-mode-info-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(android-mode-verbose-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(android-mode-warning-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,night-owl-class (:foreground ,night-owl-violet
                                   :weight bold))))

   ;; bm
   `(bm-face
     ((,night-owl-class (:background ,night-owl-yellow-lc
                                   :foreground ,night-owl-background))))

   `(bm-fringe-face
     ((,night-owl-class (:background ,night-owl-yellow-lc
                                   :foreground ,night-owl-background))))

   `(bm-fringe-persistent-face
     ((,night-owl-class (:background ,night-owl-green-lc
                                   :foreground ,night-owl-background))))

   `(bm-persistent-face
     ((,night-owl-class (:background ,night-owl-green-lc
                                   :foreground ,night-owl-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(cfw:face-annotation
     ((,night-owl-class (:inherit cfw:face-day-title
                                :foreground ,night-owl-yellow))))

   `(cfw:face-default-content
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(cfw:face-default-day
     ((,night-owl-class (:inherit cfw:face-day-title
                                :weight bold))))

   `(cfw:face-disable
     ((,night-owl-class (:inherit cfw:face-day-title
                                :foreground ,night-owl-comments))))

   `(cfw:face-grid
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(cfw:face-header
     ((,night-owl-class (:foreground ,night-owl-blue-hc
                                   :background ,night-owl-blue-lc
                                   :weight bold))))

   `(cfw:face-holiday
     ((,night-owl-class (:background nil
                                   :foreground ,night-owl-red
                                   :weight bold))))

   `(cfw:face-periods
     ((,night-owl-class (:foreground ,night-owl-magenta))))

   `(cfw:face-select
     ((,night-owl-class (:background ,night-owl-magenta-lc
                                   :foreground ,night-owl-magenta-hc))))

   `(cfw:face-saturday
     ((,night-owl-class (:foreground ,night-owl-cyan-hc
                                   :background ,night-owl-cyan-lc))))

   `(cfw:face-sunday
     ((,night-owl-class (:foreground ,night-owl-red-hc
                                   :background ,night-owl-red-lc
                                   :weight bold))))

   `(cfw:face-title
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-yellow
                                :weight bold
                                :height ,night-owl-height-plus-4))))

   `(cfw:face-today
     ((,night-owl-class (:weight bold
                               :background ,night-owl-highlight-line
                               :foreground nil))))

   `(cfw:face-today-title
     ((,night-owl-class (:background ,night-owl-yellow-lc
                                   :foreground ,night-owl-yellow-hc
                                   :weight bold))))

   `(cfw:face-toolbar
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-foreground))))

   `(cfw:face-toolbar-button-off
     ((,night-owl-class (:background ,night-owl-yellow-lc
                                   :foreground ,night-owl-yellow-hc
                                   :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,night-owl-class (:background ,night-owl-yellow-hc
                                   :foreground ,night-owl-yellow-lc
                                   :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :background nil
                                   :box (:color ,night-owl-yellow :line-width -1 :style nil)))))

   `(cider-enlightened-local
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(cider-instrumented-face
     ((,night-owl-class (:foreground ,night-owl-violet
                                   :background nil
                                   :box (:color ,night-owl-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :background nil
                                   :box (:color ,night-owl-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-orange))))

   `(cider-test-failure-face
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-red))))

   `(cider-test-success-face
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-green))))

   `(cider-traced-face
     ((,night-owl-class :box (:color ,night-owl-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight bold
                                   :underline t))))

   `(clojure-test-error-face
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :weight bold
                                   :underline t))))

   `(clojure-test-success-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :weight bold
                                   :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-emphasis))))

   `(company-tooltip-selection
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-background))))

   `(company-tooltip-mouse
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-background))))

   `(company-tooltip-common
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :underline t))))

   `(company-tooltip-common-selection
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-blue
                                   :underline t))))

   `(company-preview
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-emphasis))))

   `(company-preview-common
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :underline t))))

   `(company-scrollbar-bg
     ((,night-owl-class (:background ,night-owl-gray))))

   `(company-scrollbar-fg
     ((,night-owl-class (:background ,night-owl-comments))))

   `(company-tooltip-annotation
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-green))))

   `(company-template-field
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-blue))))

   ;; compilation
   `(compilation-column-face
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :underline nil))))

   `(compilation-column-number
     ((,night-owl-class (:inherit font-lock-doc-face
                                :foreground ,night-owl-cyan
                                :underline nil))))

   `(compilation-enter-directory-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :underline nil))))

   `(compilation-error
     ((,night-owl-class (:inherit error
                                :underline nil))))

   `(compilation-error-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :underline nil))))

   `(compilation-face
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :underline nil))))

   `(compilation-info
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :underline nil
                                   :bold nil))))

   `(compilation-info-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :underline nil))))

   `(compilation-leave-directory-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :underline nil))))

   `(compilation-line-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :underline nil))))

   `(compilation-line-number
     ((,night-owl-class (:foreground ,night-owl-green
                                   :underline nil))))

   `(compilation-warning
     ((,night-owl-class (:inherit warning
                                :underline nil))))

   `(compilation-warning-face
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight normal
                                   :underline nil))))

   `(compilation-mode-line-exit
     ((,night-owl-class (:inherit compilation-info
                                :foreground ,night-owl-green
                                :weight bold))))

   `(compilation-mode-line-fail
     ((,night-owl-class (:inherit compilation-error
                                :foreground ,night-owl-red
                                :weight bold))))

   `(compilation-mode-line-run
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :weight bold))))

   `(cscope-function-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(cscope-line-number-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(cscope-line-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(cscope-mouse-face
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-emphasis
                                   :underline ,night-owl-emphasis
                                   :weight bold))))

   `(ctbl:face-continue-bar
     ((,night-owl-class (:background ,night-owl-gray
                                   :foreground ,night-owl-yellow))))

   `(ctbl:face-row-select
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-foreground
                                   :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold))))

   `(coffee-mode-function-param
     ((,night-owl-class (:foreground ,night-owl-violet
                                   :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :height ,night-owl-height-plus-3
                                :foreground ,night-owl-violet
                                :weight bold))))

   `(custom-variable-tag
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-cyan
                                :height ,night-owl-height-plus-3))))

   `(custom-comment-tag
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(custom-group-tag
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-blue
                                :height ,night-owl-height-plus-3))))

   `(custom-group-tag-1
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-red
                                :height ,night-owl-height-plus-3))))

   `(custom-state
     ((,night-owl-class (:foreground ,night-owl-green))))

   ;; diff
   `(diff-added
     ((,night-owl-class (:foreground ,night-owl-green
                                   :background ,night-owl-background))))

   `(diff-changed
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :background ,night-owl-background))))

   `(diff-removed
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-background))))

   `(diff-header
     ((,night-owl-class (:background ,night-owl-background))))

   `(diff-file-header
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-foreground
                                   :weight bold))))

   `(diff-refine-added
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-green))))

   `(diff-refine-change
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-blue))))

   `(diff-refine-removed
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,night-owl-class (:background ,night-owl-blue-lc
                                   :foreground ,night-owl-blue-hc))))

   `(diff-hl-delete
     ((,night-owl-class (:background ,night-owl-red-lc
                                   :foreground ,night-owl-red-hc))))

   `(diff-hl-insert
     ((,night-owl-class (:background ,night-owl-green-lc
                                   :foreground ,night-owl-green-hc))))

   `(diff-hl-unknown
     ((,night-owl-class (:background ,night-owl-violet-lc
                                   :foreground ,night-owl-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,night-owl-class (:background ,night-owl-orange-lc))))

   `(ediff-fine-diff-B
     ((,night-owl-class (:background ,night-owl-green-lc))))

   `(ediff-fine-diff-C
     ((,night-owl-class (:background ,night-owl-yellow-lc))))

   `(ediff-current-diff-C
     ((,night-owl-class (:background ,night-owl-blue-lc))))

   `(ediff-even-diff-A
     ((,night-owl-class (:background ,night-owl-comments
                                   :foreground ,night-owl-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,night-owl-class (:background ,night-owl-comments
                                   :foreground ,night-owl-foreground-hc ))))

   `(ediff-even-diff-B
     ((,night-owl-class (:background ,night-owl-comments
                                   :foreground ,night-owl-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,night-owl-class (:background ,night-owl-comments
                                   :foreground ,night-owl-foreground-lc ))))

   `(ediff-even-diff-C
     ((,night-owl-class (:background ,night-owl-comments
                                   :foreground ,night-owl-foreground ))))

   `(ediff-odd-diff-C
     ((,night-owl-class (:background ,night-owl-comments
                                   :foreground ,night-owl-background ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) night-owl-class)
       (:underline (:style line :color ,night-owl-red)
                   :inherit unspecified))
      (,night-owl-class (:foreground ,night-owl-red-hc
                                   :background ,night-owl-red-lc
                                   :weight bold
                                   :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) night-owl-class)
       (:underline (:style line :color ,night-owl-yellow)
                   :inherit unspecified))
      (,night-owl-class (:foreground ,night-owl-yellow-hc
                                   :background ,night-owl-yellow-lc
                                   :weight bold
                                   :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background unspecified
                                   :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :background unspecified
                                   :weight bold))))

   `(edts-face-error-mode-line
     ((,night-owl-class (:background ,night-owl-red
                                   :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,night-owl-class (:background ,night-owl-yellow
                                   :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(elfeed-search-feed-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(elfeed-search-tag-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(elfeed-search-title-face
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   ;; elixir
   `(elixir-attribute-face
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(elixir-atom-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   ;; ein
   `(ein:cell-input-area
     ((,night-owl-class (:background ,night-owl-highlight-line))))
   `(ein:cell-input-prompt
     ((,night-owl-class (:foreground ,night-owl-green))))
   `(ein:cell-output-prompt
     ((,night-owl-class (:foreground ,night-owl-red))))
   `(ein:notification-tab-normal
     ((,night-owl-class (:foreground ,night-owl-blue))))
   `(ein:notification-tab-selected
     ((,night-owl-class (:foreground ,night-owl-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,night-owl-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,night-owl-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,night-owl-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,night-owl-class (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) night-owl-class)
       (:underline (:style wave :color ,night-owl-red)
                   :inherit unspecified))
      (,night-owl-class (:foreground ,night-owl-red-hc
                                   :background ,night-owl-red-lc
                                   :weight bold
                                   :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) night-owl-class)
       (:underline (:style wave :color ,night-owl-orange)
                   :inherit unspecified))
      (,night-owl-class (:foreground ,night-owl-orange-hc
                                   :background ,night-owl-orange-lc
                                   :weight bold
                                   :underline t))))

   ;; epc
   `(epc:face-title
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :background ,night-owl-background
                                   :weight normal
                                   :underline nil))))

   ;; erc
   `(erc-action-face
     ((,night-owl-class (:inherit erc-default-face))))

   `(erc-bold-face
     ((,night-owl-class (:weight bold))))

   `(erc-current-nick-face
     ((,night-owl-class (:foreground ,night-owl-blue :weight bold))))

   `(erc-dangerous-host-face
     ((,night-owl-class (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(erc-highlight-face
     ((,night-owl-class (:inherit erc-default-face
                                :background ,night-owl-highlight))))

   `(erc-direct-msg-face
     ((,night-owl-class (:inherit erc-default-face))))

   `(erc-error-face
     ((,night-owl-class (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,night-owl-class (:inherit erc-default-face))))

   `(erc-input-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(erc-keyword-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :weight bold))))

   `(erc-nick-default-face
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold))))

   `(erc-my-nick-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight bold))))

   `(erc-nick-msg-face
     ((,night-owl-class (:inherit erc-default-face))))

   `(erc-notice-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(erc-pal-face
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :weight bold))))

   `(erc-prompt-face
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :background ,night-owl-background
                                   :weight bold))))

   `(erc-timestamp-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :inherit bold))))

   `(eshell-ls-archive
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight bold))))

   `(eshell-ls-backup
     ((,night-owl-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,night-owl-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :inherit bold))))

   `(eshell-ls-executable
     ((,night-owl-class (:foreground ,night-owl-green
                                   :inherit bold))))

   `(eshell-ls-unreadable
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(eshell-ls-missing
     ((,night-owl-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,night-owl-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :inherit bold))))

   `(eshell-ls-symlink
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-red-l
                                   :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-green-l
                                   :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,night-owl-class (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-orange
                                   :underline t
                                   :slant italic))))

   `(fic-face
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-orange
                                   :weight normal
                                   :slant italic))))

   `(font-lock-fic-face
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-orange
                                   :weight normal
                                   :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :weight normal
                                   :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) night-owl-class)
       (:underline (:style wave :color ,night-owl-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,night-owl-class (:foreground ,night-owl-red-hc
                                   :background ,night-owl-red-lc
                                   :weight bold
                                   :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) night-owl-class)
       (:underline (:style wave :color ,night-owl-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,night-owl-class (:foreground ,night-owl-green-hc
                                   :background ,night-owl-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) night-owl-class)
       (:underline (:style wave :color ,night-owl-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,night-owl-class (:foreground ,night-owl-yellow-hc
                                   :background ,night-owl-yellow-lc
                                   :weight bold
                                   :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) night-owl-class)
       (:underline (:style line :color ,night-owl-red)))
      (,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-background
                                   :weight bold
                                   :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) night-owl-class)
       (:underline (:style line :color ,night-owl-orange)))
      (,night-owl-class (:foreground ,night-owl-orange
                                   :background ,night-owl-background
                                   :weight bold
                                   :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) night-owl-class)
       (:underline (:style line :color ,night-owl-blue)))
      (,night-owl-class (:foreground ,night-owl-blue
                                   :background ,night-owl-background
                                   :weight bold
                                   :underline t))))

   `(flycheck-fringe-error
     ((,night-owl-class (:foreground ,night-owl-red-l
                                   :background unspecified
                                   :weight bold))))

   `(flycheck-fringe-warning
     ((,night-owl-class (:foreground ,night-owl-orange-l
                                   :background unspecified
                                   :weight bold))))

   `(flycheck-fringe-info
     ((,night-owl-class (:foreground ,night-owl-blue-l
                                   :background unspecified
                                   :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) night-owl-class)
       (:underline (:style wave :color ,night-owl-yellow)
                   :inherit unspecified))
      (,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold
                                   :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) night-owl-class)
       (:underline (:style wave :color ,night-owl-red)
                   :inherit unspecified))
      (,night-owl-class (:foreground ,night-owl-red
                                   :weight bold
                                   :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,night-owl-class (:background ,night-owl-green
                                   :foreground ,night-owl-background
                                   :inherit bold))))

   `(git-gutter:deleted
     ((,night-owl-class (:background ,night-owl-red
                                   :foreground ,night-owl-background
                                   :inherit bold))))

   `(git-gutter:modified
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-background
                                   :inherit bold))))

   `(git-gutter:unchanged
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-background
                                   :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,night-owl-class (:foreground ,night-owl-green
                                   :inherit bold))))

   `(git-gutter-fr:deleted
     ((,night-owl-class (:foreground ,night-owl-red
                                   :inherit bold))))

   `(git-gutter-fr:modified
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,night-owl-class (:background ,night-owl-green
                                   :foreground ,night-owl-background
                                   :inherit bold))))

   `(git-gutter+-deleted
     ((,night-owl-class (:background ,night-owl-red
                                   :foreground ,night-owl-background
                                   :inherit bold))))

   `(git-gutter+-modified
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-background
                                   :inherit bold))))

   `(git-gutter+-unchanged
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-background
                                   :inherit bold))))

   `(git-gutter-fr+-added
     ((,night-owl-class (:foreground ,night-owl-green
                                   :weight bold))))

   `(git-gutter-fr+-deleted
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight bold))))

   `(git-gutter-fr+-modified
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :background ,night-owl-highlight-line
                                   :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(guide-key/key-face
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(guide-key/prefix-command-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,night-owl-class (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,night-owl-class (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,night-owl-class (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,night-owl-class (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,night-owl-class (:weight bold
                               :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,night-owl-class (:inherit message-header-other))))

   `(gnus-header-from
     ((,night-owl-class (:inherit message-header-other))))

   `(gnus-header-name
     ((,night-owl-class (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,night-owl-class (:inherit message-header-other))))

   `(gnus-header-subject
     ((,night-owl-class (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(gnus-summary-high-ancient
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :weight bold))))

   `(gnus-summary-high-read
     ((,night-owl-class (:foreground ,night-owl-green
                                   :weight bold))))

   `(gnus-summary-high-ticked
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :weight bold))))

   `(gnus-summary-high-unread
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :weight bold))))

   `(gnus-summary-low-ancient
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(gnus-summary-low-read
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(gnus-summary-low-ticked
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(gnus-summary-low-unread
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(gnus-summary-normal-ancient
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(gnus-summary-normal-read
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(gnus-summary-normal-ticked
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(gnus-summary-normal-unread
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(gnus-summary-selected
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold))))

   `(gnus-cite-1
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(gnus-cite-2
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(gnus-cite-3
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(gnus-cite-4
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(gnus-cite-5
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(gnus-cite-6
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(gnus-cite-7
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(gnus-cite-8
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(gnus-cite-9
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(gnus-cite-10
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(gnus-cite-11
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(gnus-group-news-1-empty
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(gnus-group-news-2-empty
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(gnus-group-news-3-empty
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(gnus-group-news-4-empty
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(gnus-group-news-5-empty
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(gnus-group-news-6-empty
     ((,night-owl-class (:foreground ,night-owl-blue-lc))))

   `(gnus-group-news-low-empty
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(gnus-signature
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(gnus-x-face
     ((,night-owl-class (:background ,night-owl-foreground
                                   :foreground ,night-owl-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(helm-apt-installed
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(helm-bookmark-directory
     ((,night-owl-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(helm-bookmark-gnus
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   `(helm-bookmark-info
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(helm-bookmark-man
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(helm-bookmark-w3m
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(helm-bookmarks-su
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(helm-buffer-file
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(helm-buffer-directory
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(helm-buffer-process
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(helm-buffer-saved-out
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-background
                                   :inverse-video t))))

   `(helm-buffer-size
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(helm-candidate-number
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-emphasis
                                   :bold t))))

   `(helm-ff-directory
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(helm-ff-executable
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(helm-ff-file
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-foreground))))

   `(helm-ff-invalid-symlink
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-orange
                                   :slant italic))))

   `(helm-ff-prefix
     ((,night-owl-class (:background ,night-owl-green
                                   :foreground ,night-owl-background))))

   `(helm-ff-symlink
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   `(helm-grep-file
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :underline t))))

   `(helm-grep-finish
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(helm-grep-lineno
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(helm-grep-match
     ((,night-owl-class (:inherit helm-match))))

   `(helm-grep-running
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(helm-header
     ((,night-owl-class (:inherit header-line))))

   `(helm-lisp-completion-info
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(helm-lisp-show-completion
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :background ,night-owl-highlight-line
                                   :bold t))))

   `(helm-M-x-key
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :underline t))))

   `(helm-moccur-buffer
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :underline t))))

   `(helm-match
     ((,night-owl-class (:foreground ,night-owl-green :inherit bold))))

   `(helm-match-item
     ((,night-owl-class (:inherit helm-match))))

   `(helm-selection
     ((,night-owl-class (:background ,night-owl-highlight
                                   :inherit bold
                                   :underline nil))))

   `(helm-selection-line
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-emphasis
                                   :underline nil))))

   `(helm-separator
     ((,night-owl-class (:foreground ,night-owl-gray))))

   `(helm-source-header
     ((,night-owl-class (:background ,night-owl-violet-l
                                   :foreground ,night-owl-background
                                   :underline nil))))

   `(helm-swoop-target-line-face
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(helm-swoop-target-word-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(helm-time-zone-current
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(helm-time-zone-home
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(helm-visible-mark
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,night-owl-class :foreground ,night-owl-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,night-owl-class :foreground ,night-owl-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,night-owl-class :foreground ,night-owl-blue-l)))

   `(helm-ls-git-untracked-face
     ((,night-owl-class :foreground ,night-owl-orange)))

   `(helm-ls-git-added-copied-face
     ((,night-owl-class :foreground ,night-owl-green)))

   `(helm-ls-git-added-modified-face
     ((,night-owl-class :foreground ,night-owl-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,night-owl-class :foreground ,night-owl-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,night-owl-class :foreground ,night-owl-red-l)))

   `(helm-ls-git-conflict-face
     ((,night-owl-class :foreground ,night-owl-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,night-owl-class (:foreground ,night-owl-yellow-lc
                                   :background ,night-owl-yellow-hc))))

   `(hi-pink
     ((,night-owl-class (:foreground ,night-owl-magenta-lc
                                   :background ,night-owl-magenta-hc))))

   `(hi-green
     ((,night-owl-class (:foreground ,night-owl-green-lc
                                   :background ,night-owl-green-hc))))

   `(hi-blue
     ((,night-owl-class (:foreground ,night-owl-blue-lc
                                   :background ,night-owl-blue-hc))))

   `(hi-black-b
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-background
                                   :weight bold))))

   `(hi-blue-b
     ((,night-owl-class (:foreground ,night-owl-blue-lc
                                   :weight bold))))

   `(hi-green-b
     ((,night-owl-class (:foreground ,night-owl-green-lc
                                   :weight bold))))

   `(hi-red-b
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight bold))))

   `(hi-black-hb
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-background
                                   :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(highlight-changes-delete
     ((,night-owl-class (:foreground ,night-owl-red
                                   :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,night-owl-class (:background ,night-owl-gray))))

   `(highlight-indentation-current-column-face
     ((,night-owl-class (:background ,night-owl-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face
     ((,night-owl-class (:background ,night-owl-highlight))))

   ;; hl-line-mode
   `(hl-line
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(hl-line-face
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight normal))))

   `(ido-only-match
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-yellow
                                   :weight normal))))

   `(ido-subdir
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(ido-incomplete-regexp
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight bold ))))

   `(ido-indicator
     ((,night-owl-class (:background ,night-owl-red
                                   :foreground ,night-owl-background
                                   :width condensed))))

   `(ido-virtual
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   ;; info
   `(info-header-xref
     ((,night-owl-class (:foreground ,night-owl-green
                                   :inherit bold
                                   :underline t))))

   `(info-menu
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(info-node
     ((,night-owl-class (:foreground ,night-owl-violet
                                   :inherit bold))))

   `(info-quoted-name
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(info-reference-item
     ((,night-owl-class (:background nil
                                   :underline t
                                   :inherit bold))))

   `(info-string
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(info-title-1
     ((,night-owl-class (:height ,night-owl-height-plus-4))))

   `(info-title-2
     ((,night-owl-class (:height ,night-owl-height-plus-3))))

   `(info-title-3
     ((,night-owl-class (:height ,night-owl-height-plus-2))))

   `(info-title-4
     ((,night-owl-class (:height ,night-owl-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,night-owl-class (:background ,night-owl-gray :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,night-owl-class (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,night-owl-class (:foreground ,night-owl-violet
                                   :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,night-owl-class (:foreground ,night-owl-green
                                   :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :underline t))))

   `(ivy-remote
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(swiper-line-face
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(swiper-match-face-1
     ((,night-owl-class (:background ,night-owl-gray-d))))

   `(swiper-match-face-2
     ((,night-owl-class (:background ,night-owl-green))))

   `(swiper-match-face-3
     ((,night-owl-class (:background ,night-owl-orange))))

   `(swiper-match-face-4
     ((,night-owl-class (:background ,night-owl-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,night-owl-class (:weight bold
                               :foreground ,night-owl-red))))

   `(jabber-activity-personal-face
     ((,night-owl-class (:weight bold
                               :foreground ,night-owl-blue))))

   `(jabber-chat-error
     ((,night-owl-class (:weight bold
                               :foreground ,night-owl-red))))

   `(jabber-chat-prompt-foreign
     ((,night-owl-class (:weight bold
                               :foreground ,night-owl-red))))

   `(jabber-chat-prompt-local
     ((,night-owl-class (:weight bold
                               :foreground ,night-owl-blue))))

   `(jabber-chat-prompt-system
     ((,night-owl-class (:weight bold
                               :foreground ,night-owl-green))))

   `(jabber-chat-text-foreign
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(jabber-chat-text-local
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(jabber-chat-rare-time-face
     ((,night-owl-class (:underline t
                                  :foreground ,night-owl-green))))

   `(jabber-roster-user-away
     ((,night-owl-class (:slant italic
                              :foreground ,night-owl-green))))

   `(jabber-roster-user-chatty
     ((,night-owl-class (:weight bold
                               :foreground ,night-owl-orange))))

   `(jabber-roster-user-dnd
     ((,night-owl-class (:slant italic
                              :foreground ,night-owl-red))))

   `(jabber-roster-user-error
     ((,night-owl-class (:weight light
                               :slant italic
                               :foreground ,night-owl-red))))

   `(jabber-roster-user-offline
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(jabber-roster-user-online
     ((,night-owl-class (:weight bold
                               :foreground ,night-owl-blue))))

   `(jabber-roster-user-xa
     ((,night-owl-class (:slant italic
                              :foreground ,night-owl-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(js2-external-variable
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(js2-function-call
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(js2-function-param
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(js2-instance-member
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(js2-jsdoc-html-tag-name
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(js2-jsdoc-tag
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(js2-jsdoc-type
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(js2-jsdoc-value
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(js2-magic-paren
     ((,night-owl-class (:underline t))))

   `(js2-object-property
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(js2-private-function-call
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(js2-private-member
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(js2-warning
     ((,night-owl-class (:underline ,night-owl-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,night-owl-class (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,night-owl-class (:foreground ,night-owl-line-number
                                   :background ,night-owl-fringe-bg
                                   :inherit default
                                   :underline nil))))

   ;; line-number (>= Emacs26)
   `(line-number
     ((,night-owl-class (:foreground ,night-owl-line-number
                                   :background ,night-owl-fringe-bg
                                   :inherit default
                                   :underline nil))))
   `(line-number-current-line
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :background ,night-owl-fringe-bg
                                   :inherit default
                                   :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,night-owl-class (:foreground ,night-owl-line-number
                                   :background ,night-owl-highlight-line
                                   :underline nil))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,night-owl-class (:inherit dinight-owl-red-directory))))

   `(lusty-file-face
     ((,night-owl-class nil)))

   `(lusty-match-face
     ((,night-owl-class (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,night-owl-class (:foreground ,night-owl-green
                                   :background ,night-owl-background))))

   `(magit-diff-added-highlight
     ((,night-owl-class (:foreground ,night-owl-green
                                   :background ,night-owl-highlight-line))))

   `(magit-diff-removed
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-background))))

   `(magit-diff-removed-highlight
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-highlight-line))))

   `(magit-section-title
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold))))

   `(magit-branch
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :weight bold))))

   `(magit-item-highlight
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :weight unspecified))))

   `(magit-log-author
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   `(magit-log-graph
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(magit-log-head-label-bisect-bad
     ((,night-owl-class (:background ,night-owl-red-hc
                                   :foreground ,night-owl-red-lc
                                   :box 1))))

   `(magit-log-head-label-bisect-good
     ((,night-owl-class (:background ,night-owl-green-hc
                                   :foreground ,night-owl-green-lc
                                   :box 1))))

   `(magit-log-head-label-default
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :box 1))))

   `(magit-log-head-label-local
     ((,night-owl-class (:background ,night-owl-blue-lc
                                   :foreground ,night-owl-blue-hc
                                   :box 1))))

   `(magit-log-head-label-patches
     ((,night-owl-class (:background ,night-owl-red-lc
                                   :foreground ,night-owl-red-hc
                                   :box 1))))

   `(magit-log-head-label-remote
     ((,night-owl-class (:background ,night-owl-green-lc
                                   :foreground ,night-owl-green-hc
                                   :box 1))))

   `(magit-log-head-label-tags
     ((,night-owl-class (:background ,night-owl-yellow-lc
                                   :foreground ,night-owl-yellow-hc
                                   :box 1))))

   `(magit-log-sha1
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   ;; man
   `(Man-overstrike
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :weight bold))))

   `(Man-reverse
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(Man-underline
     ((,night-owl-class (:foreground ,night-owl-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold))))

   `(monky-diff-add
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(monky-diff-del
     ((,night-owl-class (:foreground ,night-owl-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(markdown-header-face-1
     ((,night-owl-class (:inherit markdown-header-face
                                :height ,night-owl-height-plus-4))))

   `(markdown-header-face-2
     ((,night-owl-class (:inherit markdown-header-face
                                :height ,night-owl-height-plus-3))))

   `(markdown-header-face-3
     ((,night-owl-class (:inherit markdown-header-face
                                :height ,night-owl-height-plus-2))))

   `(markdown-header-face-4
     ((,night-owl-class (:inherit markdown-header-face
                                :height ,night-owl-height-plus-1))))

   `(markdown-header-face-5
     ((,night-owl-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,night-owl-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(message-header-name
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(message-header-other
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :weight normal))))

   `(message-header-to
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :weight normal))))

   `(message-header-cc
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :weight normal))))

   `(message-header-newsgroups
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold))))

   `(message-header-subject
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :weight normal))))

   `(message-header-xheader
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   `(message-mml
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold))))

   `(message-separator
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(mew-face-header-from
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(mew-face-header-date
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(mew-face-header-to
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(mew-face-header-key
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(mew-face-header-private
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(mew-face-header-important
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(mew-face-header-marginal
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :weight bold))))

   `(mew-face-header-warning
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(mew-face-header-xmew
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(mew-face-header-xmew-bad
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(mew-face-body-url
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(mew-face-body-comment
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :slant italic))))

   `(mew-face-body-cite1
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(mew-face-body-cite2
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(mew-face-body-cite3
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(mew-face-body-cite4
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(mew-face-body-cite5
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(mew-face-mark-review
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(mew-face-mark-escape
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(mew-face-mark-delete
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(mew-face-mark-unlink
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(mew-face-mark-refile
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(mew-face-mark-unread
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(mew-face-eof-message
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(mew-face-eof-part
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(mingus-pausing-face
     ((,night-owl-class (:foreground ,night-owl-magenta))))

   `(mingus-playing-face
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   `(mingus-playlist-face
     ((,night-owl-class (:foreground ,night-owl-cyan ))))

   `(mingus-song-file-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(mingus-stopped-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,night-owl-class (:background ,night-owl-violet-d))))

   `(mmm-cleanup-submode-face
     ((,night-owl-class (:background ,night-owl-orange-d))))

   `(mmm-declaration-submode-face
     ((,night-owl-class (:background ,night-owl-cyan-d))))

   `(mmm-comment-submode-face
     ((,night-owl-class (:background ,night-owl-blue-d))))

   `(mmm-output-submode-face
     ((,night-owl-class (:background ,night-owl-red-d))))

   `(mmm-special-submode-face
     ((,night-owl-class (:background ,night-owl-green-d))))

   `(mmm-code-submode-face
     ((,night-owl-class (:background ,night-owl-gray))))

   `(mmm-default-submode-face
     ((,night-owl-class (:background ,night-owl-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,night-owl-class (:underline t))))

   `(moccur-edit-done-face
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :background ,night-owl-background
                                   :slant italic))))

   `(moccur-edit-face
     ((,night-owl-class (:background ,night-owl-yellow
                                   :foreground ,night-owl-background))))

   `(moccur-edit-file-face
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(moccur-edit-reject-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(moccur-face
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-emphasis
                                   :weight bold))))

   `(search-buffers-face
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-emphasis
                                   :weight bold))))

   `(search-buffers-header-face
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-yellow
                                   :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :slant italic
                                   :weight normal))))

   `(mu4e-cited-2-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :slant italic
                                   :weight normal))))

   `(mu4e-cited-3-face
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :slant italic
                                   :weight normal))))

   `(mu4e-cited-4-face
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :slant italic
                                   :weight normal))))

   `(mu4e-cited-5-face
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :slant italic
                                   :weight normal))))

   `(mu4e-cited-6-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :slant italic
                                   :weight normal))))

   `(mu4e-cited-7-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :slant italic
                                   :weight normal))))

   `(mu4e-flagged-face
     ((,night-owl-class (:foreground ,night-owl-magenta
                                   :weight bold))))

   `(mu4e-view-url-number-face
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight normal))))

   `(mu4e-warning-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :slant normal
                                   :weight bold))))

   `(mu4e-header-highlight-face
     ((,night-owl-class (:inherit unspecified
                                :foreground unspecified
                                :background ,night-owl-highlight-line
                                :underline ,night-owl-emphasis
                                :weight normal))))


   `(mu4e-draft-face
     ((,night-owl-class (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,night-owl-class (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,night-owl-class (:inherit font-lock-builtin-face
                                :weight normal))))

   `(mu4e-header-face
     ((,night-owl-class (:inherit default))))

   `(mu4e-header-marks-face
     ((,night-owl-class (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,night-owl-class (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,night-owl-class (:inherit font-lock-pseudo-keyword-face
                                :weight bold))))

   `(mu4e-moved-face
     ((,night-owl-class (:inherit font-lock-comment-face
                                :slant italic))))

   `(mu4e-ok-face
     ((,night-owl-class (:inherit font-lock-comment-face
                                :slant normal
                                :weight bold))))

   `(mu4e-replied-face
     ((,night-owl-class (:inherit font-lock-builtin-face
                                :weight normal))))

   `(mu4e-system-face
     ((,night-owl-class (:inherit font-lock-comment-face
                                :slant italic))))

   `(mu4e-title-face
     ((,night-owl-class (:inherit font-lock-type-face
                                :weight bold))))

   `(mu4e-trashed-face
     ((,night-owl-class (:inherit font-lock-comment-face
                                :strike-through t))))

   `(mu4e-unread-face
     ((,night-owl-class (:inherit font-lock-keyword-face
                                :weight bold))))

   `(mu4e-view-attach-number-face
     ((,night-owl-class (:inherit font-lock-variable-name-face
                                :weight bold))))

   `(mu4e-view-contact-face
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :weight normal))))

   `(mu4e-view-header-key-face
     ((,night-owl-class (:inherit message-header-name
                                :weight normal))))

   `(mu4e-view-header-value-face
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :weight normal
                                   :slant normal))))

   `(mu4e-view-link-face
     ((,night-owl-class (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :weight normal
                                   :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(nav-face-button-num
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   `(nav-face-dir
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(nav-face-hdir
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(nav-face-file
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(nav-face-hfile
     ((,night-owl-class (:foreground ,night-owl-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :background ,night-owl-background
                                   :weight bold))))


   `(neo-header-face
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-background))))

   `(neo-root-dir-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :background ,night-owl-background))))

   `(neo-dir-link-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(neo-file-link-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(neo-button-face
     ((,night-owl-class (:underline nil))))

   `(neo-expand-btn-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(neo-vc-default-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(neo-vc-user-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :slant italic))))

   `(neo-vc-up-to-date-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(neo-vc-edited-face
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(neo-vc-needs-update-face
     ((,night-owl-class (:underline t))))

   `(neo-vc-needs-merge-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(neo-vc-unlocked-changes-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-comments))))

   `(neo-vc-added-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(neo-vc-removed-face
     ((,night-owl-class (:strike-through t))))

   `(neo-vc-conflict-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(neo-vc-missing-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(neo-vc-ignored-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,night-owl-class (:foreground ,night-owl-gray-l))))

   `(markup-table-face
     ((,night-owl-class (:foreground ,night-owl-blue-hc
                                   :background ,night-owl-blue-lc))))

   `(markup-verbatim-face
     ((,night-owl-class (:background ,night-owl-orange-lc))))

   `(markup-list-face
     ((,night-owl-class (:foreground ,night-owl-violet-hc
                                   :background ,night-owl-violet-lc))))

   `(markup-replacement-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(markup-complex-replacement-face
     ((,night-owl-class (:foreground ,night-owl-violet-hc
                                   :background ,night-owl-violet-lc))))

   `(markup-gen-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(markup-secondary-text-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,night-owl-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,night-owl-background)))))

   `(org-agenda-calendar-event
     ((,night-owl-class (:foreground ,night-owl-emphasis))))

   `(org-agenda-calendar-sexp
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :slant italic))))

   `(org-agenda-date
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :background ,night-owl-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,night-owl-background)))) t)

   `(org-agenda-date-weekend
     ((,night-owl-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,night-owl-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))) t)

   `(org-agenda-date-today
     ((,night-owl-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,night-owl-blue
                                :background ,night-owl-background))) t)

   `(org-agenda-done
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :slant italic))) t)

   `(org-archived
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :weight normal))))

   `(org-block
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-highlight-alt))))

   `(org-block-background
     ((,night-owl-class (:background ,night-owl-highlight-alt))))

   `(org-block-begin-line
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :background ,night-owl-gray-d
                                   :slant italic))))

   `(org-block-end-line
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :background ,night-owl-gray-d
                                   :slant italic))))

   `(org-checkbox
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-foreground
                                   :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(org-date
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :underline t))))

   `(org-done
     ((,night-owl-class (:weight bold
                               :foreground ,night-owl-green))))

   `(org-ellipsis
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(org-formula
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(org-headline-done
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(org-hide
     ((,night-owl-class (:foreground ,night-owl-background))))

   `(org-level-1
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :height ,night-owl-height-plus-4
                                :foreground ,night-owl-orange))))

   `(org-level-2
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :height ,night-owl-height-plus-3
                                :foreground ,night-owl-green))))

   `(org-level-3
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :height ,night-owl-height-plus-2
                                :foreground ,night-owl-blue))))

   `(org-level-4
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :height ,night-owl-height-plus-1
                                :foreground ,night-owl-yellow))))

   `(org-level-5
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-cyan))))

   `(org-level-6
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-green))))

   `(org-level-7
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-red))))

   `(org-level-8
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-blue))))

   `(org-link
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :underline t))))

   `(org-sexp-date
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(org-scheduled
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(org-scheduled-previously
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   `(org-scheduled-today
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :weight normal))))

   `(org-special-keyword
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :weight bold))))

   `(org-table
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(org-tag
     ((,night-owl-class (:weight bold))))

   `(org-time-grid
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(org-todo
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight bold))))

   `(org-upcoming-deadline
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight normal
                                   :underline nil))))

   `(org-warning
     ((,night-owl-class (:foreground ,night-owl-orange
                                   :weight normal
                                   :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,night-owl-class (:background ,night-owl-blue-lc
                                   :foreground ,night-owl-blue-hc))))

   `(org-habit-clear-future-face
     ((,night-owl-class (:background ,night-owl-blue-lc))))

   `(org-habit-ready-face
     ((,night-owl-class (:background ,night-owl-green-lc
                                   :foreground ,night-owl-green))))

   `(org-habit-ready-future-face
     ((,night-owl-class (:background ,night-owl-green-lc))))

   `(org-habit-alert-face
     ((,night-owl-class (:background ,night-owl-yellow
                                   :foreground ,night-owl-yellow-lc))))

   `(org-habit-alert-future-face
     ((,night-owl-class (:background ,night-owl-yellow-lc))))

   `(org-habit-overdue-face
     ((,night-owl-class (:background ,night-owl-red
                                   :foreground ,night-owl-red-lc))))

   `(org-habit-overdue-future-face
     ((,night-owl-class (:background ,night-owl-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(org-agenda-restriction-lock
     ((,night-owl-class (:background ,night-owl-yellow))))

   `(org-clock-overlay
     ((,night-owl-class (:background ,night-owl-yellow))))

   `(org-column
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))))

   `(org-column-title
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :underline t
                                   :weight bold))))

   `(org-date-selected
     ((,night-owl-class (:foreground ,night-owl-red
                                   :inverse-video t))))

   `(org-document-info
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(org-document-title
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :weight bold
                                   :height ,night-owl-height-plus-4))))

   `(org-drawer
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   `(org-footnote
     ((,night-owl-class (:foreground ,night-owl-magenta
                                   :underline t))))

   `(org-latex-and-export-specials
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(org-mode-line-clock-overrun
     ((,night-owl-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,night-owl-class (:inherit org-level-1))))

   `(outline-2
     ((,night-owl-class (:inherit org-level-2))))

   `(outline-3
     ((,night-owl-class (:inherit org-level-3))))

   `(outline-4
     ((,night-owl-class (:inherit org-level-4))))

   `(outline-5
     ((,night-owl-class (:inherit org-level-5))))

   `(outline-6
     ((,night-owl-class (:inherit org-level-6))))

   `(outline-7
     ((,night-owl-class (:inherit org-level-7))))

   `(outline-8
     ((,night-owl-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face)

   ;; perspective
   `(persp-selected-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight normal))))

   ;; popup
   `(popup-face
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-foreground))))

   `(popup-isearch-match
     ((,night-owl-class (:background ,night-owl-green))))

   `(popup-menu-face
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-foreground))))

   `(popup-menu-mouse-face
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-foreground))))

   `(popup-menu-selection-face
     ((,night-owl-class (:background ,night-owl-magenta
                                   :foreground ,night-owl-background))))

   `(popup-scroll-bar-background-face
     ((,night-owl-class (:background ,night-owl-comments))))

   `(popup-scroll-bar-foreground-face
     ((,night-owl-class (:background ,night-owl-emphasis))))

   `(popup-tip-face
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(rainbow-delimiters-depth-4-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(rainbow-delimiters-depth-7-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(rainbow-delimiters-depth-10-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(rainbow-delimiters-unmatched-face
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :background ,night-owl-background
                                   :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,night-owl-class (:foreground ,night-owl-green-d))))

   `(realgud-overlay-arrow2
     ((,night-owl-class (:foreground ,night-owl-yellow-d))))

   `(realgud-overlay-arrow3
     ((,night-owl-class (:foreground ,night-owl-orange-d))))

   `(realgud-bp-enabled-face
     ((,night-owl-class (:inherit error))))

   `(realgud-bp-disabled-face
     ((,night-owl-class (:inherit secondary-selection))))

   `(realgud-bp-line-enabled-face
     ((,night-owl-class (:foreground ,night-owl-red-d))))

   `(realgud-bp-line-disabled-face
     ((,night-owl-class (:inherit secondary-selection))))

   `(realgud-line-number
     ((,night-owl-class (:inerhit night-owl-line-number))))

   `(realgud-backtrace-number
     ((,night-owl-class (:foreground ,night-owl-yellow-d
                                   :weight bold))))

   ;; rhtm-mode
   `(erb-face
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-background))))

   `(erb-delim-face
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :background ,night-owl-background))))

   `(erb-exec-face
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-background))))

   `(erb-exec-delim-face
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :background ,night-owl-background))))

   `(erb-out-face
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-background))))

   `(erb-out-delim-face
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :background ,night-owl-background))))

   `(erb-comment-face
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-background))))

   `(erb-comment-delim-face
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :background ,night-owl-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,night-owl-class (:background ,night-owl-yellow
                                   :foreground ,night-owl-background))))

   `(rst-level-2-face
     ((,night-owl-class (:background ,night-owl-cyan
                                   :foreground ,night-owl-background))))

   `(rst-level-3-face
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-background))))

   `(rst-level-4-face
     ((,night-owl-class (:background ,night-owl-violet
                                   :foreground ,night-owl-background))))

   `(rst-level-5-face
     ((,night-owl-class (:background ,night-owl-magenta
                                   :foreground ,night-owl-background))))

   `(rst-level-6-face
     ((,night-owl-class (:background ,night-owl-red
                                   :foreground ,night-owl-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(rpm-spec-doc-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(rpm-spec-ghost-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(rpm-spec-macro-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(rpm-spec-package-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(rpm-spec-section-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(rpm-spec-tag-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(rpm-spec-var-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,night-owl-class (:foreground ,night-owl-violet
                                   :weight bold))))

   `(sh-escaped-newline
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold))))

   `(sh-heredoc
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(sp-wrap-overlay-face
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(sp-show-pair-enclosing
     ((,night-owl-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :background ,night-owl-background
                                   :weight normal
                                   :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-background
                                   :weight normal
                                   :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,night-owl-class (:foreground ,night-owl-green
                                   :background ,night-owl-background
                                   :weight normal
                                   :inverse-video t))))

   `(show-paren-mismatch
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-background
                                   :weight normal
                                   :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,night-owl-class (:foreground ,night-owl-green
                                   :background ,night-owl-background
                                   :weight normal
                                   :inverse-video t))))

   `(paren-face-mismatch
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-background
                                   :weight normal
                                   :inverse-video t))))

   `(paren-face-no-match
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-background
                                   :weight normal
                                   :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-comments))))

   `(speedbar-directory-face
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-blue))))

   `(speedbar-file-face
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-foreground))))

   `(speedbar-highlight-face
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :background ,night-owl-highlight-line))))

   `(speedbar-selected-face
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-yellow
                                :underline t))))

   `(speedbar-separator-face
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :background ,night-owl-blue
                                :foreground ,night-owl-background
                                :overline ,night-owl-cyan-lc))))

   `(speedbar-tag-face
     ((,night-owl-class (:inherit ,night-owl-pitch
                                :foreground ,night-owl-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,night-owl-class (:background ,night-owl-blue
                                   :foreground ,night-owl-background
                                   :height ,night-owl-height-plus-1
                                   :weight bold))))

   `(sr-editing-path-face
     ((,night-owl-class (:background ,night-owl-yellow
                                   :foreground ,night-owl-background
                                   :weight bold
                                   :height ,night-owl-height-plus-1))))

   `(sr-highlight-path-face
     ((,night-owl-class (:background ,night-owl-green
                                   :foreground ,night-owl-background
                                   :weight bold
                                   :height ,night-owl-height-plus-1))))

   `(sr-passive-path-face
     ((,night-owl-class (:background ,night-owl-comments
                                   :foreground ,night-owl-background
                                   :weight bold
                                   :height ,night-owl-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,night-owl-class (:inherit dinight-owl-red-marked))))

   `(sr-marked-file-face
     ((,night-owl-class (:inherit dinight-owl-red-marked))))

   `(sr-alt-marked-dir-face
     ((,night-owl-class (:background ,night-owl-magenta
                                   :foreground ,night-owl-background
                                   :weight bold))))

   `(sr-alt-marked-file-face
     ((,night-owl-class (:background ,night-owl-magenta
                                   :foreground ,night-owl-background
                                   :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,night-owl-class (:inherit dinight-owl-red-directory
                                :weight normal))))

   `(sr-symlink-directory-face
     ((,night-owl-class (:inherit dinight-owl-red-directory
                                :slant italic
                                :weight normal))))

   `(sr-symlink-face
     ((,night-owl-class (:inherit dinight-owl-red-symlink
                                :slant italic
                                :weight normal))))

   `(sr-broken-link-face
     ((,night-owl-class (:inherit dinight-owl-red-warning
                                :slant italic
                                :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(sr-encrypted-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(sr-log-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(sr-packaged-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(sr-html-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(sr-xml-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,night-owl-class (:background ,night-owl-red
                                   :foreground ,night-owl-background
                                   :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-yellow))))

   `(syslog-hour-face
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-green))))

   `(syslog-error-face
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-red
                                   :weight bold))))

   `(syslog-warn-face
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-orange
                                   :weight bold))))

   `(syslog-info-face
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-blue
                                   :weight bold))))

   `(syslog-debug-face
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-cyan
                                   :weight bold))))

   `(syslog-su-face
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-magenta))))

   ;; table
   `(table-cell
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :background ,night-owl-highlight-line))))

   ;; term
   `(term-color-black
     ((,night-owl-class (:foreground ,night-owl-background
                                   :background ,night-owl-highlight-line))))

   `(term-color-red
     ((,night-owl-class (:foreground ,night-owl-red
                                   :background ,night-owl-red-d))))

   `(term-color-green
     ((,night-owl-class (:foreground ,night-owl-green
                                   :background ,night-owl-green-d))))

   `(term-color-yellow
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :background ,night-owl-yellow-d))))

   `(term-color-blue
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :background ,night-owl-blue-d))))

   `(term-color-magenta
     ((,night-owl-class (:foreground ,night-owl-magenta
                                   :background ,night-owl-magenta-d))))

   `(term-color-cyan
     ((,night-owl-class (:foreground ,night-owl-cyan
                                   :background ,night-owl-cyan-d))))

   `(term-color-white
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-foreground))))

   `(term-default-fg-color
     ((,night-owl-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,night-owl-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,night-owl-class (:background ,night-owl-yellow-hc
                                   :foreground ,night-owl-background
                                   :inherit ,night-owl-pitch))))

   ;; treemacs
   `(treemacs-directory-face
      ((,night-owl-class (:foreground ,night-owl-violet
                         :background ,night-owl-background
                         :weight bold))))

   `(treemacs-header-face
      ((,night-owl-class (:foreground ,night-owl-yellow
                         :background ,night-owl-background
                         :underline t
                         :weight bold))))

   `(treemacs-git-modified-face
      ((,night-owl-class (:foreground ,night-owl-green
                         :background ,night-owl-background))))

   `(treemacs-git-renamed-face
      ((,night-owl-class (:foreground ,night-owl-red
                         :background ,night-owl-background))))

   `(treemacs-git-ignored-face
      ((,night-owl-class (:foreground ,night-owl-gray-l
                         :background ,night-owl-background))))

   `(treemacs-git-untracked-face
      ((,night-owl-class (:foreground ,night-owl-red
                         :background ,night-owl-background))))

   `(treemacs-git-added-face
      ((,night-owl-class (:foreground ,night-owl-green
                         :background ,night-owl-background))))

   `(treemacs-git-conflict-face
      ((,night-owl-class (:foreground ,night-owl-orange
                         :background ,night-owl-background))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,night-owl-class (:foreground ,night-owl-magenta
                                   :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :background ,night-owl-highlight-line
                                   :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,night-owl-class (:foreground ,night-owl-emphasis))))

   `(tuareg-font-lock-error-face
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :background ,night-owl-red
                                   :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,night-owl-class (:foreground ,night-owl-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :background ,night-owl-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(undo-tree-visualizer-current-face
     ((,night-owl-class (:foreground ,night-owl-blue
                                   :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :background ,night-owl-background
                                   :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   ;; volatile highlights
   `(vhl/default-face
      ((,night-owl-class (:background ,night-owl-highlight-alt))))

   ;; w3m
   `(w3m-anchor
     ((,night-owl-class (:inherit link))))

   `(w3m-arrived-anchor
     ((,night-owl-class (:inherit link-visited))))

   `(w3m-form
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-foreground))))

   `(w3m-header-line-location-title
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-yellow))))

   `(w3m-header-line-location-content

     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-foreground))))

   `(w3m-bold
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :weight bold))))

   `(w3m-image-anchor
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-cyan
                                   :inherit link))))

   `(w3m-image
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,night-owl-class (:foreground ,night-owl-emphasis))))

   `(w3m-lnum-match
     ((,night-owl-class (:background ,night-owl-highlight-line))))

   `(w3m-lnum
     ((,night-owl-class (:underline nil
                                  :bold nil
                                  :foreground ,night-owl-red))))

   `(w3m-session-select
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(w3m-session-selected
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :bold t
                                   :underline t))))

   `(w3m-tab-background
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-foreground))))

   `(w3m-tab-selected-background
     ((,night-owl-class (:background ,night-owl-background
                                   :foreground ,night-owl-foreground))))

   `(w3m-tab-mouse
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-yellow))))

   `(w3m-tab-selected
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-emphasis
                                   :bold t))))

   `(w3m-tab-unselected
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-foreground))))

   `(w3m-tab-selected-retrieving
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-red))))

   `(w3m-tab-unselected-retrieving
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-orange))))

   `(w3m-tab-unselected-unseen
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :foreground ,night-owl-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(web-mode-comment-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(web-mode-constant-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(web-mode-current-element-highlight-face
     ((,night-owl-class (:underline unspecified
                                  :weight unspecified
                                  :background ,night-owl-highlight-line))))

   `(web-mode-doctype-face
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :slant italic
                                   :weight bold))))

   `(web-mode-folded-face
     ((,night-owl-class (:underline t))))

   `(web-mode-function-name-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(web-mode-html-attr-name-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(web-mode-html-attr-custom-face
     ((,night-owl-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,night-owl-class (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,night-owl-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(web-mode-html-tag-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(web-mode-keyword-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(web-mode-preprocessor-face
     ((,night-owl-class (:foreground ,night-owl-yellow
                                   :slant normal
                                   :weight unspecified))))

   `(web-mode-string-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(web-mode-type-face
     ((,night-owl-class (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(web-mode-warning-face
     ((,night-owl-class (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,night-owl-class (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,night-owl-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,night-owl-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,night-owl-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,night-owl-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,night-owl-class (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,night-owl-class (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,night-owl-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,night-owl-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,night-owl-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,night-owl-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,night-owl-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,night-owl-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,night-owl-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,night-owl-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,night-owl-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,night-owl-class (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,night-owl-class (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(web-mode-json-key-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(web-mode-json-string-face
     ((,night-owl-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(web-mode-part-comment-face
     ((,night-owl-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,night-owl-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,night-owl-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,night-owl-class (:foreground ,night-owl-violet))))

   `(web-mode-whitespace-face
     ((,night-owl-class (:background ,night-owl-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-comments
                                   :inverse-video unspecified
                                   :slant italic))))

   `(whitespace-hspace
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-emphasis
                                   :inverse-video unspecified))))

   `(whitespace-tab
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-red
                                   :inverse-video unspecified
                                   :weight bold))))

   `(whitespace-newline
     ((,night-owl-class(:background unspecified
                                  :foreground ,night-owl-comments
                                  :inverse-video unspecified))))

   `(whitespace-trailing
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-orange-lc
                                   :inverse-video t))))

   `(whitespace-line
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-magenta
                                   :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,night-owl-class (:background ,night-owl-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))))

   `(whitespace-indentation
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-yellow
                                   :inverse-video unspecified
                                   :weight bold))))

   `(whitespace-empty
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-red-lc
                                   :inverse-video t))))

   `(whitespace-space-after-tab
     ((,night-owl-class (:background unspecified
                                   :foreground ,night-owl-orange
                                   :inverse-video t
                                   :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(wl-highlight-folder-many-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(wl-highlight-folder-path-face
     ((,night-owl-class (:foreground ,night-owl-orange))))

   `(wl-highlight-folder-unread-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(wl-highlight-folder-zero-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(wl-highlight-message-citation-header
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(wl-highlight-message-cited-text-1
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(wl-highlight-message-cited-text-2
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(wl-highlight-message-cited-text-3
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(wl-highlight-message-cited-text-4
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(wl-highlight-message-header-contents-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(wl-highlight-message-headers-face
     ((,night-owl-class (:foreground ,night-owl-red))))

   `(wl-highlight-message-important-header-contents
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(wl-highlight-message-header-contents
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(wl-highlight-message-important-header-contents2
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(wl-highlight-message-signature
     ((,night-owl-class (:foreground ,night-owl-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(wl-highlight-summary-answenight-owl-red-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(wl-highlight-summary-disposed-face
     ((,night-owl-class (:foreground ,night-owl-foreground
                                   :slant italic))))

   `(wl-highlight-summary-new-face
     ((,night-owl-class (:foreground ,night-owl-blue))))

   `(wl-highlight-summary-normal-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,night-owl-class (:foreground ,night-owl-yellow))))

   `(wl-highlight-thread-indent-face
     ((,night-owl-class (:foreground ,night-owl-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,night-owl-class (:underline t
                                  :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,night-owl-class (:inherit error))))

   `(weechat-highlight-face
     ((,night-owl-class (:foreground ,night-owl-emphasis
                                   :weight bold))))

   `(weechat-nick-self-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :weight unspecified
                                   :inverse-video t))))

   `(weechat-prompt-face
     ((,night-owl-class (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   ;; which-func-mode
   `(which-func
     ((,night-owl-class (:foreground ,night-owl-green))))

   ;; which-key
   `(which-key-key-face
     ((,night-owl-class (:foreground ,night-owl-green
                                   :weight bold))))

   `(which-key-separator-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(which-key-note-face
     ((,night-owl-class (:foreground ,night-owl-comments))))

   `(which-key-command-description-face
     ((,night-owl-class (:foreground ,night-owl-foreground))))

   `(which-key-local-map-description-face
     ((,night-owl-class (:foreground ,night-owl-yellow-hc))))

   `(which-key-group-description-face
     ((,night-owl-class (:foreground ,night-owl-red
                                   :weight bold))))
   ;; window-number-mode
   `(window-number-face
     ((,night-owl-class (:foreground ,night-owl-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :background ,night-owl-comments))))

   `(yascroll:thumb-fringe
     ((,night-owl-class (:foreground ,night-owl-comments
                                   :background ,night-owl-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,night-owl-class (:background ,night-owl-highlight-line
                                   :box ,night-owl-emphasis)))))

  (custom-theme-set-variables
   'night-owl
   `(ansi-color-names-vector [,night-owl-background ,night-owl-red ,night-owl-green ,night-owl-yellow
                                                  ,night-owl-blue ,night-owl-magenta ,night-owl-cyan ,night-owl-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,night-owl-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,night-owl-magenta ,night-owl-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,night-owl-highlight-line . 0)
       (,night-owl-green-lc . 20)
       (,night-owl-cyan-lc . 30)
       (,night-owl-blue-lc . 50)
       (,night-owl-yellow-lc . 60)
       (,night-owl-orange-lc . 70)
       (,night-owl-magenta-lc . 85)
       (,night-owl-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,night-owl-background)
   `(pos-tip-background-color ,night-owl-yellow-hc)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,night-owl-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,night-owl-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,night-owl-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,night-owl-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,night-owl-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     '(unspecified ,night-owl-background ,night-owl-highlight-line
                  ,night-owl-red-d ,night-owl-red
                  ,night-owl-green-d ,night-owl-green
                  ,night-owl-yellow-d ,night-owl-yellow
                  ,night-owl-blue-d ,night-owl-blue
                  ,night-owl-magenta-d ,night-owl-magenta
                  ,night-owl-cyan-d ,night-owl-cyan
                  ,night-owl-foreground ,night-owl-emphasis))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(add-to-list 'custom-theme-load-path "/Users/aaronjensen/Source/night-owl-theme/")
(provide-theme 'night-owl)
;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; night-owl-theme.el ends here
