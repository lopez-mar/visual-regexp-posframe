# visual-regexp-posframe

Display [visual-regexp](https://github.com/benma/visual-regexp.el) prompts in a posframe, positioned near your cursor instead of the bottom minibuffer

## Features

- 🎯 **Cursor-proximate input** - Posframe appears near your cursor in the target buffer
- 🔄 **Two-phase support** - Works for both regexp input and replacement string prompts
- ✨ **Live visual feedback** - All visual-regexp highlighting still works in the buffer
- 🎨 **Customizable positioning** - Choose from many posframe position handlers
- 🪟 **Frame-aware** - Properly handles child frames and multiple frame setups

## Why?

When using `visual-regexp` for search and replace with live preview, the default minibuffer at the bottom of the screen can be far from where you’re actually working. This package brings the input prompt right to your point of focus, making the workflow more ergonomic and keeping your eyes on the relevant part of the buffer.

## Installation

### Using straight.el

```elisp
(use-package visual-regexp-posframe
  :straight (:host github :repo "YOUR-USERNAME/visual-regexp-posframe")
  :after visual-regex
  :custom
  (vr-posframe-poshandler . #'posframe-poshandler-frame-bottom-center)  
  :config
  (vr-posframe-mode 1))
```

### Manual Installation

1. Clone this repository:

```bash
git clone https://github.com/lopez-mar/visual-regexp-posframe.git
```

1. Add to your load path and require:

```elisp
(add-to-list 'load-path "/path/to/visual-regexp-posframe")
(require 'visual-regexp-posframe)
(vr-posframe-mode 1)
```

## Requirements

- Emacs 26.1 or later
- [posframe](https://github.com/tumashu/posframe) 1.4.0+
- [visual-regexp](https://github.com/benma/visual-regexp.el) 1.1+

## Usage

Once `vr-posframe-mode` is enabled, simply use visual-regexp commands as normal:

- `M-x vr/replace` - Replace with live visual feedback
- `M-x vr/query-replace` - Query replace with live feedback
- `M-x vr/mc-mark` - Mark matches with multiple-cursors

The prompts will appear in a posframe near your cursor instead of the minibuffer.

## Configuration

### Positioning

Change where the posframe appears using `vr-posframe-poshandler`:

```elisp
;; Default: below and to the left of cursor
(setq vr-posframe-poshandler #'posframe-poshandler-point-bottom-left-corner)

;; Center of screen
(setq vr-posframe-poshandler #'posframe-poshandler-frame-center)

;; Top center of screen
(setq vr-posframe-poshandler #'posframe-poshandler-frame-top-center)

;; Above the cursor
(setq vr-posframe-poshandler #'posframe-poshandler-point-top-left-corner)

;; Window center right
(setq vr-posframe-poshandler #'posframe-poshandler-window-center-right)
```

See [posframe documentation](https://github.com/tumashu/posframe#poshandler) for all available position handlers.

### Appearance

Customize the look of the posframe:

```elisp
;; Border width
(setq vr-posframe-border-width 2)

;; Internal padding
(setq vr-posframe-internal-border-width 8)

;; Minimum dimensions
(setq vr-posframe-min-width 60
      vr-posframe-min-height 3)

;; Border color
(set-face-attribute 'vr-posframe-border nil 
                    :background "#ff0000")

;; Additional frame parameters
(setq vr-posframe-parameters 
      '((alpha . 95)
        (left-fringe . 8)
        (right-fringe . 8)))
```

### Custom Position Handler

For total control, write your own position handler:

```elisp
(defun my-vr-posframe-poshandler (info)
  "Position vr-posframe at 20% from left, 80% from top."
  (let* ((parent-frame-width (plist-get info :parent-frame-width))
         (parent-frame-height (plist-get info :parent-frame-height)))
    (cons (round (* parent-frame-width 0.2))
          (round (* parent-frame-height 0.8)))))

(setq vr-posframe-poshandler #'my-vr-posframe-poshandler)
```

## How It Works

This package uses the same approach as [vertico-posframe](https://github.com/tumashu/vertico-posframe):

1. The actual minibuffer window is shrunk to 1px and scrolled out of view
1. The minibuffer buffer is displayed in a posframe at your chosen location
1. You’re editing the real minibuffer, just displayed elsewhere
1. All hooks, keybindings, and visual-regexp features work normally

## License

GPL-3.0-or-later

## Acknowledgments

- [visual-regexp](https://github.com/benma/visual-regexp.el) by Marko Bencun - The excellent package this extends
- [posframe](https://github.com/tumashu/posframe) by Feng Shu - The posframe implementation
- [vertico-posframe](https://github.com/tumashu/vertico-posframe) - Inspiration for the implementation approach

## Contributing

Issues and pull requests welcome! This is a simple package, but improvements are always appreciated.
