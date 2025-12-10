;;; visual-regexp-posframe.el --- Use posframe to show visual-regexp -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Mar <your-email@example.com>
;; URL: https://github.com/YOUR-USERNAME/visual-regexp-posframe
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (posframe "1.4.0") (visual-regexp "1.1"))
;; Keywords: matching, convenience

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package displays visual-regexp minibuffer prompts in a posframe
;; positioned near point in the target buffer, rather than in the
;; minibuffer at the bottom of the screen.
;;
;; Usage:
;;
;;   (require 'visual-regexp-posframe)
;;   (vr-posframe-mode 1)
;;
;; The posframe will appear near your cursor when using vr/replace,
;; vr/query-replace, or vr/mc-mark.

;;; Code:

(require 'posframe)
(require 'visual-regexp)

(defgroup visual-regexp-posframe nil
  "Display visual-regexp in posframe."
  :group 'visual-regexp
  :prefix "vr-posframe-")

(defcustom vr-posframe-poshandler #'posframe-poshandler-point-bottom-left-corner
  "Posframe position handler for visual-regexp.
See `posframe-poshandler-functions' for available handlers."
  :type 'function
  :group 'visual-regexp-posframe)

(defcustom vr-posframe-border-width 2
  "Border width for visual-regexp posframe."
  :type 'integer
  :group 'visual-regexp-posframe)

(defcustom vr-posframe-internal-border-width 8
  "Internal border width (padding) for visual-regexp posframe."
  :type 'integer
  :group 'visual-regexp-posframe)

(defcustom vr-posframe-min-width 60
  "Minimum width for visual-regexp posframe."
  :type 'integer
  :group 'visual-regexp-posframe)

(defcustom vr-posframe-min-height 3
  "Minimum height for visual-regexp posframe."
  :type 'integer
  :group 'visual-regexp-posframe)

(defcustom vr-posframe-parameters nil
  "Additional frame parameters for visual-regexp posframe.
These will be merged with the default parameters."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'visual-regexp-posframe)

(defface vr-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face for visual-regexp posframe border."
  :group 'visual-regexp-posframe)

(defvar vr-posframe--buffer nil
  "The minibuffer buffer currently shown in posframe.")

(defvar vr-posframe--target-window nil
  "The window where visual-regexp was invoked.")

(defun vr-posframe--get-target-position ()
  "Get the position where posframe should be displayed.
Returns the point in the target buffer where visual-regexp was invoked."
  (when (and vr--target-buffer
             (get-buffer-window vr--target-buffer))
    (with-current-buffer vr--target-buffer
      ;; Use vr--target-buffer-start as the reference point
      (or vr--target-buffer-start (point)))))

(defun vr-posframe--show ()
  "Show the minibuffer buffer in a posframe."
  (when (and (minibufferp)
             vr--in-minibuffer
             (posframe-workable-p))
    (let ((buffer (current-buffer))
          (point (point))
          (target-window (get-buffer-window vr--target-buffer)))
      
      (setq vr-posframe--buffer buffer)
      (setq vr-posframe--target-window target-window)
      
      ;; Hide the actual minibuffer window
      (let ((minibuffer-window (active-minibuffer-window)))
        (setq-local max-mini-window-height 1)
        (window-resize minibuffer-window
                       (- (window-pixel-height minibuffer-window))
                       nil nil 'pixelwise)
        ;; Scroll minibuffer content out of view
        (set-window-vscroll minibuffer-window 100))
      
      ;; Show minibuffer buffer in posframe
      (when (and target-window (window-live-p target-window))
        (with-selected-window target-window
          (let ((pos (vr-posframe--get-target-position)))
            (when pos
              (posframe-show
               buffer
               :position pos
               :poshandler vr-posframe-poshandler
               :border-width vr-posframe-border-width
               :border-color (face-attribute 'vr-posframe-border :background nil t)
               :internal-border-width vr-posframe-internal-border-width
               :internal-border-color (face-attribute 'default :background nil t)
               :min-width vr-posframe-min-width
               :min-height vr-posframe-min-height
               :width nil  ;; Let it size automatically
               :height nil
               :window-point point
               :cursor (if (eq cursor-type t) 'box cursor-type)
               :tty-non-selected-cursor t
               :lines-truncate nil  ;; Allow wrapping for long patterns
               :override-parameters vr-posframe-parameters
               :hidehandler #'vr-posframe--hidehandler
               :refresh t))))))))

(defun vr-posframe--hidehandler (_info)
  "Determine if posframe should be hidden.
Posframe should be hidden when not in a minibuffer."
  (not (minibufferp)))

(defun vr-posframe--minibuffer-setup ()
  "Setup hook to show posframe when visual-regexp starts."
  ;; Only activate for visual-regexp minibuffers
  (when (and vr--in-minibuffer
             (posframe-workable-p))
    ;; Initial display
    (vr-posframe--show)
    ;; Update posframe as user types and moves cursor
    (add-hook 'post-command-hook #'vr-posframe--show nil 'local)))

(defun vr-posframe--minibuffer-exit ()
  "Cleanup when exiting minibuffer."
  (when vr-posframe--buffer
    ;; Restore minibuffer window visibility
    (when-let ((minibuf-window (active-minibuffer-window)))
      (set-window-vscroll minibuf-window 0))
    ;; Hide the posframe
    (posframe-hide vr-posframe--buffer)
    (setq vr-posframe--buffer nil
          vr-posframe--target-window nil)))

(defun vr-posframe--advice-target-window (orig-fun &rest args)
  "Advice for `vr--target-window' to search all frames.
This ensures visual-regexp can find the target window even when
the minibuffer is in a child frame or posframe."
  (if vr-posframe-mode
      (when vr--target-buffer
        (get-buffer-window vr--target-buffer t))
    (apply orig-fun args)))

;;;###autoload
(define-minor-mode vr-posframe-mode
  "Display visual-regexp prompts in a posframe.

When enabled, visual-regexp (vr/replace, vr/query-replace, vr/mc-mark)
will display its prompts in a posframe positioned near point in the
target buffer, rather than in the minibuffer at the bottom of the screen.

The visual feedback (highlighted matches and replacements) will still
appear in the target buffer as normal."
  :global t
  :group 'visual-regexp-posframe
  (if vr-posframe-mode
      (progn
        ;; Add hooks to show/hide posframe
        (add-hook 'minibuffer-setup-hook #'vr-posframe--minibuffer-setup)
        (add-hook 'minibuffer-exit-hook #'vr-posframe--minibuffer-exit)
        ;; Fix vr--target-window to search all frames
        (advice-add 'vr--target-window :around #'vr-posframe--advice-target-window))
    ;; Cleanup when disabled
    (remove-hook 'minibuffer-setup-hook #'vr-posframe--minibuffer-setup)
    (remove-hook 'minibuffer-exit-hook #'vr-posframe--minibuffer-exit)
    (advice-remove 'vr--target-window #'vr-posframe--advice-target-window)
    ;; Hide any active posframe
    (when vr-posframe--buffer
      (posframe-hide vr-posframe--buffer))))

;;;###autoload
(defun vr-posframe-cleanup ()
  "Clean up any visual-regexp posframes.
Useful if posframes get stuck visible."
  (interactive)
  (when vr-posframe--buffer
    (posframe-hide vr-posframe--buffer)
    (setq vr-posframe--buffer nil)))

(provide 'visual-regexp-posframe)
;;; visual-regexp-posframe.el ends here
