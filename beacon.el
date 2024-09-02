;;; beacon.el --- Highlight the cursor whenever the window scrolls  -*- lexical-binding: t; -*-
;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; URL: https://github.com/Malabarba/beacon
;; Keywords: convenience
;; Version: 1.3.4
;; Package-Requires: ((emacs "25.1"))

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

;; This is a global minor-mode.  Turn it on everywhere with:
;; ┌────
;; │ (beacon-mode 1)
;; └────
;;
;; Whenever the window scrolls a light will shine on top of your cursor so
;; you know where it is.
;;
;; That’s it.
;;
;; See the accompanying Readme.org for configuration details.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'faces)

(defgroup beacon nil
  "Customization group for beacon."
  :group 'emacs
  :prefix "beacon-")

(defvar beacon--timer nil)
(defvar beacon-mark-ring nil)
(defvar beacon-mark-ring-max 48
  "Maximum size of overall mark ring.  Start discarding off end if gets this big.")

(defvar beacon-mark-ring-traversal-position 0
  "Stores the traversal location within the beacon-mark-ring.")

(defvar beacon-last-mark-before-jump nil)

(defvar beacon-in-progress nil
  "Suppresses generation of marks in beacon-ring.
Dynamically bound to during the navigation process.")

(defcustom beacon-push-mark-p-or-threshold 3
  "Should the mark be pushed before long movements?
If nil, `beacon' will not push the mark.
Otherwise this should be a number, and `beacon' will push the
mark whenever point moves more than that many lines."
  :type '(choice integer (const nil)))

(defcustom beacon-blink-duration 0.3
  "Time, in seconds, that the blink should last."
  :type 'number)

(defcustom beacon-blink-delay 0.3
  "Time, in seconds, before starting to fade the beacon."
  :type 'number)

(defcustom beacon-size 40
  "Size of the beacon in characters."
  :type 'number)

(defcustom beacon-color 0.5
  "Color of the beacon.
This can be a string or a number.

If it is a number, the color is taken to be white or
black (depending on the current theme's background) and this
number is a float between 0 and 1 specifing the brightness.

If it is a string, it is a color name or specification,
e.g. \"#666600\"."
  :type '(choice number color))

(defface beacon-fallback-background
  '((((class color) (background light)) (:background "black"))
    (((class color) (background dark)) (:background "white")))
  "Fallback beacon background color.
Used in cases where the color can't be determined by Emacs.
Only the background of this face is used.")

(defvar beacon-dont-blink-predicates nil
  "A list of predicates that prevent the beacon blink.
These predicate functions are called in order, with no
arguments, before blinking the beacon.  If any returns
non-nil, the beacon will not blink.

For instance, if you want to disable beacon on buffers where
`hl-line-mode' is on, you can do:

    (add-hook \\='beacon-dont-blink-predicates
              (lambda () (bound-and-true-p hl-line-mode)))")

(defun beacon--compilation-mode-p ()
  "Non-nil if this is some form of compilation mode."
  (or (derived-mode-p 'compilation-mode)
      (bound-and-true-p compilation-minor-mode)))

(add-hook 'beacon-dont-blink-predicates #'window-minibuffer-p)
(add-hook 'beacon-dont-blink-predicates #'beacon--compilation-mode-p)

(defcustom beacon-dont-blink-major-modes '(t magit-status-mode magit-popup-mode
                                             inf-ruby-mode
                                             mu4e-headers-mode
                                             gnus-summary-mode gnus-group-mode)
  "A list of major-modes where the beacon won't blink.
Whenever the current buffer satisfies `derived-mode-p' for
one of the major-modes on this list, the beacon will not
blink."
  :type '(repeat symbol))

(defcustom beacon-dont-blink-commands '(next-line previous-line forward-line)
  "A list of commands that should not make the beacon blink.
Use this for commands that scroll the window in very
predictable ways, when the blink would be more distracting
than helpful.."
  :type '(repeat symbol))

(defcustom beacon-before-blink-hook nil
  "Hook run immediately before blinking the beacon."
  :type 'hook)


;;; Internal variables
(defvar beacon--window-scrolled nil)
(defvar beacon--pre-command-point-marker nil)
(defvar beacon--pre-command-mark-ring-head nil)
(defvar beacon--pre-command-window nil)
(defvar beacon--pre-command-window-start 0)

(defun beacon--record-vars ()
  "Record some variables for interal use."
  (unless (window-minibuffer-p)
    (setq beacon--pre-command-mark-ring-head (car mark-ring))
    (setq beacon--pre-command-point-marker (point-marker))
    (setq beacon--pre-command-window (selected-window))
    (setq beacon--pre-command-window-start (window-start))))


;;; Overlays
(defvar beacon--ovs nil)

(defconst beacon-overlay-priority (/ most-positive-fixnum 2)
  "Priotiy used on all of our overlays.")

(defun beacon--make-overlay (length &rest properties)
  "Put an overlay at point over LENGTH columns.

Specify background color in PROPERTIES."
  (let ((ov (make-overlay (point) (+ length (point)))))
    (overlay-put ov 'beacon t)
    ;; Our overlay is very temporary, so we take the liberty of giving
    ;; it a high priority.
    (overlay-put ov 'priority beacon-overlay-priority)
    (overlay-put ov 'window (selected-window))
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    (push ov beacon--ovs)
    ov))

(defun beacon--colored-overlay (color)
  "Put an overlay at point with background COLOR."
  (beacon--make-overlay 1 'face (list :background color)))

(defun beacon--ov-put-after-string (overlay colors)
  "Add an after-string property to OVERLAY.
The property's value is a string of spaces with background
COLORS applied to each one.
If COLORS is nil, OVERLAY is deleted!"
  (if (not colors)
      (when (overlayp overlay)
        (delete-overlay overlay))
    (overlay-put overlay 'beacon-colors colors)
    (overlay-put overlay 'after-string
                 (propertize
                  (mapconcat (lambda (c) (propertize " " 'face (list :background c)))
                             colors
                             "")
                  'cursor 1000))))

(defun beacon--visual-current-column ()
  "Get the visual column we are at.

Take long lines and visual line mode into account."
  (save-excursion
    (let ((current (point)))
      (beginning-of-visual-line)
      (- current (point)))))

(defun beacon--after-string-overlay (colors)
  "Put an overlay at point with an after-string property.
The property's value is a string of spaces with background
COLORS applied to each one."
  ;; The after-string must not be longer than the remaining columns
  ;; from point to right window-end else it will be wrapped around.
  (let ((colors (seq-take colors (- (window-width) (beacon--visual-current-column) 1))))
    (beacon--ov-put-after-string (beacon--make-overlay 0) colors)))

(defun beacon--ov-at-point ()
  "Return beacon overlay at current point."
  (car (or (seq-filter (lambda (o) (overlay-get o 'beacon))
                       (overlays-in (point) (point)))
           (seq-filter (lambda (o) (overlay-get o 'beacon))
                       (overlays-at (point))))))

(defun beacon--vanish (&rest _)
  "Turn off the beacon."
  (when (get-buffer-window)
    (mapc #'delete-overlay beacon--ovs)
    (setq beacon--ovs nil)))


;;; Colors
(defun beacon--int-range (a b)
  "Return a list of integers between A inclusive and B exclusive.
Only returns `beacon-size' elements."
  (let ((d (/ (- b a) beacon-size))
        (out (list a)))
    (dotimes (_ (1- beacon-size))
      (push (+ (car out) d) out))
    (nreverse out)))

(defun beacon--color-range ()
  "Return a list of background colors for the beacon."
  (let* ((default-bg (or (save-excursion
                           (unless (eobp)
                             (forward-line 1)
                             (unless (or (bobp) (not (bolp)))
                               (forward-char -1)))
                           (background-color-at-point))
                         (face-background 'default)))
         (bg (color-values (if (or (not (stringp default-bg))
                                   (string-match "\\`unspecified-" default-bg))
                               (face-attribute 'beacon-fallback-background :background)
                             default-bg)))
         (fg (cond
              ((stringp beacon-color) (color-values beacon-color))
              ((and (stringp bg)
                    (< (color-distance "black" bg)
                       (color-distance "white" bg)))
               (make-list 3 (* beacon-color 65535)))
              (t (make-list 3 (* (- 1 beacon-color) 65535))))))
    (when bg
      (apply #'seq-mapn (lambda (r g b) (format "#%04x%04x%04x" r g b))
             (mapcar (lambda (n) (butlast (beacon--int-range (elt fg n) (elt bg n))))
                     [0 1 2])))))


;;; Blinking
(defun beacon--shine ()
  "Shine a beacon at point."
  (let ((colors (beacon--color-range)))
    (save-excursion
      (while colors
        (if (looking-at "$")
            (progn
              (beacon--after-string-overlay colors)
              (setq colors nil))
          (beacon--colored-overlay (pop colors))
          (forward-char 1))))))

(defun beacon--dec ()
  "Decrease the beacon brightness by one."
  (pcase (beacon--ov-at-point)
    (`nil (beacon--vanish))
    ((and o (let c (overlay-get o 'beacon-colors)) (guard c))
     (beacon--ov-put-after-string o (cdr c)))
    (o
     (delete-overlay o)
     (save-excursion
       (while (and (condition-case nil
                       (progn (forward-char 1) t)
                     (end-of-buffer nil))
                   (setq o (beacon--ov-at-point)))
         (let ((colors (overlay-get o 'beacon-colors)))
           (if (not colors)
               (move-overlay o (1- (point)) (point))
             (forward-char -1)
             (beacon--colored-overlay (pop colors))
             (beacon--ov-put-after-string o colors)
             (forward-char 1))))))))

;;;###autoload
(defun beacon-blink ()
  "Blink the beacon at the location of the cursor.
Unlike `beacon--blink-automated', the beacon will blink
unconditionally (even if `beacon-mode' is disabled), and this can
be invoked as a user command or called from Lisp code."
  (interactive)
  (run-hooks 'beacon-before-blink-hook)
  (when (timerp beacon--timer)
    (cancel-timer beacon--timer))
  (beacon--vanish)
  (beacon--shine)
  (setq beacon--timer
        (run-at-time beacon-blink-delay
                     (/ beacon-blink-duration 1.0 beacon-size)
                     #'beacon--dec)))

(defun beacon--blink-automated (&optional ignore-check)
  "If appropriate, blink the beacon at the location of the cursor.
Unlike `beacon-blink', the blinking is conditioned on a series of
variables: `beacon-mode', `beacon-dont-blink-commands',
`beacon-dont-blink-major-modes', and
`beacon-dont-blink-predicates'."
  ;; Record vars here in case something is blinking outside the
  ;; command loop.
  (unless (or (run-hook-with-args-until-success 'beacon-dont-blink-predicates)
              (seq-find #'derived-mode-p beacon-dont-blink-major-modes)
              (memq (or this-command last-command) beacon-dont-blink-commands))
    (beacon-blink)
    (beacon--maybe-push-mark ignore-check)))


;;; Movement detection
(defun beacon--movement-> (delta-y &optional delta-x)
  "Return non-nil if latest vertical movement is > DELTA-Y.
If DELTA-Y is nil, return nil.
The same is true for DELTA-X and horizonta movement."
  (and delta-y
       (markerp beacon--pre-command-point-marker)
       (equal (marker-buffer beacon--pre-command-point-marker)
              (current-buffer))
       ;; Quick check that prevents running the code below in very
       ;; short movements (like typing).
       (> (abs (- (point) beacon--pre-command-point-marker))
          delta-y)
       ;; Col movement.
       (or (and delta-x
                (> (abs (- (current-column)
                           (save-excursion
                             (goto-char beacon--pre-command-point-marker)
                             (current-column))))
                   delta-x))
           ;; Check if the movement was >= DELTA lines by moving DELTA
           ;; lines. `count-screen-lines' is too slow if the movement had
           ;; thousands of lines.
           (save-excursion
             (let ((p (point)))
               (goto-char (min beacon--pre-command-point-marker p))
               (vertical-motion delta-y)
               (> (max p beacon--pre-command-point-marker)
                  (line-beginning-position)))))))

(defun beacon--scroll-command-p (cmd)
  (or (equal cmd 'scroll-up-command)
      (equal cmd 'scroll-down-command)))

(defun beacon-increase-mark-position (&optional step)
  "Used to navigate to the previous location on beacon-mark-ring.
1. Increments beacon-mark-ring-traversal-position.
2. Jumps to the mark at that position.
Borrows code from `pop-global-mark'."
  (interactive)
  (when beacon-mark-ring
    (cl-incf beacon-mark-ring-traversal-position step)
    (setq beacon-mark-ring-traversal-position (mod beacon-mark-ring-traversal-position (length beacon-mark-ring)))
    (beacon--go-to-marker (elt beacon-mark-ring beacon-mark-ring-traversal-position))
    (message "beacon-mark-position: %s" beacon-mark-ring-traversal-position)))

(defun beacon-push-mark-wrapper (&rest args)
  "Allows one to bind push-mark to various commands of your choosing.
Optional argument ARGS completely ignored"
  (beacon--push-mark (point) 'silent))

(defun beacon--backward-forward-last-command-p ()
  (or (equal last-command 'beacon-backward-forward-previous)
      (equal last-command 'beacon-backward-forward-next)))

(defun beacon--backward-forward-this-command-p ()
  (or (equal this-command 'beacon-backward-forward-previous)
      (equal this-command 'beacon-backward-forward-next)))

(defun beacon-backward-forward-previous ()
  "A `beacon-increase-mark-position' wrap for skip invalid locations."
  (interactive)
  (when (not (beacon--backward-forward-last-command-p))
    (setq beacon-mark-ring-traversal-position -1))
  (beacon-increase-mark-position 1))

(defun beacon-backward-forward-next ()
  "A `beacon-increase-mark-position' wrap for skip invalid locations."
  (interactive)
  (when (not (beacon--backward-forward-last-command-p))
    (setq beacon-mark-ring-traversal-position 0))
  (beacon-increase-mark-position -1))

(defun beacon--push-mark (&optional location nomsg activate)
  "Handles mark-tracking work for backward-forward.
ignores its arguments LOCATION, NOMSG, ACTIVATE
Uses following steps:
pushes the just-created mark by `push-mark' onto beacon-mark-ring
\(If we exceed beacon-mark-ring-max then old marks are pushed off\)

note that perhaps this should establish one ring per window in the future"
  (unless beacon-in-progress
    ;;      (message "beacon--push-mark %S %S %S" location nomsg activate)
    ;; based on push-mark
    (let* ((buffer (current-buffer))
           (marker (set-marker (mark-marker) (or location (point)) buffer))
           (old (nth beacon-mark-ring-max beacon-mark-ring))
           (history-delete-duplicates nil))
      ;;don't insert duplicate marks
      (if (or (eql (length beacon-mark-ring) 0)
              (not (and (eql location (marker-position (elt beacon-mark-ring 0)))
                        (eql buffer (marker-buffer (elt beacon-mark-ring 0))))))
          (progn
            ;;                (message "pushing marker %S" marker)
            (setq beacon-mark-ring (cons (copy-marker marker) beacon-mark-ring))))
      ;; (add-to-history 'beacon-mark-ring (copy-marker marker) beacon-mark-ring-max t)
      ;; (when old (set-marker old nil))

      ;;purge excess entries from the end of the list
      (when (> (length beacon-mark-ring) beacon-mark-ring-max)
        (move-marker (car (nthcdr beacon-mark-ring-max beacon-mark-ring)) nil)
        (setcdr (nthcdr (1- beacon-mark-ring-max) beacon-mark-ring) nil)))))

(defun beacon--go-to-marker (marker)
  "See pop-to-global-mark for where most of this code came from.
Argument MARKER the marker, in any buffer, to go to."
  (let* ((buffer (marker-buffer marker))
         (location (marker-position marker))
         (beacon-in-progress t))
    (if (null buffer)
        (message "buffer no longer exists.")
      (progn
        (if (eql buffer (current-buffer))
            (goto-char marker)
          (progn
            (set-buffer buffer)
            (or (and (>= location (point-min))
                     (<= location (point-max)))
                (if widen-automatically
                    (widen)
                  (error "Global mark location is outside accessible part of buffer")))
            (goto-char location)
            (switch-to-buffer buffer)))))))

;;;###autoload
(defun beacon--push-mark-with-check (&optional ignore-check)
  (when (or ignore-check
            (beacon--movement-> beacon-push-mark-p-or-threshold))
    (beacon--push-mark beacon--pre-command-point-marker 'silent)))

;;;###autoload
(defun beacon--maybe-push-mark (&optional ignore-check)
  "Maybe push pre-command point to beacon-mark-ring."
  (when (not mark-active)
    (cond
     ((beacon--backward-forward-this-command-p)
      (when (not (beacon--backward-forward-last-command-p))
        ;; first invocation of beacon-backward-forward, save point-marker, but not in mark ring
        (setq beacon-last-mark-before-jump beacon--pre-command-point-marker)))
     ;; run at first and last scroll, the last scroll position is saved
     ((not (and (beacon--scroll-command-p this-command)
                (beacon--scroll-command-p last-command)))
      (beacon--push-mark-with-check ignore-check))
     ((and (not (beacon--backward-forward-last-command-p))
           (or
            ;; mark added to mark-ring
            (not (eq beacon--pre-command-mark-ring-head (car mark-ring)))
            ;; no mark added, but command has changed point
            (not (equal (point-marker) beacon--pre-command-point-marker))))
      (beacon--push-mark-with-check ignore-check)))))

(defun beacon--post-command ()
  "Blink if point moved very far."
  (cond
   ;; Sanity check.
   ((not (markerp beacon--pre-command-point-marker)))
   ;; Blink for switching buffers.
   ((not (eq (marker-buffer beacon--pre-command-point-marker)
             (current-buffer)))
    (beacon--blink-automated))
   ;; Blink for switching windows.
   ((not (eq beacon--pre-command-window (selected-window)))
    (beacon--blink-automated))
   ;; Blink for scrolling.
   ((and beacon--window-scrolled
         (equal beacon--window-scrolled (selected-window)))
    (beacon--blink-automated))
   ;; Blink for movement
   ((beacon--movement-> beacon-push-mark-p-or-threshold)
    (beacon--blink-automated t)))
  (when (and (beacon--backward-forward-last-command-p)
             (equal this-command 'keyboard-quit)
             beacon-last-mark-before-jump)
    (beacon--go-to-marker beacon-last-mark-before-jump)
    (beacon-blink))
  (setq beacon--window-scrolled nil))

(defun beacon--window-scroll-function (window start-pos)
  "Blink the beacon or record that WINDOW has been scrolled.
If invoked during the command loop, record the current window so
that it may be blinked on post-command.  This is because the
scrolled window might not be active, but we only know that at
`post-command-hook'.

If invoked outside the command loop, `post-command-hook' would be
unreliable, so just blink immediately."
  (unless (and (equal beacon--pre-command-window-start start-pos)
               (equal beacon--pre-command-window window))
    (if this-command
        (setq beacon--window-scrolled window)
      (setq beacon--window-scrolled nil))))


;;; Minor-mode
(defcustom beacon-lighter
  (cond
   ;; ((char-displayable-p ?💡) " 💡")
   ;; ((char-displayable-p ?Λ) " Λ")
   (t " (*)"))
  "Lighter string used on the mode-line."
  :type 'string)

;;;###autoload
(define-minor-mode beacon-mode
  nil :lighter beacon-lighter
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<C-left>") #'beacon-backward-forward-previous)
            (define-key map (kbd "<C-right>") #'beacon-backward-forward-next)
            map)
  (if beacon-mode
      (progn
        ;; (advice-add 'push-mark :after #'beacon--maybe-push-mark)
        ;; (advice-add 'ggtags-find-tag-dwim :before #'beacon-push-mark-wrapper)
        ;; (advice-add 'switch-to-buffer :before #'beacon-push-mark-wrapper)

        (add-hook 'window-scroll-functions #'beacon--window-scroll-function)
        (add-function :after after-focus-change-function #'beacon--blink-automated)
        (add-hook 'post-command-hook #'beacon--post-command)
        (add-hook 'before-change-functions #'beacon--vanish)
        (add-hook 'pre-command-hook #'beacon--record-vars))

    ;; (advice-remove 'push-mark #'beacon--maybe-push-mark)
    ;; (advice-remove 'ggtags-find-tag-dwim #'push-mark)
    ;; (advice-remove 'switch-to-buffer #'beacon-push-mark-wrapper)

    (remove-function after-focus-change-function #'beacon--blink-automated)
    (remove-hook 'window-scroll-functions #'beacon--window-scroll-function)
    (remove-hook 'post-command-hook #'beacon--post-command)
    (remove-hook 'before-change-functions #'beacon--vanish)
    (remove-hook 'pre-command-hook #'beacon--record-vars)))

(provide 'beacon)
;;; beacon.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
