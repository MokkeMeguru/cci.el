;;; claude-code-ide-session.el --- Claude Code terminal session functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Meguru Mokke <meguru.mokke@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1") (claude-code-ide-utils "0.1.0") (vterm "0.0.1"))
;; Keywords: ai, claude, ide, tools
;; URL: https://github.com/mixi-sb/claude-code-ide.el

;;; Commentary:

;; Terminal-based Claude Code session functionality including:
;; - Direct Claude CLI integration via vterm package
;; - Session management with conversation continuity
;; - Content sharing (buffers, regions, files)
;; - Slash commands

;;; Code:

(require 'project)
(require 'cl-lib)

;; Declare vterm functions to avoid compilation warnings
(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

;;;; Variables

;; Forward declarations for customizations from utils module
(defvar claude-code-ide-buffer-name "*claude-code*")
(defvar claude-code-ide-large-buffer-threshold 1000)
(defvar claude-code-ide-program "claude")
(defvar claude-code-ide-program-switches nil)
(defvar claude-code-ide-startup-delay 1)

(defvar claude-code-ide--buffer nil
  "The Claude Code buffer.")

;;;; Utility Functions

(defun claude-code-ide--display-buffer-right (buffer _alist)
  "Display BUFFER in a new window to the right.
This function is designed to be called as a `display-buffer' action."
  ;; alist is ignored for now as we don't need custom parameters.
  (let ((window (split-window-right)))
    (set-window-buffer window buffer)
    window))

(defun claude-code-ide--get-buffer ()
  "Return the Claude Code buffer if it exists, nil otherwise."
  (get-buffer claude-code-ide-buffer-name))

(defun claude-code-ide--running-p ()
  "Check if Claude Code is currently running."
  (not (null (claude-code-ide--get-buffer))))

(defun claude-code-ide--send-command (cmd)
  "Send command CMD to Claude Code if buffer exists."
  (if-let ((buffer (claude-code-ide--get-buffer)))
      (with-current-buffer buffer
        (if (eq major-mode 'vterm-mode)
            (progn
              (vterm-send-string cmd)
              (vterm-send-return)
              (display-buffer buffer))
          (error "Claude terminal is not properly initialized")))
    (error "Claude Code is not running")))

(defun claude-code-ide--start (dir &optional _arg continue)
  "Start Claude Code in directory DIR.
Always switches to Claude buffer after starting.
With non-nil CONTINUE, start Claude with --continue flag to continue
previous conversation."
  (unless (require 'vterm nil t)
    (error "Vterm package is required but not installed.  Please install it first"))

  ;; Check if Claude CLI exists
  (unless (executable-find claude-code-ide-program)
    (error "Claude CLI '%s' not found.  Please install Claude Code CLI first" claude-code-ide-program))

  ;; Check if already running
  (if (claude-code-ide--running-p)
      (progn
        (message "Claude Code is already running.  Use 'claude-code-ide-kill' to stop it first.")
        (pop-to-buffer (claude-code-ide--get-buffer))
        ;; Switch to the buffer
        (select-window (get-buffer-window (claude-code-ide--get-buffer))))

    (let* ((program-switches (if continue
                                 (append claude-code-ide-program-switches '("--continue"))
                               claude-code-ide-program-switches))
           (command (if program-switches
                        (format "%s %s" claude-code-ide-program
                                (mapconcat 'shell-quote-argument program-switches " "))
                      claude-code-ide-program)))

      ;; --- MODIFIED PART ---
      ;; First, create the window layout by splitting the current window.
      ;; Then, create the vterm process inside the newly created window.
      ;; This prevents vterm from taking over the original window.
      (let ((vterm-window (split-window-right)))
        (with-selected-window vterm-window
          ;; Now that we are in the target window, call vterm.
          (let* ((default-directory dir)
                 (vterm-buffer (vterm claude-code-ide-buffer-name)))

            (setq claude-code-ide--buffer vterm-buffer)

            ;; Send the startup command to the vterm buffer.
            (with-current-buffer vterm-buffer
              (vterm-send-string command)
              (vterm-send-return))

            (sleep-for claude-code-ide-startup-delay)
            (message "Claude Code started in %s" dir)))))))

;;;; Interactive Functions

;;;###autoload
(defun claude-code-ide-start (&optional arg)
  "Start Claude Code in current project directory.
Always switches to Claude buffer after starting.
With double prefix ARG (C-u C-u), continue previous conversation."
  (interactive "P")
  (let* ((dir (if (project-current)
                  (project-root (project-current))
                default-directory))
         (continue (equal arg '(16))))
    (claude-code-ide--start dir arg continue)))

;;;###autoload
(defun claude-code-ide-current-directory (&optional arg)
  "Start Claude Code in current directory.
Always switches to Claude buffer after starting.
With double prefix ARG (C-u C-u), continue previous conversation."
  (interactive "P")
  (let ((continue (equal arg '(16))))
    (claude-code-ide--start default-directory arg continue)))

;;;###autoload
(defun claude-code-ide-toggle ()
  "Show or hide the Claude Code window."
  (interactive)
  (let ((buffer (claude-code-ide--get-buffer)))
    (if buffer
        (if-let ((window (get-buffer-window buffer)))
            (quit-window nil window)
          ;; Use a lexically-scoped display-buffer-alist with the correct syntax.
          (let ((display-buffer-alist
                 `((,(regexp-quote (buffer-name buffer))
                    (claude-code-ide--display-buffer-right . ())))))
            (let ((window (display-buffer buffer t))) ; 't' forces a display action.
              (when (and window (window-live-p window))
                (select-window window)))))
      (error "Claude Code is not running"))))

;;;###autoload
(defun claude-code-ide-switch-to-buffer ()
  "Switch to the Claude Code buffer if it exists."
  (interactive)
  (if-let ((buffer (claude-code-ide--get-buffer)))
      (switch-to-buffer buffer)
    (error "Claude Code is not running")))

;;;###autoload
(defun claude-code-ide-kill ()
  "Kill Claude Code process and close its window."
  (interactive)
  (if-let ((buffer (claude-code-ide--get-buffer)))
      (progn
        (when-let ((window (get-buffer-window buffer)))
          (quit-window nil window))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (setq claude-code-ide--buffer nil)
        (message "Claude Code killed"))
    (error "Claude Code is not running")))

;;;###autoload
(defun claude-code-ide-send-command (cmd &optional switch)
  "Send command CMD to Claude Code.
With non-nil SWITCH, switch to Claude buffer after sending."
  (interactive "sClaude command: \nP")
  (claude-code-ide--send-command cmd)
  (when switch
    (claude-code-ide-switch-to-buffer)))

;;;###autoload
(defun claude-code-ide-send-region (&optional arg)
  "Send current region to Claude Code.
If no region is active, send entire buffer.
With prefix ARG, prompt for instructions."
  (interactive "P")
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (if (> (buffer-size) claude-code-ide-large-buffer-threshold)
                     (when (yes-or-no-p "Buffer is large.  Send anyway? ")
                       (buffer-substring-no-properties (point-min) (point-max)))
                   (buffer-substring-no-properties (point-min) (point-max)))))
         (prompt (when arg
                   (read-string "Instructions for Claude: ")))
         (full-text (if prompt
                        (format "%s\n\n%s" prompt text)
                      text)))
    (when full-text
      (claude-code-ide--send-command full-text))))

;;;###autoload
(defun claude-code-ide-send-buffer ()
  "Send current buffer to Claude Code."
  (interactive)
  (claude-code-ide-send-region))

;;;###autoload
(defun claude-code-ide-status ()
  "Show Claude Code IDE status."
  (interactive)
  (if (claude-code-ide--running-p)
      (message "Claude Code is running in buffer %s" claude-code-ide-buffer-name)
    (message "Claude Code is not running")))

;;;###autoload
(defun claude-code-ide-send-file (file)
  "Send FILE contents to Claude Code."
  (interactive "fFile to send: ")
  (when (file-exists-p file)
    (let ((content (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))
      (claude-code-ide--send-command
       (format "Here's the content of %s:\n\n%s" file content)))))

;;;; Slash Commands

;;;###autoload
(defun claude-code-ide-slash-help ()
  "Send /help command to Claude."
  (interactive)
  (claude-code-ide--send-command "/help"))

;;;###autoload
(defun claude-code-ide-slash-clear ()
  "Send /clear command to Claude."
  (interactive)
  (claude-code-ide--send-command "/clear"))

;;;###autoload
(defun claude-code-ide-slash-model ()
  "Send /model command to Claude."
  (interactive)
  (claude-code-ide--send-command "/model"))

;;;###autoload
(defun claude-code-ide-slash-quit ()
  "Send /quit command to Claude."
  (interactive)
  (claude-code-ide--send-command "/quit"))

(provide 'claude-code-ide-session)
;;; claude-code-ide-session.el ends here
