;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


(defun sudo-command (command-to-run)
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? ")) " | sudo -S " command-to-run)))

(defun sudo-command-async (command-to-run)
  (async-shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? ")) " | sudo -S " command-to-run)))



(defun load-settings (&rest files)
  "Load multiple config files from ~/.config/doom/configs/."
  (dolist (file files)
    (load (expand-file-name file "~/.config/doom/configs/"))))

(load-settings
 "general.el"                           ; general settings
 "fullscreen.el"                        ; allows full screen mode
 "center_text.el"                       ; allows buffer to center text
 "logo.el"                              ; dashboard settings
 "vterm.el"                             ; terminal emulator settings
 "epub.el"                              ; epub reader settings
 "org.el"                               ; org mode settings
 "lsp.el"                               ; code completion settings
 "treesitter.el"                        ; syntax highlighting settings
 "guile.el"                             ; scheme lang settings
 "dired.el"                             ; file explorer settings
 "evil.el"                              ; vim settings
 "projectile.el"                        ; project management settings
 "treemacs.el"                          ; project drawer settings
 "vertico.el"                           ; search settings
 "buffers.el"                           ; scripts for buffer management
 "scripts.el"                           ; run bash scripts functions
 "dap.el"                               ; DAP configs
 "hotkeys.el"                           ; hotkeys for all scripts and doom
 "company.el"                           ; autcompletion config
 "aider.el"                             ; AI vibe coding 
 ;; "centaur.el"
 ;; "emms.el"                              ; media player settings
 ;; "emojify.el"                           ; lazy load emojis
 )

;; At the very top of ~/.doom.d/init.el
(defvar my-init-start-time (current-time))

;; At the very end of ~/.doom.d/config.el
(message "Doom init elapsed: %.2fs" (float-time (time-since my-init-start-time)))

(setenv "PATH" (concat (getenv "PATH") ":/home/pascal/.local/bin"))
(setq exec-path (append exec-path '("/home/pascal/.local/bin")))

;; Emerge install package
(require 'vtable)

(defun equery-install/update-table ()
  (interactive) 
  (let* ((col (vtable-current-column))
         (obj (vtable-current-object))
         (table (vtable-current-table)))
    (when (and obj (member col '(0 1))) 
      (let ((new-val (if (string= (nth col obj) "-") "+" "-")))
        (progn
          ;; Visually update
          (setf (nth col obj) new-val)
          (forward-line)
          ;; Update list
          (setf (nth (- (string-to-number (what-line)) 1) (nth col emerge-install-flags)) new-val)))
      (vtable-update-object table obj))))

(defun emerge/next-col ()
  (interactive)
  (when (= (vtable-current-column) 0)
    (vtable-next-column)))

(defun emerge/prev-col ()
  (interactive)
  (when (= (vtable-current-column) 1)
    (vtable-previous-column)))


;; (setq emerge-install-flags flags-list)
;; (setq emerge-package-to-install package-to-install)

;; Either append the file that has same name as package, or use first package reference


(defun emerge/get-correct-filename ()
  (let* ((filenames (shell-command-to-string (format "grep -rl \"^%s\" /etc/portage/package.use/" emerge-package-to-install)))
         (filenames-list (split-string filenames "\n" t))
         (second-half-name (nth 1 (split-string emerge-package-to-install "/"))))
    
    (cond
     ;; CASE 1: Package not found anywhere -> Return 'new-file
     ((string-empty-p filenames) 
      (list 'new-file second-half-name))
     
     ;; CASE 2: Found in a file named after the package -> Return 'update
     ((member (concat "/etc/portage/package.use/" second-half-name) filenames-list)
      (list 'update second-half-name))
     
     ;; CASE 3: Found in some other file (e.g., "common") -> Return 'update with that filename
     (t 
      (list 'update (file-name-nondirectory (car filenames-list)))))))

(defun emerge/create-flags-string ()
  (let ((result emerge-package-to-install)) ; Removed parens around variable
    (dolist (flag-obj emerge-install-flags)
      ;; Fixed the parenthesis here so setq is INSIDE the when
      (when (string= (nth 1 flag-obj) "+")
        (setq result (concat result (format " %s" (nth 2 flag-obj))))))
    result))


(defun emerge/run-install-command ()
  (interactive)
  (let* ((file-info (emerge/get-correct-filename))
         (status (car file-info))
         (target-name (nth 1 file-info))
         (install-string (emerge/create-flags-string))
         (full-file-name (concat "/etc/portage/package.use/" target-name)))
    
    (if (eq status 'update)
        ;; Logic for SED (the line exists)
        (progn
          (sudo-command (format "sed -i 's|^%s.*|%s|' %s && " 
                                emerge-package-to-install 
                                install-string 
                                full-file-name))
          (sudo-command-async (format "emerge %s" emerge-package-to-install)))
      
      ;; Logic for NEW FILE (the line doesn't exist)
      (progn (sudo-command (format "sh -c 'echo %s >> %s'" 
                                   (shell-quote-argument install-string) 
                                   (shell-quote-argument full-file-name)))
             (sudo-command-async (format "emerge %s" emerge-package-to-install))))))

(define-minor-mode emerge-install-mode
  "Minor mode for interacting with emerge"
  :lighter "Install flags"
  :keymap `((,(kbd "C-c m") . 'equery-install/update-table)))

(defun emerge-install--extract-flags-from-equery (equery-output)
  (mapcar (lambda (line)
            (split-string line "|" t))
          (split-string equery-output "\n" t)))

(defun emerge-install--create-emerge-flags-buffer-table (flags-list package-to-install)
  (with-current-buffer (get-buffer-create "*Gentoo-Install*")
    (make-vtable :columns '("U" "I" "flag" "desc") :objects flags-list)
    (setq emerge-install-flags flags-list)
    (setq emerge-package-to-install package-to-install)
    (setq header-line-format "C-c C-c to confirm changes")
    (emerge-install-mode 1)
    (read-only-mode)
    (hl-line-mode 1)
    (display-buffer (current-buffer))))

(defun emerge-install-package (package)
  "Searches for package to install and installs it with user flags."
  (interactive "sSearch for package(equery): ")
  (let* ((cmd-output (shell-command-to-string 
                      (format "eix %s | sed -n 's/^\\*[ ]*\\([a-zA-Z0-9\\/-]*\\).*/\\1/p'" 
                              package)))
         (cmd-options (split-string cmd-output "\n" t))
         (package-to-install (completing-read "Results: " cmd-options))
         (flag-data (emerge-install--extract-flags-from-equery (shell-command-to-string (format "script -q -c \"equery --no-color u %s\" /dev/null | sed -n 's/^[[:space:]]*\\([+-]\\)[[:space:]]*\\([+-]\\)[[:space:]]*\\([^[:space:]:]*\\)[[:space:]]*:[[:space:]]*\\(.*\\)/\\1|\\2|\\3|\\4/p'" 
                                                                                                package-to-install)))))
    (emerge-install--create-emerge-flags-buffer-table flag-data package-to-install)))

;; Emerge install hotkeys
(evil-define-key 'normal emerge-install-mode-map 
  (kbd "l") 'emerge/next-col)
(evil-define-key 'normal emerge-install-mode-map 
  (kbd "h") 'emerge/prev-col)
(evil-define-key 'normal emerge-install-mode-map 
  (kbd "C-c C-c") 'emerge/run-install-command)
(evil-define-key 'normal emerge-install-mode-map 
  (kbd "RET") 'equery-install/update-table)

