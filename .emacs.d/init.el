;; Bump GC threshold and other stuff to speed up LSP  -*- lexical-binding: t; -*-
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 8 1024 1024)) ;; 8 megabytes

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;(unless (server-running-p)
;;  (server-start))
(server-start)
(desktop-save-mode)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Don't clutter working directories
(setq auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

(load-theme 'deeper-blue)
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq frame-resize-pixelwise t)

(setq display-line-numbers-mode 1)
(global-linum-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(column-number-mode 1)

;; Fuck tabs
(setq-default indent-tabs-mode nil)

(setq inhibit-compacting-font-caches t)

;; Packages
(when (not (package-installed-p 'use-package))
  (message "Installing 'use-package'")
  (package-install 'use-package))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode))
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "DarkOrchid3"))))
  (rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "orange2"))))
  (rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "steel blue"))))
  (rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "SeaGreen2"))))
  (rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "OrangeRed2"))))
  (rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "IndianRed2"))))
  (rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan4"))))
  (rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "SpringGreen1"))))
  (rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "yellow1")))))

(use-package direnv
  :config
  (direnv-mode))

(use-package paren
  :ensure t
  :config
  (show-paren-mode t))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  ;; Set delay to 1 to avoid burning CPU too much
  (setq company-idle-delay 0))

(use-package elcord
  :ensure t
  :commands (elcord-mode))

(use-package smex
  :ensure t
  :commands (smex smex-major-mode-commands smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . (face-foreground 'warning))
          ("FIXME" . (face-foreground 'error))
          ("NOTE"  . (face-foreground 'success))
          ("XXX"   . (face-foreground 'error)))))

(use-package diff-hl
  :ensure t
  :hook (after-init . global-diff-hl-mode))

(use-package cider
  :ensure t
  :hook ((cider-mode . cider-company-enable-fuzzy-completion)
         (cider-repl-mode . cider-company-enable-fuzzy-completion)))

(use-package neotree
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package go-mode
  :ensure t
  :commands (go-mode)
  :mode (("\\.go\\'" . go-mode)))

(use-package rust-mode
  :ensure t)

;; it'll disable rust-mode itself
;; (use-package rustic
;;   :ensure t
;;   :commands (rustic-mode))

(use-package cargo
  :ensure t
  :commands (cargo-minor-mode)
  :hook ((rustic-mode . cargo-minor-mode)))

(use-package racer
  :ensure t
  :commands (racer-mode)
  :hook ((rustic-mode . racer-mode)
         (racer-mode . eldoc-mode)
         (racer-mode . company-mode)))

;; Multiple cursors!
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (dart-mode . lsp-deferred)
         (haskell-mode . lsp-deferred))
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (lsp-deferred)
                            (add-hook 'before-save-hook #'lsp-format-buffer t t)
                            (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(use-package lsp-ui
  :ensure t
  :hook ((rustic-mode . lsp-ui-mode)
         (rust-mode . lsp-ui-mode)))

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1))

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  :hook ((prog-mode . flycheck-mode)))

(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet
  :ensure t
  :requires (yasnippet-snippets)
  :commands (yas-minor-mode yas-reload-all)
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package aggressive-indent
  :ensure t
  :commands (aggressive-indent-mode)
  :hook ((lisp-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)))

(use-package org
  :ensure t)

(use-package magit
  :ensure t)

(use-package elm-mode
  :ensure t
  :config
  ;;(add-to-list 'company-backends 'elm-company)
  )

(use-package yaml-mode
  :ensure t)

(use-package nix-mode
  :ensure t)

(use-package dhall-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package dart-mode
  :ensure t)

(use-package lsp-dart
  :ensure t
  :config
  (add-hook 'dart-mode-hook (lambda ()
                              (setq lsp-dart-sdk-dir "/Users/mark/sdks/flutter/bin/cache/dart-sdk/")
                              (with-eval-after-load 'projectile
                                (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
                                (add-to-list 'projectile-project-root-files-bottom-up "BUILD")))))

(use-package graphql-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper"))

(use-package monokai-pro-theme
  :ensure t
  :config
  (load-theme 'monokai-pro t))

(use-package exec-path-from-shell
  :ensure t
  ;;  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-copy-envs '("PATH" "GOPATH" "JAVA_HOME" "SHELL"))
  (exec-path-from-shell-initialize)
  (let ((default-gopath (concat (getenv "HOME") "/go")))
    (unless (getenv "GOPATH")
      (setenv "GOPATH" default-gopath)
      (add-to-list 'exec-path (concat default-gopath "/bin"))))
  )

(use-package origami
  :ensure t
  :commands (origami-close-node-recursively origami-open-node origami-mode)
  :hook ((prog-mode . origami-mode))
  :bind
  (("C-c C-M-g" . origami-open-node)
   ("C-c C-M-f" . origami-close-node-recursively)))

(use-package telega
  :ensure t
  :config
  ;; Enable notifications only on Linux
  (if (and (string-equal "gnu/linux" system-type) (string-match-p "DBUS" system-configuration-features))
      (telega-notifications-mode 1))
  (add-hook 'telega-chat-mode-hook
            (lambda ()
              ;; Emoji, hashtag & username completion
              (set (make-local-variable 'company-backends)
                   (append '(telega-company-emoji
                             telega-company-username
                             telega-company-hashtag)
                           (when (telega-chat-bot-p telega-chatbuf--chat)
                             '(telega-company-botcmd))))
              ;; Nope
              (editorconfig-mode 0))))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config (dap-auto-configure-mode))

;;(use-package dap-java
;;  :ensure t)

(use-package all-the-icons
  :ensure t
  :config
  (setq neo-theme 'icons))

(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)))

(use-package erc-image
  :ensure t
  :config
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

(use-package vterm
  :ensure t)

;; (use-package helm
;;   :ensure t)

(defun testing-telega-ignore-hook (msg &rest notused)
  (let ((content (plist-get msg :content))
        (is-outgoing (plist-get msg :is_outgoing)))
    (when (and (not is-outgoing) (string= "messageSticker" (plist-get content :@type)))
      (message "Ignored sticker")
      (telega-msg-ignore msg))))

(add-hook 'telega-chat-insert-message-hook 'testing-telega-ignore-hook)

(let ((font-name (cond
                  ((memq window-system '(mac ns))
                   ;;"Fira Code Retina-13"
                   "Hack-14")
                  ((memq window-system '(x))
                   "Hack-10"))))
  (add-to-list 'default-frame-alist
               (cons 'font font-name))
  (set-frame-font font-name nil t)
  )

(when (string-equal system-type "darwin")
  ;; Needed to enable emoji rendering on OS X
  ;; Found from: https://github.com/zonuexe/emoji-fontset.el
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)

  (when (fboundp 'mac-auto-operator-composition-mode)
    ;; Enable font ligatures
    (mac-auto-operator-composition-mode))

  ;; https://gist.github.com/railwaycat/3498096
  (setq mac-option-modifier 'meta
        mac-command-modifier 'hyper)
  
  (global-set-key [(hyper a)] 'mark-whole-buffer)
  (global-set-key [(hyper v)] 'yank)
  (global-set-key [(hyper c)] 'kill-ring-save)
  (global-set-key [(hyper s)] 'save-buffer)
  (global-set-key [(hyper l)] 'goto-line)
  (global-set-key [(hyper w)]
                  (lambda () (interactive) (delete-window)))
  (global-set-key [(hyper z)] 'undo))

(add-hook 'clojure-mode-hook 'eldoc-mode)

;; Utilize native support if available
(when (fboundp 'native-compile-async)
  (message "native compile support present, woohoo")
  (condition-case err
      (let ((ignored-packages '("lsp" "telega"))
            (elpa-paths (directory-files "~/.emacs.d/elpa")))
        (let ((filtered-paths
               (seq-filter
                (lambda (f)
                  (when (and (not (string= "." f))
                             (not (string= ".." f)))
                    (let (elem val ip)
                      (setq ip ignored-packages)
                      (while (and ip (not val))
                        (setq elem (car ip))
                        (setq ip (cdr ip))
                        (when (string-prefix-p elem f)
                          (setq ip nil)
                          (setq val t)))
                      (not val))))
                elpa-paths)))
          (let (elem full-path)
            (while filtered-paths
              (setq elem (car filtered-paths))
              (setq filtered-paths (cdr filtered-paths))
              (setq full-path (format "~/.emacs.d/elpa/%s" elem))

              (message "Compiling path: %s" full-path)
              (native-compile-async full-path 4 t)))))
    (error (message "failed to native-compile-async: %s" err))))

(setq erc-interpret-mirc-color t)
(setq erc-rename-buffers t)
(setq erc-autojoin-channels-alist
      '(("irc.spi.gt" "#paper" "#paper-help" "#paper-dev")))

(add-to-list 'erc-modules 'netsplit)
(add-to-list 'erc-modules 'ring)
(add-to-list 'erc-modules 'stamp)
;; Enable notifications only on Linux
(if (and (string-equal "gnu/linux" system-type) (string-match-p "DBUS" system-configuration-features))
    (add-to-list 'erc-modules 'notifications))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("983eb22dae24cab2ce86ac26700accbf615a3f41fef164085d829fe0bcd3c236" default))
 '(global-whitespace-newline-mode nil)
 '(package-selected-packages
   '(direnv vterm libvterm erc-image lsp-dart treemacs dart-mode graphql-mode all-the-icons-dired all-the-icons neotree typescript-mode company-box racer cargo editorconfig telega dockerfile-mode origami yafolding fold-this yasnippet-snippets yaml-mode use-package smex rjsx-mode rainbow-delimiters nix-mode monokai-theme monokai-pro-theme magit lsp-ui hl-todo go-mode flycheck exec-path-from-shell epc elcord diff-hl dhall-mode commenter clj-refactor aggressive-indent 2048-game))
 '(tab-stop-list '(4))
 '(whitespace-action '(auto-cleanup)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "DarkOrchid3"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "orange2"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "steel blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "SeaGreen2"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "OrangeRed2"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "IndianRed2"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan4"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "SpringGreen1"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "yellow1"))))
 '(whitespace-line ((t nil)))
 '(whitespace-newline ((t (:foreground "darkgray" :weight normal)))))
