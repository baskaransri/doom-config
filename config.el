;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Baskaran Sripathmanathan"
      user-mail-address "baskaran@robots.ox.ac.uk")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq
 org_notes (concat (getenv "HOME") "/Research/Notes/")
 zot_bib (concat (getenv "HOME") "/Research/masterLibrary.bib")
 org-directory org_notes
 deft-directory org_notes
 org-roam-directory org_notes
 )

(setq undo-limit 80000000
      evil-want-fine-undo t
      truncate-string-ellipsis "…")


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; place repl in non-active window
(after! python
  (set-popup-rule! "^\\*Python*" :ignore t))

;; for Helm-bibtex:
(setq
 bibtex-completion-notes-path org_notes
 bibtex-completion-bibliography zot_bib
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
  )
 )

(use-package! org-ref
  :config
  (setq
   org-ref-completion-library 'org-ref-helm-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-default-bibliography (list zot_bib)
   org-ref-bibliography-notes (concat org_notes "bibnotes.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory org_notes
   org-ref-notes-function 'orb-edit-notes
   )
  )

;; time for some anki!
;; Think about not just hooking into journal mode?
(make-variable-buffer-local 'baskaran/anki-cloze-counter)
(use-package! anki-editor
  :hook (org-journal-mode . anki-editor-mode)
  :init
  ;; code here runs immediately
  (setq-default baskaran/anki-cloze-counter 1)
  :config
  (setq anki-editor-break-consecutive-braces-in-latex 't)
 )


(defun baskaran/anki-editor-cloze ()
  "Cloze region from BEGIN to END with number ARG."
  (setq baskaran/anki-cloze-counter (+ 1 baskaran/anki-cloze-counter))
  (let ((region (buffer-substring (region-beginning) (region-end))))
    (save-excursion
      (delete-region (region-beginning) (region-end))
      (insert (with-output-to-string
                (princ (format "{{c%d::%s" baskaran/anki-cloze-counter region))
                (princ "}}"))))))

(defun baskaran/anki-insert-cloze-maths ()
  (anki-editor--insert-note-skeleton "" "orgtest" "Maths Item" "Cloze Maths" ()))

(map! :after anki-editor
      :leader
      (:prefix ("a" . "anki")
       :desc "cloze region" "c" (lambda! (baskaran/anki-editor-cloze))
       :desc "push cards to Anki" "p" #'anki-editor-push-notes
       :desc "retry pushing cards" "r" #'anki-editor-retry-failure-notes
       :desc "new card template" "n" #'anki-editor-insert-note
       (:prefix ("m" . "maths")
        :desc "test" "a" (lambda! (baskaran/anki-insert-cloze-maths))
        :desc "Definition (cloze)" "d" #'anki-editor-insert-note
        :desc "Theorem (cloze)" "t" #'anki-editor-insert-note
        :desc "Proof (cloze)" "p" #'anki-editor-insert-note
        :desc "Definition (basic)" "b" #'anki-editor-insert-note)
       ))
