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
  ;;(setq buffer-face-mode-face '(:font "DejaVu Sans Mono-15"))
  ;;(buffer-face-mode t)
  (set-frame-font "DejaVu Sans Mono-18")
  (setq company-idle-delay 0.1)
 )
(set-company-backend! 'anki-editor-mode '(:seperate company-math-symbols-unicode company-yasnippet))
;;(set-company-backend! 'anki-editor-mode 'company-math-symbols-unicode 'company-yasnippet 'company-dabbrev)

(defun set-company-math ()
  (interactive)
  (add-to-list 'company-backends 'company-math-symbols-unicode))
;; need to have clozes labelled incrementally.
(defun baskaran/anki-editor-cloze ()
  "Cloze region from BEGIN to END with number ARG."
  (let ((region (buffer-substring (region-beginning) (region-end))))
    (save-excursion
      (delete-region (region-beginning) (region-end))
      (insert (with-output-to-string
                (princ (format "{{c%d::%s" baskaran/anki-cloze-counter region))
                (princ "}}")))))
  (setq baskaran/anki-cloze-counter (+ 1 baskaran/anki-cloze-counter))
  )

(defun baskaran/anki-insert-cloze-maths (maths-type-string)
  (setq baskaran/anki-cloze-counter 1)
  (outline-up-heading 1)
  (org-end-of-subtree)
  ;; ideally we'd check the parent for if adding Cloze Maths and orgtest were necessary.
  ;; also, make it so that the tree doesn't get deeper and deeper?
  ;; And maybe add tags?
  (anki-editor--insert-note-skeleton
   ""
   (org-entry-get-with-inheritance anki-editor-prop-deck) ;; just use level above's deck
   "Maths Item"
   "Cloze Maths"
   (anki-editor--anki-connect-invoke-result "modelFieldNames" `((modelName . "Cloze Maths"))))
  (baskaran/add-text-to-tree-end maths-type-string)
  (org-goto-first-child)
  (end-of-line)
  (org-newline-and-indent)
  (evil-insert-state)
  )

(defun baskaran/re-seq (regexp string)
  "Get a list of all regexp matches in a string. matches nth group, feed in 0 for default"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(setq baskaran-test-str
      "An {{c3::open neighbourhood}} of P, a point in (X,d), is an {{c1::open subset}} N {{c14::containing P}}")

(defun baskaran/get-cloze-numbers (string)
  (mapcar
   (lambda (str) (string-to-number (substring str 3 -1)))
   (baskaran/re-seq
    "{{c[[:digit:]]+[[:blank:]]*::"
    ;; (rx "{{c" (one-or-more digit) (zero-or-more blank) "::")
    string)))

(defun least-missing-natural (n-seq)
  (cl-loop for x from 1
           until (null (member x n-seq))
           finally return x))
;; (least-missing-natural (baskaran/get-cloze-numbers basaran))

(defun baskaran/add-text-to-tree-end (text)
  (save-excursion
    (org-end-of-subtree)
    (end-of-line)
    (save-excursion (insert text))
    (newline)))

;; like push-notes, but only creates
(defun baskaran/anki-create-notes (&optional arg match scope)
  (interactive "P")

  (unless scope
    (setq scope (cond
                 ((region-active-p) 'region)
                 ((equal arg '(4)) 'tree)
                 ((equal arg '(16)) 'file)
                 ((equal arg '(64)) 'agenda)
                 (t nil))))

  (let* ((total (progn
                  (message "Counting notes...")
                  (length (anki-editor-map-note-entries t match scope))))
         (acc 0)
         (failed 0))
    (anki-editor-map-note-entries
     (lambda ()
       (message "[%d/%d] Processing notes in buffer \"%s\", wait a moment..."
                (cl-incf acc) total (buffer-name))
       (anki-editor--clear-failure-reason)
       (condition-case-unless-debug err
  (if (= (alist-get 'note-id (anki-editor-note-at-point)) -1)
           (anki-editor--push-note (anki-editor-note-at-point)))
         (error (cl-incf failed)
                (anki-editor--set-failure-reason (error-message-string err)))))
     match
     scope)

    (message (if (= 0 failed)
                 (format "Successfully pushed %d notes to Anki." acc)
               (format "Pushed %d notes, %d of which are failed. Check property drawers for failure reasons. Once you've fixed the issues, you could use `anki-editor-retry-failure-notes' to re-push the failed notes."
                       acc failed)))))

(map! :after anki-editor
      :leader
      (:prefix ("a" . "anki")
       :desc "cloze region" "c" (lambda! (baskaran/anki-editor-cloze))
       :desc "push cards to Anki" "P" (lambda! (baskaran/anki-push-notes))
       :desc "push new cards to Anki" "p" #'baskaran/anki-create-notes
       :desc "retry pushing cards" "r" #'anki-editor-retry-failure-notes
       :desc "new card template" "n" #'anki-editor-insert-note
       (:prefix ("m" . "maths")
        :desc "test" "a" (λ! (baskaran/anki-insert-cloze-maths "Defn"))
        :desc "Definition (cloze)" "d" (λ! (baskaran/anki-insert-cloze-maths "Defn"))
        :desc "Theorem (cloze)" "t" (λ! (baskaran/anki-insert-cloze-maths "Thm"))
        :desc "Proof (cloze)" "p" (λ! (baskaran/anki-insert-cloze-maths "Proof"))
        :desc "Definition (basic)" "b" #'anki-editor-insert-note
        :desc "Set company-math-unicode" "c" #'set-company-math)
       ))


;;(add-hook 'org-journal-mode-hook #'set-company-math)


;; maths everywhere
;; wh(doom/increase-font-size 1)

;; Julia!
(use-package! julia-snail
  :hook (julia-mode . julia-snail-mode)
  :init
  ;; code here runs immediately
  :config
  ;;(setq anki-editor-break-consecutive-braces-in-latex 't)
  ;;(setq buffer-face-mode-face '(:font "DejaVu Sans Mono-15"))
  ;;(buffer-face-mode t)
  ;;(set-frame-font "DejaVu Sans Mono-18")
 )
(map! :after julia-snail
      :leader
      (:prefix ("j". "julia")
       :desc "REPL" "z" #'julia-snail
       :desc "Activate Project" "a" #'julia-snail-package-activate
       :desc "Doc Lookup" "d" #'julia-snail-doc-lookup
       :desc "Send Block" "b" #'julia-snail-send-top-level-form
       :desc "Send Line"  "l" #'julia-snail-send-line
       :desc "Send Region" "r" #'julia-snail-send-region
       :desc "Send DWIM" "e" #'julia-snail-send-dwim
       :desc "Send File" "k" #'julia-snail-send-buffer-file
       :desc "Update for Revise" "R" #'julia-snail-update-module-cache))

(let ((process-environment tramp-remote-process-environment))
  (setenv "ENV" "$HOME/.bashrc")
  (setq tramp-remote-process-environment process-environment))
