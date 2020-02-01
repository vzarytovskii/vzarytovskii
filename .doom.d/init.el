;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       (company +childframe +auto)           ; the ultimate code completion backend
       (ivy +fuzzy +childframe)               ; a search engine for love and life

       :ui
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       fill-column       ; a `fill-column' indicator
       ophints
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       pretty-code       ; replace bits of code with pretty symbols
       treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select +ace-window)     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       objed             ; text object editing for the innocent
       parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       term              ; terminals in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       spell             ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       lsp
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pdf               ; pdf enhancements
       tmux              ; an API for interacting with tmux

       :lang
       cc                ; C/C++/Obj-C madness
       clojure           ; java with a lisp
       (csharp +lsp)            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       (fsharp +lsp)           ; ML stands for Microsoft's Language
       (haskell +dante +lsp)  ; a language that's lazier than I am
       markdown          ; writing docs for people to ignore
       nix               ; I hereby declare "nix geht mehr!"
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
        +jupyter        ; ipython/jupyter support for babel
        +pandoc          ; export-with-pandoc support
        +pomodoro        ; be fruitful with the tomato technique
        +present)        ; using org-mode for presentations
       plantuml          ; diagrams for confusing people more
       (python +lsp)            ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       rst               ; ReST in peace
       rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (sh +lsp)                ; she sells {ba,z,fi}sh shells on the C xor

       :app
       calendar
       twitter           ; twitter client https://twitter.com/vnought

       :config
       (default +bindings +smartparens))
