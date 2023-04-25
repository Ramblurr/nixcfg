" Reformat whole buffer
nnoremap <leader>m=    :action ReformatCode<CR>

" Reformat selected lines
vnoremap <leader>m=    :action ReformatCode<CR>

" Repl set ns
nnoremap <leader>mrn :action :cursive.repl.actions/switch-namespace<CR>
vnoremap <leader>mrn <Esc>:action :cursive.repl.actions/switch-namespace<CR>

" Repl load buffer
nnoremap <leader>mrl :action :cursive.repl.actions/load-file<CR>
vnoremap <leader>mrl <Esc>:action :cursive.repl.actions/load-file<CR>

" Eval Defun
nnoremap <leader>med :action :cursive.repl.actions/run-top-sexp<CR>
vnoremap <leader>med <Esc>:action :cursive.repl.actions/run-top-sexp<CR>

" Eval Last sexp
nnoremap <leader>mee :action :cursive.repl.actions/run-last-sexp<CR>
vnoremap <leader>mee <Esc>:action :cursive.repl.actions/run-last-sexp<CR>

" barf forward lispyville-barf
nnoremap < :action :cursive.actions.paredit/barf-forwards<cr>

" barf forward lispyville-slurp
nnoremap < :action :cursive.actions.paredit/slurp-forwards<cr>

nnoremap <leader>bb :action RecentFiles<cr>
nnoremap <leader>pt :action ActivateProjectToolWindow<cr>
nnoremap <leader>bd :q<cr>
nnoremap <leader>wu :action ReopenClosedTab<cr>
nnoremap <leader>pf :action GotoFile<cr>
nnoremap <leader>pa :action GotoSymbol<cr>
nnoremap <leader>; :action CommentByLineComment<cr>
vnoremap <leader>kw :action :cursive.actions.paredit/wrap-paren<cr>
nnoremap <leader>kw :action :cursive.actions.paredit/wrap-paren<cr>
nnoremap <leader>kr :action :cursive.actions.paredit/raise<cr>
nnoremap <leader>kB :action :cursive.actions.paredit/barf-backwards<cr>
nnoremap <leader>kb :action :cursive.actions.paredit/barf-forwards<cr>
nnoremap <leader>kdx :action :cursive.actions.paredit/kill-sexp<cr>
nnoremap <C-k>       :action :cursive.actions.paredit/kill-sexp<cr>
nnoremap <leader>kS :action :cursive.actions.paredit/slurp-backwards<cr>
nnoremap <leader>ks :action :cursive.actions.paredit/slurp-forwards<cr>
nnoremap  S-,       :action :cursive.actions.paredit/slurp-forwards<cr>
nnoremap  S-.       :action :cursive.actions.paredit/barf-forwards<cr>
nnoremap <leader>ke :action :cursive.actions.paredit/splice-killing-backwards<cr>
nnoremap <leader>kE :action :cursive.actions.paredit/splice-killing-forwards<cr>
nnoremap <leader><tab> :action Back<cr>

" sources
" https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/clojure/config.el
" https://gist.github.com/krisleech/1229ea028ad58c6dbd76c9bca4abb1e2
