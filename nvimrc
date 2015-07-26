"-- Plugins ------------------------------------------------------------

call plug#begin('~/.nvim/plugged')

" Defaults everyone can agree on
Plug 'tpope/vim-sensible'

" A Git wrapper so awesome, it should be illegal
Plug 'tpope/vim-fugitive'

" Fuzzy file, buffer, mru, tag, etc finder
Plug 'kien/ctrlp.vim'

" A Vim alignment plugin
Plug 'junegunn/vim-easy-align'

" Wrapper of some neovim's :terminal functions
Plug 'kassio/neoterm'

" Material color scheme for Vim based on w0ng/vim-hybrid color scheme
Plug 'kristijanhusak/vim-hybrid-material'

" Base16 for Vim
Plug 'chriskempson/base16-vim'

" Lean & mean status/tabline for vim that's light as air
Plug 'bling/vim-airline'

" A vim mode for Haskell (forked from travitch/hasksyn)
"Plug '~/src/hasksyn'

call plug#end()


"-- Colors -------------------------------------------------------------

set background=dark

"colorscheme jellybeans
colorscheme hybrid_material
"colorscheme hybrid_reverse
"colorscheme base16-ocean


"-- Airline ------------------------------------------------------------

let g:airline_powerline_fonts = 1
let g:airline_theme= 'tomorrow'


"-- Leader Keys --------------------------------------------------------

let mapleader      = ","
let maplocalleader = ";"


"-- OS Clipboard -------------------------------------------------------

" Leader + y = copy selection to clipboard
vnoremap <Leader>y "+y

" Leader + p = paste from clipboard
nnoremap <Leader>p "+gpl
vnoremap <Leader>p :<C-U>call VPaste()<CR>

function! VPaste()
  normal gv
  normal "+P
  normal l
endfunction


"-- Buffers ------------------------------------------------------------

" Show line numbers
set number
set numberwidth=5

" Disable folding
set nofoldenable

" Don't discard buffer when switching away
set hidden

" Ctrl + j = next buffer
nnoremap <C-j> :bnext<CR>

" Ctrl + k = previous buffer
nnoremap <C-k> :bprevious<CR>

" Ctrl + l = close buffer
nnoremap <C-l> :bdelete<CR>


"-- Ctrl+P -------------------------------------------------------------

nnoremap <silent> <Leader>t :CtrlP<CR>
nnoremap <silent> <Leader>b :CtrlPBuffer<CR>
nnoremap <silent> <Leader>r :CtrlPClearCache<CR>

let g:ctrlp_root_markers=['*.cabal', 'sbt', 'build.sbt', 'pom.xml']


"-- Wildcard config for file listing / completion ----------------------

" bash-style file completion
set wildmode=list:longest

set wildignore=_*/*
set wildignore+=*.exe,.dll
set wildignore+=*.gif,*.jpg,*.png
set wildignore+=*.o,*.hi,*.p_o,*.p_hi,*.pyc
set wildignore+=*.ibc
set wildignore+=*.class,*.jar
set wildignore+=*/dist/*
set wildignore+=*/output/*
set wildignore+=*/bower_components/*
set wildignore+=*/node_modules/*
set wildignore+=*/projects/*
set wildignore+=*/elm-stuff/*

" SBT
set wildignore+=*/project/target/*
set wildignore+=*/target/streams/*
set wildignore+=*/target/resolution-cache/*
set wildignore+=*/target/scala-*/classes/*
set wildignore+=*/target/specs2-reports/*
set wildignore+=*/target/test-reports/*

" Maven
set wildignore+=*/target/maven-*/*


"-- Terminal Mode ------------------------------------------------------

" Leader + e = exit terminal mode
tnoremap <Leader>e <C-\><C-n>


"-- Easy Align ---------------------------------------------------------

" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)


"-- Haskell ------------------------------------------------------------

"au BufNewFile,BufRead *.dump-cmm set filetype=c
"au BufNewFile,BufRead *.hs,*.hsc,*.lhs,*.dump-simpl set filetype=haskell
"au BufNewFile,BufRead *.lhs set syntax=lhaskell
au FileType haskell setlocal iskeyword+='
au FileType haskell setlocal tabstop=4
au FileType haskell setlocal shiftwidth=4
au FileType haskell setlocal path=src,,
au FileType haskell setlocal include=^import\\s*\\(qualified\\)\\?\\s*
au FileType haskell setlocal includeexpr=substitute(v:fname,'\\.','/','g').'.hs'
"au FileType haskell call EnableWhitespace('et')

let hs_highlight_types = 1
let hs_highlight_boolean = 1
let hs_highlight_debug = 1
let hs_allow_hash_operator = 1
