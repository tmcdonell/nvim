"-- Plugins ------------------------------------------------------------

call plug#begin('~/.nvim/plugged')

" Defaults everyone can agree on
Plug 'tpope/vim-sensible'

" A Git wrapper so awesome, it should be illegal
Plug 'tpope/vim-fugitive'

" Fuzzy file, buffer, mru, tag, etc finder
Plug 'kien/ctrlp.vim'

" Fast vim CtrlP matcher based on python
Plug 'FelikZ/ctrlp-py-matcher'

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

" Pandoc integration and utilities for vim
Plug 'vim-pandoc/vim-pandoc'

" Pandoc markdown syntax, to be installed alongside vim-pandoc
Plug 'vim-pandoc/vim-pandoc-syntax'

" A vim mode for Haskell (forked from travitch/hasksyn)
"Plug '~/src/hasksyn'

" Integration of Scala into Vim
Plug 'derekwyatt/vim-scala'

call plug#end()


"-- Colors -------------------------------------------------------------

set background=dark

"colorscheme jellybeans
colorscheme hybrid_material
"colorscheme hybrid_reverse
"colorscheme base16-ocean

" Matches iTerm2 Colors
let g:terminal_color_0  = '#808080'
let g:terminal_color_1  = '#da4f56'
let g:terminal_color_2  = '#78af54'
let g:terminal_color_3  = '#fa8e43'
let g:terminal_color_4  = '#6197d4'
let g:terminal_color_5  = '#cf9af8'
let g:terminal_color_6  = '#50c283'
let g:terminal_color_7  = '#d6d6c6'
let g:terminal_color_8  = '#bdbdbd'
let g:terminal_color_9  = '#fc767c'
let g:terminal_color_10 = '#95da72'
let g:terminal_color_11 = '#fecc7a'
let g:terminal_color_12 = '#8ebaf6'
let g:terminal_color_13 = '#f7bdff'
let g:terminal_color_14 = '#67e39f'
let g:terminal_color_15 = '#efeee0'


"-- Airline ------------------------------------------------------------

let g:airline_powerline_fonts                   = 1
let g:airline_theme                             = 'tomorrow'
let g:airline#extensions#tabline#enabled        = 1
let g:airline#extensions#tabline#fnamecollapse  = 0
let g:airline#extensions#tabline#formatter      = 'unique_tail_improved'
let g:airline#extensions#tabline#fnamemod       = ':t'
let g:airline#extensions#tabline#buffer_nr_show = 1


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

" (buffer id) + <Leader> = jump to buffer
let buffer_id = 1
while buffer_id <= 99
  execute "nnoremap " . buffer_id . "<Leader> :" . buffer_id . "b\<CR>"
  let buffer_id += 1
endwhile


"-- Ctrl+P -------------------------------------------------------------

nnoremap <silent> <Leader>t :CtrlP<CR>
nnoremap <silent> <Leader>b :CtrlPBuffer<CR>
nnoremap <silent> <Leader>r :CtrlPClearCache<CR>

let g:ctrlp_root_markers = ['*.cabal', 'sbt', 'build.sbt', 'pom.xml']
let g:ctrlp_match_func   = { 'match': 'pymatcher#PyMatch' }


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
set wildignore+=*/target/resolution-cache/*
set wildignore+=*/target/rpm/*
set wildignore+=*/target/scala-*/api/*
set wildignore+=*/target/scala-*/classes/*
set wildignore+=*/target/specs2-reports/*
set wildignore+=*/target/streams/*
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


"-- Replace word under cursor ------------------------------------------
" Usage: type <Leader>s on "foo" and then type "bar/g" to
"        replace foo with bar

nnoremap <Leader>s :%s/\<<C-r><C-w>\>/


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
