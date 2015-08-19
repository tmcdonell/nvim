"-- Plugins ------------------------------------------------------------

call plug#begin('~/.nvim/plugged')

" A Git wrapper so awesome, it should be illegal
Plug 'tpope/vim-fugitive'

" Fuzzy file, buffer, mru, tag, etc finder
Plug 'kien/ctrlp.vim'

" Fast vim CtrlP matcher based on python
Plug 'FelikZ/ctrlp-py-matcher'

" A Vim alignment plugin
Plug 'junegunn/vim-easy-align'

" Configurable, flexible, intuitive text aligning
Plug 'godlygeek/tabular'

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

" A plugin for asynchronous :make using Neovim's job-control functionality
Plug 'benekastah/neomake'

" Happy Haskell programming on Vim, powered by ghc-mod
Plug 'eagletmt/ghcmod-vim'
Plug 'Shougo/vimproc.vim'

" Integration of Scala into Vim
Plug 'derekwyatt/vim-scala'

call plug#end()


"-- Colors -------------------------------------------------------------

set background=dark

"colorscheme jellybeans
"colorscheme hybrid_reverse
"colorscheme base16-ocean

" Hybrid Material + Custom
colorscheme hybrid_material
highlight SpecialComment guifg=#75bd84

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

" This must come before any bindings to <Leader>
let mapleader      = ","

" This must come before any bindings to <LocalLeader>
let maplocalleader = ";"


"-- nvimrc -------------------------------------------------------------

" Location of nvimrc
let g:nvimrc = "~/.config/nvim/nvimrc"

" Edit nvimrc
nnoremap <Leader>m :exec ":e " . g:nvimrc<CR>

" Source the nvimrc file after saving it
augroup nvimrc
  autocmd!
  autocmd BufWritePost nvimrc exec "source " . g:nvimrc | AirlineRefresh
augroup END


"-- OS Clipboard -------------------------------------------------------

" Leader + y = copy selection to clipboard
vnoremap <Leader>y "+y

" Leader + p = paste from clipboard
nnoremap <Leader>p "+gpl
vnoremap <Leader>p :<C-U>call VisualPaste()<CR>

function! VisualPaste()
  normal gv
  normal "+P
  normal l
endfunction


"-- Terminal Mode / neoterm --------------------------------------------

" Open neoterm windows in a vertical split
let g:neoterm_position = 'vertical'

" Leader + Esc = exit terminal mode
tnoremap <Leader><Esc> <C-\><C-n>

" Ctrl + w + n = create new window with empty buffer
tnoremap <C-w>n     <C-\><C-n><C-w>n
tnoremap <C-w><C-n> <C-\><C-n><C-w><C-n>

" Ctrl + w + s = split window horizontally
tnoremap <C-w>s     <C-\><C-n><C-w>s
tnoremap <C-w><C-s> <C-\><C-n><C-w><C-s>

" Ctrl + w + v = split window vertically
tnoremap <C-w>v     <C-\><C-n><C-w>v
tnoremap <C-w><C-v> <C-\><C-n><C-w><C-v>

" Ctrl + w + w = move cursor to window below/right of the current one
tnoremap <C-w>w     <C-\><C-n><C-w>w
tnoremap <C-w><C-w> <C-\><C-n><C-w><C-w>

" Ctrl + w + c = close current window
tnoremap <C-w>c <C-\><C-n><C-w>c

augroup Terminal
  autocmd!

  " Always start insert mode when you enter a terminal window
  autocmd BufWinEnter,WinEnter term://* startinsert

  " Always stop insert mode when you leave a terminal window
  autocmd BufLeave term://* stopinsert
augroup END


"-- Buffers ------------------------------------------------------------

if !exists('g:nvimrc_buffers')
  " Only source the first time
  let g:nvimrc_buffers = 1

  " Show ruler
  set ruler

  " Show command in the last line of the screen
  set showcmd

  " Always show status line
  set laststatus=2

  " Show line numbers
  set number
  set numberwidth=5

  " Disable highlighting of search matches
  set nohlsearch

  " Disable folding
  set nofoldenable

  " Don't discard buffer when switching away
  set hidden
endif

" Ctrl + j = next buffer
nnoremap <C-j>           :bnext<CR>
tnoremap <C-j> <C-\><C-n>:bnext<CR>

" Ctrl + k = previous buffer
nnoremap <C-k>           :bprevious<CR>
tnoremap <C-k> <C-\><C-n>:bprevious<CR>

" Ctrl + l = close buffer
nnoremap <C-l>           :bdelete<CR>
nnoremap <C-l> <C-\><C-n>:bdelete<CR>

" (buffer id) + Leader = jump to buffer
let buffer_id = 1
while buffer_id <= 99
  execute "nnoremap " . buffer_id . "<Leader> :" . buffer_id . "b\<CR>"
  let buffer_id += 1
endwhile


"-- Grep ---------------------------------------------------------------

" Grep word under cursor, same file type
nnoremap <Leader>g
  \ :noautocmd lvim /\<lt><C-R><C-W>\>/gj
  \ **/*<C-R>=(expand("%:e")=="" ? "" : ".".expand("%:e"))<CR>
  \ <Bar> lw<CR>

" Grep word under cursor, all files
nnoremap <Leader>G
  \ :noautocmd lvim /\<lt><C-R><C-W>\>/gj
  \ **
  \ <Bar> lw<CR>


"-- Ctrl+P -------------------------------------------------------------

" Leader + t = find file in project
nnoremap <silent> <Leader>t :CtrlP<CR>

" Leader + b = find file in open buffers
nnoremap <silent> <Leader>b :CtrlPBuffer<CR>

" Leader + r = clean file cache
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

" Vim
set wildignore+=*/spell/*
set wildignore+=*/plugged/*

" Elm
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


"-- Fugitive -----------------------------------------------------------

" Leader + gd = diff current vs git head
nnoremap <Leader>d :Gdiff<cr>

" Leader + gD = Switch back to current file and closes Fugitive buffer
nnoremap <Leader>D <c-w>h<c-w>c


"-- Easy Align ---------------------------------------------------------

" vip<Enter> = Start interactive EasyAlign in visual mode
vmap <Enter> <Plug>(EasyAlign)

" gaip = Start interactive EasyAlign for a motion/text object
nmap ga <Plug>(EasyAlign)


"-- Tabular ------------------------------------------------------------

" Align = or += or &= or == or ==>
nnoremap <Leader>a= :Tabularize /[+&]\?=\+[>]\?<CR>
vnoremap <Leader>a= :Tabularize /[+&]\?=\+[>]\?<CR>

" Align -> or <-
nnoremap <Leader>a- :Tabularize /-\+>\\|<-\+<CR>
vnoremap <Leader>a- :Tabularize /-\+>\\|<-\+<CR>


"-- Word manipulation --------------------------------------------------

" Leader + s = replace word under cursor
nnoremap <Leader>s :%s/\<<C-r><C-w>\>/

" gw = Swap current word with next
nnoremap <silent> gw "_yiw:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR><C-o><C-l>


"-- Language Defaults --------------------------------------------------

if !exists('g:nvimrc_language_defaults')
  " Only source the first time
  let g:nvimrc_language_defaults = 1

  " How many columns a tab counts for
  set tabstop=8

  " Tab in insert mode will produce the appropriate number of spaces
  set expandtab

  " How many columns text is indented with the reindent operations
  set shiftwidth=8

  " How many columns vim uses when you hit Tab in insert mode
  set softtabstop=0

  " Prevent backspace from deleting multiple spaces, thinking that it
  " means a <Tab>
  set nosmarttab
endif


"-- Neomake ------------------------------------------------------------

" Leader + e = open location list
nnoremap <Leader>e :lopen<CR>

" Leader + E = close location list
nnoremap <Leader>E :lclose<CR>

function! s:NeomakeExclude()
  let l:path   = expand('%:p:h')
  let l:ignore = expand('^$HOME/.config/nvim')
  if match(l:path, l:ignore)
    Neomake
  endif
endfunction

augroup Neomake
  autocmd!
  autocmd BufWritePost * call s:NeomakeExclude()
augroup END

if !exists('g:nvimrc_neomake')
  " Only source the first time
  let g:nvimrc_neomake = 1

  highlight SpellBad gui=NONE term=NONE
  highlight SpellCap gui=NONE term=NONE
  highlight clear SignColumn

  call neomake#signs#RedefineErrorSign({ 'texthl': 'SpellBad' })
  call neomake#signs#RedefineWarningSign({ 'texthl': 'SpellCap' })
endif

let g:neomake_haskell_enabled_makers  = ['ghcmod']

let g:neomake_haskell_ghcmod_maker = {
    \ 'exe': 'ghc-mod',
    \ 'args': ['check', "-b\n"],
    \ 'errorformat':
      \ '%-Z %#,' .
      \ '%E%f:%l:%c:Error: %m,' .
      \ '%W%f:%l:%c:Warning: %m,'.
      \ '%E%f:%l:%c: Error: %m,' .
      \ '%W%f:%l:%c: Warning: %m,' .
      \ '%E%f:%l:%c:%m,' .
      \ '%m'
    \ }

"-- VimL ---------------------------------------------------------------

augroup VimL
  autocmd!
  autocmd FileType vim setlocal tabstop=2
  autocmd FileType vim setlocal shiftwidth=2
  autocmd FileType vim setlocal textwidth=72
augroup END


"-- LLVM ---------------------------------------------------------------

augroup LLVM
  autocmd!
  autocmd BufNewFile,BufRead *.ll set filetype=llvm
  autocmd BufNewFile,BufRead *.td set filetype=tablegen
  autocmd FileType llvm set tabstop=2
  autocmd FileType llvm set shiftwidth=2
augroup END


"-- DDC ----------------------------------------------------------------

augroup DDC
  autocmd!
  autocmd BufNewFile,BufRead *.ds  set filetype=ddc " Source
  autocmd BufNewFile,BufRead *.dst set filetype=ddc " Source
  autocmd BufNewFile,BufRead *.dsx set filetype=ddc " Source
  autocmd BufNewFile,BufRead *.dcx set filetype=ddc " Core
  autocmd BufNewFile,BufRead *.dcl set filetype=ddc " Core
  autocmd BufNewFile,BufRead *.dcs set filetype=ddc " Core
  autocmd BufNewFile,BufRead *.dce set filetype=ddc " Core
  autocmd BufNewFile,BufRead *.dct set filetype=ddc " Core
  autocmd FileType ddc setlocal tabstop=8
  autocmd FileType ddc setlocal shiftwidth=8
augroup END


"-- Haskell ------------------------------------------------------------

augroup Haskell
  autocmd!

  "autocmd BufNewFile,BufRead *.dump-cmm set filetype=c
  "autocmd BufNewFile,BufRead *.hs,*.hsc,*.lhs,*.dump-simpl set filetype=haskell
  "autocmd BufNewFile,BufRead *.lhs set syntax=lhaskell
  autocmd FileType haskell setlocal iskeyword+='
  autocmd FileType haskell setlocal tabstop=4
  autocmd FileType haskell setlocal shiftwidth=4
  autocmd FileType haskell setlocal path=src,,
  autocmd FileType haskell setlocal include=^import\\s*\\(qualified\\)\\?\\s*
  autocmd FileType haskell setlocal includeexpr=substitute(v:fname,'\\.','/','g').'.hs'

  " Sort then align imports
  autocmd FileType haskell nnoremap <Leader>ai
    \ vip :sort r /\u.*/<CR> <Bar> :Tabularize /^import qualified\\|^import\\|^$<CR>
augroup END


"-- nvim-hs ------------------------------------------------------------

function! s:RequireHaskellHost(name)
  "return rpcstart('nvim-hs-devel.sh', [a:name.name])
  return rpcstart('nvim-hs-devel.sh', ['-l','/tmp/nvim-hs.log','-v','DEBUG',a:name.name])
endfunction

" Lazily register 'nvim-hs' as the 'haskell' plugin host
call remote#host#Register('haskell', "*.[cl]\?hs", function('s:RequireHaskellHost'))

" Block until nvim-hs has started
function! StartHaskellHost()
  let l:channel = remote#host#Require('haskell')
  call rpcrequest(l:channel, "PingNvimhs")
endfunction

"call StartHaskellHost()
