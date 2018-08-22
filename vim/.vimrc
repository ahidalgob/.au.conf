" Sets some variables specific to each machine
:source ~/.vim/local_specific.vim

" Vundle {{{1
" Vundle Internal {{{2
set nocompatible              " be iMproved, required
filetype off                  " required <<========== We can turn it on later

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Plugins {{{2

if light==0
    Plugin 'Valloric/YouCompleteMe'
endif

" syntax and indentation support
Plugin 'sheerun/vim-polyglot'

" linting
Plugin 'w0rp/ale'

" sublime-like multiple cursors
Plugin 'terryma/vim-multiple-cursors'

" easy motion
Plugin 'easymotion/vim-easymotion'

" graphic undo tree
Plugin 'sjl/gundo.vim'

" easy visual select
Plugin 'terryma/vim-expand-region'

"
Plugin 'tpope/vim-surround'

"
Plugin 'tpope/vim-unimpaired'

" file system explorer
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'

" git magic
Plugin 'airblade/vim-gitgutter'

" visual representation of marks
Plugin 'kshenoy/vim-signature'

" improves . command on some plugins actions
Plugin 'tpope/vim-repeat'

" navigate seamlessly between vim and tmux splits
Plugin 'christoomey/vim-tmux-navigator'

" airline
Plugin 'bling/vim-airline'

" generates tmux airline-like line
Plugin 'edkolev/tmuxline.vim'

" tabs-like list of buffers, integrates nicely with airline
Plugin 'bling/vim-bufferline'

" easy lining up text
Plugin 'godlygeek/tabular'

" easy commenting lines
Plugin 'scrooloose/nerdcommenter'

" indent levels guides
Plugin 'nathanaelkane/vim-indent-guides'

" distraction-free writing
Plugin 'junegunn/goyo.vim'
Plugin 'junegunn/limelight.vim'


" Plugin 'rafi/awesome-vim-colorschemes'
Plugin 'danilo-augusto/vim-afterglow'
"Plugin 'altercation/vim-colors-solarized'
"Plugin 'chriskempson/tomorrow-theme'

Plugin 'ntpeters/vim-better-whitespace'

" Vundle Internal {{{2
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
" Put the rest of your .vimrc file here



" General {{{1
" idk {{{2

filetype on
filetype plugin indent on " Load indent and plugin files for filetype
set autoread              " When file changes outside of vim
set confirm               " Enable dialogs instead of annoying errors
set autoread              " When file changes outside of vim
syntax enable
set number relativenumber
set history=1500         " REMEMBER
set lazyredraw            " Don't redraw on macros!
set ttyfast               " Batch send characters to screen (way faster)
set clipboard=unnamedplus
set mouse=a

set foldmethod=marker

set scrolloff=5

:colorscheme afterglow


" TODO
" Different cursors for different modes. Tmux-compatible
"if empty($TMUX)
  "let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  "let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  "let &t_SR = "\<Esc>]50;CursorShape=2\x7"
"else
  "let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  "let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  "let &t_SR = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=2\x7\<Esc>\\"
"endif



set autoindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set number
set expandtab
set hlsearch
set incsearch

nnoremap <silent> <leader>hh :noh<CR>

:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
:  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
:augroup END

" Mappings {{{2

inoremap (_) ()…<Esc>F)i
inoremap {<bar>} {}…<Esc>F}i
inoremap [\] []…<Esc>F]i

" Not very aesthetic, works with my urxvt maps
inoremap (_^_) ()…<Esc>F)i
inoremap [_^_] []…<Esc>F]i
inoremap {_^_} {}…<Esc>F}i
inoremap <_^_> <>…<Esc>F>i
inoremap "_^_" ""…<Esc>F"i

inoremap <c-Space> <Esc>/…<CR>:noh<CR>"_c1l
nnoremap <c-Space> /…<CR>:noh<CR>"_c1l

" On some computers (or terminal?) C-Space sends ^@ (Nul)
inoremap <Nul> <Esc>/…<CR>:noh<CR>"_c1l
nnoremap <Nul> /…<CR>:noh<CR>"_c1l

inoremap {<CR>  {<CR>}<Esc>O

nnoremap j gj
nnoremap k gk



inoremap jk <Esc>
inoremap kj <Esc>


" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>


" Buffers {{{2

set hidden        " We can leave a buffer with unsaved changes
set nostartofline " Saves cursor position

" Lists all buffers
nnoremap <leader>l       : buffers<CR>

" Previous buffer
nnoremap <leader><S-Tab> : bprevious<CR>

" Next buffer
nnoremap <leader><Tab>   : bnext<CR>

" Delete current buffer
nnoremap <leader>d       : bdelete<CR>



" idk {{{2



" TODO this should go to a file specific to gvim (.gvimrc ?)
set guioptions-=T
if small
    set guifont=Monospace\ 12
endif



autocmd filetype haskell set tabstop=2
autocmd filetype haskell set shiftwidth=0 " Follow 'tabstop'
autocmd filetype haskell set softtabstop=-1 " Follow 'shiftwidth'


au BufRead,BufNewFile *.x set filetype=haskell
au BufRead,BufNewFile *.y set filetype=haskell



" Plugins {{{1
" Multiple Cursors {{{2
" Called once right before you start selecting multiple cursors
function! Multiple_cursors_before()
    let s:previous_folding=&foldmethod
    set foldmethod=manual

    let s:old_ycm_whitelist = g:ycm_filetype_whitelist
    let g:ycm_filetype_whitelist = {}
endfunction

" Called once only when the multiple selection is canceled
function! Multiple_cursors_after()
    let &foldmethod = s:previous_folding

    let g:ycm_filetype_whitelist = s:old_ycm_whitelist
endfunction

" Airline {{{2


let g:airline_mode_map = {
            \ '__' : '-',
            \ 'n'  : 'N',
            \ 'i'  : 'I',
            \ 'R'  : 'R',
            \ 'c'  : 'C',
            \ 'v'  : 'V',
            \ 'V'  : 'V',
            \ '' : 'V',
            \ 's'  : 'S',
            \ 'S'  : 'S',
            \ '' : 'S',
            \ 't'  : 'T',
            \ }

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.maxlinenr = ''

" let g:airline#extensions#default#section_truncate_width = {}

let g:airline#parts#ffenc#skip_expected_string='utf-8[unix]'


let g:airline#extensions#tabline#enabled = 1            " enable tabline
let g:airline#extensions#tabline#tab_min_count = 2      " show tabline when there's more than 1 tab
let g:airline#extensions#tabline#show_buffers = 0       " disable show buffers when there's only one tab (stronger that the latter)

let g:airline#extensions#bufferline#enabled = 1                 " enable bufferline
let g:airline#extensions#bufferline#overwrite_variables = 1     " pretty colors

" disable bufferline, just use it as an extension for airline
let g:bufferline_echo = 0

" Tmuxline {{{2

" Don't use powerline separators
let g:tmuxline_powerline_separators = 0

let g:tmuxline_preset = {
      \'a'    : '#S',
      \'win'  : '#I #W',
      \'cwin' : '#I #W #F',
      \'z'    : '#H'}

" Tmux Navigator {{{2

" Disable tmux navigator when zooming the Vim pane
let g:tmux_navigator_disable_when_zoomed = 1



" Indent Guides {{{2

"let g:indent_guides_enable_on_vim_startup = 1

" NERDTree {{{2

map <leader>n :NERDTreeToggle<CR>



" YouCompleteMe {{{2
if light==0
    " let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
    let g:ycm_global_ycm_extra_conf = '~/.au_conf/vim/ycm_global_ycm_extra_conf.py'
    set completeopt-=preview
endif

nnoremap <leader>fi :YcmCompleter FixIt<CR>

" Better Whitespace {{{2

let g:better_whitespace_enabled=1
let g:strip_whitespace_on_save=1



" Ale {{{2

" still not configured
let g:ale_enable=0
map <leader>aa :ALEToggle<cr>
