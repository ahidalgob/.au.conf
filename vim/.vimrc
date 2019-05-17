" TODO
" Different cursors for different modes
" Make only one config file for both vim and nvim

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
" revise {{{3
" easy motion
"Plugin 'easymotion/vim-easymotion'

"
"Plugin 'tpope/vim-surround'

"
"Plugin 'tpope/vim-unimpaired'

" easy lining up text
"Plugin 'godlygeek/tabular'
"}}}

" deoplete and dependencies for vim
Plugin 'Shougo/deoplete.nvim'

" deoplete {{{2
if has('nvim')
    let g:deoplete#enable_at_startup = 1
else
    plugin 'roxma/nvim-yarp'
    Plugin 'roxma/vim-hug-neovim-rpc'
    " deoplete conflicts with ttymouse setting for vim, this *hackily* fixes
    autocmd VimEnter * call deoplete#enable()
endif

" syntax and indentation support
Plugin 'sheerun/vim-polyglot'

" linting
Plugin 'w0rp/ale'

" easy visual select
Plugin 'terryma/vim-expand-region'

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

" easy commenting lines
Plugin 'scrooloose/nerdcommenter'

" indent levels guides
Plugin 'nathanaelkane/vim-indent-guides'

Plugin 'danilo-augusto/vim-afterglow'

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
filetype on
filetype plugin indent on " Load indent and plugin files for filetype
set confirm               " Enable dialogs instead of annoying errors
set autoread              " When file changes outside of vim
set history=1500         " REMEMBER
set lazyredraw            " Don't redraw on macros!
set ttyfast               " Batch send characters to screen (way faster)
set clipboard=unnamedplus

set foldmethod=marker

set textwidth=80

set wildmenu

au BufRead,BufNewFile *.x set filetype=haskell
au BufRead,BufNewFile *.y set filetype=haskell

" Split direction for windows:
set splitbelow
set splitright

" Appearance {{{2
:colorscheme afterglow
set colorcolumn=80
syntax enable

" italicized comments
highlight Comment        cterm=italic

set number
:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
:  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
:augroup END

" Set cursorline only in active window
augroup CursorLineOnlyInActiveWindow
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

 "Indention {{{2
set autoindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" Search {{{2
set hlsearch
set incsearch
set ignorecase


" Mappings {{{2

inoremap jk <Esc>
inoremap kj <Esc>

inoremap ()<c-h> ()…<Esc>F)i
inoremap []<c-h> []…<Esc>F]i
inoremap {}<c-h> {}…<Esc>F}i
inoremap <><c-h> <>…<Esc>F>i
inoremap ""<c-h> ""…<Esc>F"i

inoremap <c-Space> <Esc>/…<CR>:noh<CR>"_c1l
" In nomral mode C-Space sends ^@ (Nul)
nnoremap <Nul> /…<CR>:noh<CR>"_c1l

inoremap {<CR>  {<CR>}<Esc>O

nnoremap j gj
nnoremap k gk

vnoremap < <gv
vnoremap > >gv

" Search for selected text, forwards or backwards.
nnoremap <silent> <leader>hh :noh<CR>

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
nnoremap <leader>d       : bp\|bd #<CR>

" Mouse {{{2
" Sometimes I use the mouse.
set scrolloff=5
set mouse=a
if !has('nvim')
    " Somethings conflicts with ttymouse setting, specifically deoplete#enable()
    " Note this has to be before than `call deoplete#enable()`
    autocmd VimEnter * set ttymouse=xterm2
endif


" idk {{{2

" TODO this should go to a file specific to gvim (.gvimrc ?)
set guioptions-=T


" Plugins {{{1
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
let NERDTreeIgnore=['\.hi$', '\.o$']



" Better Whitespace {{{2

let g:better_whitespace_enabled=1
let g:strip_whitespace_on_save=1
let g:strip_whitespace_confirm=0



" Ale {{{2

"let g:ale_enable=0
"let g:ale_lint_on_enter = 0
"
map <leader>aa :ALEToggle<cr>


let g:ale_linters = {
\ 'haskell': ['ghc', 'hlint'],
\ 'cpp': ['gcc', 'clang'],
\}

let g:ale_fixers = {
\ 'cpp' : ['clang-format'],
\ 'haskell' : ['hlint'],
\}

let g:ale_haskell_ghc_options = '-W -dynamic'

let g:ale_set_highlights = 0

nmap <leader>ad :ALEDetail<cr>
nmap <leader>an :ALENext<cr>
nmap <leader>ap :ALEPrevious<cr>


