" TODO
" Different cursors for different modes
" Make only one config file for both vim and nvim

" vim-plug {{{1
" Download plug-in {{{2
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" revise {{{2
"Plug 'easymotion/vim-easymotion'
"Plug 'tpope/vim-surround'
"Plug 'tpope/vim-unimpaired'
"Plug 'godlygeek/tabular'
"}}}

if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    let g:deoplete#enable_at_startup = 1
else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
    " deoplete conflicts with ttymouse setting for vim, this *hackily* fixes it
    autocmd VimEnter * call deoplete#enable()
endif

" syntax and indentation support
Plug 'sheerun/vim-polyglot'

Plug 'SirVer/ultisnips'

" linting
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'junegunn/fzf'

Plug 'w0rp/ale'


" easy visual select
Plug 'terryma/vim-expand-region'

" file system explorer
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

" git magic
Plug 'airblade/vim-gitgutter'

" visual representation of marks
Plug 'kshenoy/vim-signature'

" improves . command on some plugins actions
Plug 'tpope/vim-repeat'

" navigate seamlessly between vim and tmux splits
Plug 'christoomey/vim-tmux-navigator'

" airline
Plug 'bling/vim-airline'

" generates tmux airline-like line
Plug 'edkolev/tmuxline.vim'

" tabs-like list of buffers, integrates nicely with airline
Plug 'bling/vim-bufferline'

" easy commenting lines
Plug 'scrooloose/nerdcommenter'

" indent levels guides
Plug 'nathanaelkane/vim-indent-guides'

Plug 'danilo-augusto/vim-afterglow'

Plug 'ntpeters/vim-better-whitespace'

call plug#end()

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

inoremap <c-h> …<Esc>hi

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



" LanguageClient server{{{2
let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper'],
    \ 'cpp': ['ccls'],
    \ }
let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']

" ALE{{{2
let g:ale_linters = {'haskell' : ['ghc']}


" Snips{{{2
let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips"
" vertically split ultisnips edit window
let g:UltiSnipsEditSplit="vertical"
