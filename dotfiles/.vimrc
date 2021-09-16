set nocompatible
filetype plugin indent on
syntax on

" vim-plug {{{1
" Download plug-in {{{2
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif


call plug#begin('~/.vim/plugged')

" linting
" Plug 'autozimu/LanguageClient-neovim', {
"     \ 'branch': 'next',
"     \ 'do': 'bash install.sh',
"     \ }
" Plug 'junegunn/fzf'

" Plug 'w0rp/ale'

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

Plug 'Yggdroot/indentLine'

" indent levels guides
Plug 'nathanaelkane/vim-indent-guides'

"Plug 'danilo-augusto/vim-afterglow'
Plug 'ahidalgob/vim-afterglow'
Plug 'deviantfero/wpgtk.vim'
Plug 'tomasiser/vim-code-dark'
Plug 'morhetz/gruvbox'

Plug 'chrisbra/Colorizer'

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

set completeopt-=preview

set foldmethod=marker
set nofoldenable

"set textwidth=80 " annoying

set wildmenu

au BufRead,BufNewFile *.x set filetype=haskell
au BufRead,BufNewFile *.y set filetype=haskell

" Split direction for windows:
set splitbelow
set splitright

" Appearance {{{2

" color are nord0_gui and nord12_gui
autocmd ColorScheme gruvbox highlight Todo guifg=#2E3440 guibg=#D08770

colorscheme gruvbox
set bg=dark
hi Comment cterm=italic

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

inoremap {<CR>  {<CR>}<Esc>O

nnoremap j gj
nnoremap k gk

vnoremap < <gv
vnoremap > >gv

nnoremap <silent> <leader>hh :noh<CR>

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

"let g:vimwiki_list = [{'path': '~/vimwiki/',
                      "\ 'syntax': 'markdown', 'ext': '.md'}]


"let g:vimwiki_ext2syntax = {'.md': 'markdown',
                "\ '.mkd': 'markdown',
                "\ '.wiki': 'media'}


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



" LanguageClient server{{{2
let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper'],
    \ 'lhaskell': ['hie-wrapper'],
    \ 'cpp': ['ccls'],
    \ 'python': ['pyls'],
    \ }
" 'python': ['pyls', '-vv', '--log-file', '~/pyls.log'],
"
let g:LanguageClient_rootMarkers = ['stack.yaml']

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <Leader>lb :call LanguageClient#textDocument_references()<CR>
map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

"let g:LanguageClient_diagnosticsEnable = 0

let g:LanguageClient_diagnosticsDisplay = {
    \     1: {
    \         "name": "Error",
    \         "texthl": "howtowork",
    \         "signText": "✖",
    \         "signTexthl": "ErrorMsg",
    \     },
    \     2: {
    \         "name": "Warning",
    \         "texthl": "howtowork",
    \         "signText": "⚠",
    \         "signTexthl": "ALEWarningSign",
    \     },
    \     3: {
    \         "name": "Information",
    \         "texthl": "howtowork",
    \         "signText": "ℹ",
    \         "signTexthl": "ALEInfoSign",
    \     },
    \     4: {
    \         "name": "Hint",
    \         "texthl": "howtowork",
    \         "signText": "➤",
    \         "signTexthl": "ALEInfoSign",
    \     },
    \ }


hi link ALEError Error
hi Warning term=underline cterm=underline ctermfg=Yellow gui=undercurl guisp=Gold
hi link ALEWarning Warning
hi link ALEInfo SpellCap


" ALE{{{2
" 'python' : ['mypy', 'flake8']
let g:ale_linters = {
    \ 'python' : []
    \}

" 'python' : ['yapf']
let g:ale_fixers = {
    \ 'python' : []
    \}

" Snips{{{2
let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips"
" vertically split ultisnips edit window
let g:UltiSnipsEditSplit="vertical"

"let g:colorizer_auto_color = 1
"
