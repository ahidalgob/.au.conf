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

" Run > vim +PluginInstall

if light==0
    Plugin 'Valloric/YouCompleteMe'
endif

" syntax and indentation support
Plugin 'sheerun/vim-polyglot'

" Plugin 'w0rp/ale'

" graphic undo tree
Plugin 'sjl/gundo.vim'

" file system explorer
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'

" git magic
Plugin 'airblade/vim-gitgutter'

" improves . command on some plugins actions
Plugin 'tpope/vim-repeat'

" airline
Plugin 'bling/vim-airline'

" tabs-like list of buffers
" what's the advantage over just airline?
Plugin 'bling/vim-bufferline'

" easy lining up text
Plugin 'godlygeek/tabular'

" indent levels
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
if light==0
    " let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
    let g:ycm_global_ycm_extra_conf = '~/.au_conf/ycm_global_ycm_extra_conf.py'
    set completeopt-=preview
endif


let g:better_whitespace_enabled=1
let g:strip_whitespace_on_save=1




let g:airline#extensions#bufferline#enabled = 1
let g:airline#extensions#bufferline#overwrite_variables = 1

let g:bufferline_echo = 0

"let g:indent_guides_enable_on_vim_startup = 1


map <C-n> :NERDTreeToggle<CR>

filetype on
filetype indent on
syntax enable
set tabstop=4
set softtabstop=4
set shiftwidth=4
set number
set expandtab
set hlsearch
set incsearch
set autoread              " When file changes outside of vim
nnoremap <leader>h :noh<CR>
:set number relativenumber
:colorscheme afterglow

:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
:  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
:augroup END

" TODO this should go to a file specific to gvim (.gvimrc ?)
set guioptions-=T
if small
    set guifont=Monospace\ 12
endif

set clipboard=unnamedplus

autocmd filetype haskell set tabstop=2
autocmd filetype haskell set shiftwidth=0 " Follow 'tabstop'
autocmd filetype haskell set softtabstop=-1 " Follow 'shiftwidth'

let g:haskell_indent_disable = 1
au BufRead,BufNewFile *.x set filetype=haskell
au BufRead,BufNewFile *.y set filetype=haskell


inoremap <c-l> <Right>
inoremap <c-h> <Left>
inoremap <c-j> <Down>
inoremap <c-k> <Up>

inoremap (_) ()<++><Esc>F)i
inoremap {<bar>} {}<++><Esc>F}i
inoremap [\] []<++><Esc>F]i
inoremap <c-Space> <Esc>/<++><CR>:noh<CR>"_cf>

inoremap {<CR>  {<CR>}<C-c>O

nnoremap j gj
nnoremap k gk

set foldmethod=marker

set scrolloff=5

"##############################
"######################Buffers
"##############################
" We can leave a buffer with unsaved changes
set hidden

" Saves cursor position
set nostartofline

" Lists all buffers
nnoremap <leader>l       : buffers<CR>

" Previous buffer
nnoremap <leader><S-Tab> : bprevious<CR>

" Next buffer
nnoremap <leader><Tab>   : bnext<CR>

" Delete current buffer
nnoremap <leader>d       : bdelete<CR>

:let g:buftabs_only_basename=1
