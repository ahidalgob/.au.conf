set nocompatible              " be iMproved, required
filetype off                  " required <<========== We can turn it on later

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Go into vim and do :PluginInstall
Plugin 'itchyny/vim-haskell-indent'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'rafi/awesome-vim-colorschemes'
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'godlygeek/tabular'
Plugin 'scrooloose/nerdtree'

" Clone the repo into bundle
" then run git submodule update --init --recursive
Plugin 'Valloric/YouCompleteMe'


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

" let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
" let g:ycm_global_ycm_extra_conf = '~/.au_conf/ycm_global_ycm_extra_conf.py'
"  :(

let small = 1

set completeopt-=preview


let g:better_whitespace_enabled=1
let g:strip_whitespace_on_save=1


map <C-n> :NERDTreeToggle<CR>


autocmd filetype cpp nnoremap <leader>b :w <bar> !g++ -Wall -DLOCAL -std=c++11 % -o %:r <CR>
autocmd filetype cpp nnoremap <leader>ri :!./%:r < in<CR>
autocmd filetype cpp nnoremap <leader>rc :!./%:r<CR>
autocmd filetype cpp nnoremap <leader>rr :!xclip -o > clipin && ./%:r < clipin <CR>

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
nnoremap <leader>h :noh<CR>
:set number relativenumber
:colorscheme minimalist

:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
:  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
:augroup END

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
