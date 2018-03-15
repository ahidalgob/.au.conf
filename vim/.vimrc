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

let g:ycm_extra_conf_globlist = ['~/MEGA/Competitive_Programming/.ycm_extra_conf.py']
set completeopt-=preview


let g:better_whitespace_enabled=1
let g:strip_whitespace_on_save=1


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

autocmd filetype haskell set tabstop=2
autocmd filetype haskell set softtabstop=2
autocmd filetype haskell set shiftwidth=2


:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
:  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
:augroup END

let g:haskell_indent_disable = 1

inoremap {<CR>  {<CR>}<C-c>O
