" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  "autocmd VimEnter * PlugInstall
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/plugged')

" Repeat stuff
Plug 'tpope/vim-repeat'
" Surround
Plug 'tpope/vim-surround'
" Better Comments
Plug 'preservim/nerdcommenter'

" Text Navigation
Plug 'terryma/vim-expand-region'
" Plug 'justinmk/vim-sneak'
" Plug 'unblevable/quick-scope'
" Plug 'tpope/vim-unimpaired'
" Plug 'easymotion/vim-easymotion'
" Plug 'godlygeek/tabular'

" visual representation of marks
" Plug 'kshenoy/vim-signature'

" Add some color
Plug 'norcalli/nvim-colorizer.lua'
Plug 'junegunn/rainbow_parentheses.vim'
"
" Better Syntax Support
Plug 'sheerun/vim-polyglot'
" Cool Icons
Plug 'ryanoasis/vim-devicons'

"  " Auto pairs for '(' '[' '{'
"  Plug 'jiangmiao/auto-pairs'
"  " Closetags
"  Plug 'alvan/vim-closetag'

" Themes
Plug 'arcticicestudio/nord-vim'
Plug 'tomasiser/vim-code-dark'
Plug 'NLKNguyen/papercolor-theme'
" Plug 'christianchiarulli/onedark.vim'


" Intellisense
Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'dense-analysis/ale'

" Trailing whitespace
Plug 'ntpeters/vim-better-whitespace'

" Status Line
Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline-themes'

" Ranger
" Plug 'francoiscabrol/ranger.vim'
" Plug 'rbgrouleff/bclose.vim'
" Plug 'kevinhwang91/rnvimr', {'do': 'make sync'}

" FZF
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Git
" Plug 'mhinz/vim-signify'
Plug 'airblade/vim-gitgutter'
" Plug 'tpope/vim-fugitive'
" Plug 'tpope/vim-rhubarb'
" Plug 'junegunn/gv.vim'

" Terminal
" Plug 'voldikss/vim-floaterm'

" Start Screen
" Plug 'mhinz/vim-startify'
"
" Vista
" Plug 'liuchengxu/vista.vim'
"
" See what keys do like in emacs
" Plug 'liuchengxu/vim-which-key'

" Zen mode
" Plug 'junegunn/goyo.vim'

" Making stuff
" Plug 'neomake/neomake'

" Snippets
Plug 'honza/vim-snippets'
" Plug 'mattn/emmet-vim'

" Better Comments
" Plug 'jbgutierrez/vim-better-comments'
" Echo doc
" Plug 'Shougo/echodoc.vim'

" Interactive code
" Plug 'ChristianChiarulli/codi.vim'

"  endif
"
"
call plug#end()
