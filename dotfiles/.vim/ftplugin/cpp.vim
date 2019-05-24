
nnoremap <leader>g++ :w <bar> !g++ -Wall -DLOCAL -std=c++11 % -o %:r <CR>
nnoremap <leader>ri :!./%:r < in<CR>
nnoremap <leader>rc :!./%:r<CR>
nnoremap <leader>rr :!xclip -o > clipin && ./%:r < clipin <CR>
set noexpandtab

set foldmethod=syntax
