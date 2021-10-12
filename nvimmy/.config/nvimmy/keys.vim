" Keymappings

" Vertically center document when entering insert mode
autocmd InsertEnter * norm zz
"
" Remove trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e

"normal mode commands
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <TAB> :bn!<CR>
nnoremap <S-TAB> :bp!<CR>
nnoremap gb :bn!<CR>
nnoremap gB :bp!<CR>
nnoremap <leader>bd <cmd>bdelete!<CR>
"""Vifm"""
nnoremap <leader>f :Vifm<return>
nnoremap <leader>vs :VsplitVifm<return>
"insert mode commands
inoremap jj <Esc>
inoremap kj <Esc>
inoremap jk <Esc>
noremap! <C-BS> <C-w>
noremap! <C-h> <C-w>

"Telescope
nnoremap <C-f> <cmd>Telescope find_files<CR>

"Terminal
nnoremap <leader>te <cmd>terminal<CR>
tnoremap <C-c> <C-\><C-N>
tnoremap <TAB> <cmd>bn!<CR>
tnoremap <S-TAB> <cmd>bp!<CR>


nnoremap <leader>x <cmd>Cargo run<CR>i

