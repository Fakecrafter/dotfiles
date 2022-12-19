let g:mapleader = "\<Space>"

syntax on                           " Enables syntax highlighing
filetype plugin indent on

set hidden                              " Required to keep multiple buffers open multiple buffers
set nowrap                              " Display long lines as just one line
set encoding=utf-8                      " The encoding displayed
set pumheight=10                        " Makes popup menu smaller
set fileencoding=utf-8                  " The encoding written to file
"set ruler              			            " Show the cursor position all the time
set cmdheight=1                         " More space for displaying messages
set mouse=a                             " Enable your mouse
set nohlsearch                          " no highlight search
set splitright                          " Vertical splits will automatically be to the right
set splitbelow                          " Horizontal splits will automatically be below
set tabstop=4                           " Insert 4 spaces for a tab
set shiftwidth=4                        " Change the number of space characters inserted for indentation
set smarttab                            " Makes tabbing smarter will realize you have 2 vs 4
set expandtab                           " Converts tabs to spaces
set scrolloff=8
set ignorecase
set smartcase
set autoindent                          " Good auto indent
set smartindent                         " Makes indenting smart
set number relativenumber               " Line numbers
set cursorline                          " Enable highlighting of the current line
set showtabline=2                       " Always show tabs
set noshowmode                          " We don't need to see things like -- INSERT -- anymore
set nobackup                            " This is recommended by coc
set nowritebackup                       " This is recommended by coc
set noswapfile
set updatetime=300                      " Faster completion
set timeoutlen=500                      " By default timeoutlen is 1000 ms
set formatoptions-=cro                  " Stop newline continution of comments
set clipboard=unnamedplus               " Copy paste between vim and everything else
set undodir=~/.vim/undodir
set undofile
set autochdir                           " Your working directory will always be the same as your working directory
set completeopt=menuone,noinsert,noselect
" Avoid showing extra messages when using completion
set shortmess+=c


autocmd BufNewFile,BufRead *.rs set filetype=rust



" UltiSnips Settings
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Dashboard settings
let g:dashboard_default_executive ='telescope'

" Lightline Settings
let g:lightline = {
      \ 'colorscheme': 'gruvbox',
      \ }

