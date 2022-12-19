" auto-install vim-plug

call plug#begin('~/.local/share/nvim/site/autoload/')


    " Colors
    Plug 'itchyny/lightline.vim'
    Plug 'gruvbox-community/gruvbox'
    Plug 'arcticicestudio/nord-vim'

    " Telescope
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'tpope/vim-commentary'
    Plug 'nvim-telescope/telescope.nvim'

    " Snippets
    Plug 'SirVer/ultisnips'
    Plug 'honza/vim-snippets'
    " Lsp
    " Extensions to built-in LSP, for example, providing type inlay hints
    " Plug 'neovim/nvim-lspconfig'
    " Plug 'hrsh7th/nvim-compe'
    " Plug 'nvim-lua/lsp_extensions.nvim'

    " Rust
    Plug 'rust-lang/rust.vim'

    " some handy stuff
    Plug 'airblade/vim-rooter'
    Plug 'rstacruz/vim-closer'
    Plug 'vifm/vifm.vim'
    Plug 'justinmk/vim-sneak'
    Plug 'glepnir/dashboard-nvim'

call plug#end()

