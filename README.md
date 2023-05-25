# circom-lsp
A language server protocol implementation for Circom

## Installation

Installation through Cargo:
```
cargo install circom-lsp
```

There is also an accommodating VSCode extension under the name: `circom-lsp`

Code for neovim + [https://github.com/VonHeikemen/lsp-zero.nvim](lsp-zero) setup:

```lua
local lsp = require('lsp-zero').preset('recommended')

lsp.new_server({
    name = 'circom-lsp',
    cmd = { 'circom-lsp' },
    filetypes = { 'circom' },
    root_dir = function()
        return lsp.dir.find_first({ 'package.json' }) or vim.api.nvim_buf_get_name(0)
    end,
})
```

## Features

- Diagnostics
- Hover
- Go To Definition
