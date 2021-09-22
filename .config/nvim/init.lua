-- require('impatient')
require'plugins'

-- Load .nvimrc manually until this PR is merged.
-- https://github.com/neovim/neovim/pull/13503
local local_vimrc = vim.fn.getcwd()..'/.nvimrc'
if vim.loop.fs_stat(local_vimrc) then
  vim.cmd('source '..local_vimrc)
end
