local plugins = {}
local api = vim.api
vim.cmd [[packadd vim-packager]]

require('packager').setup(function(packager)
    packager.add('kristijanhusak/vim-packager', { type = 'opt' })
    packager.add('lewis6991/impatient.nvim')
end)