# Dotfiles Reorganization Plan

## Steps:
- [ ] Clone dotfiles repo to get keyboard and mako configs
- [ ] Create archive directory 
- [ ] Move kitty and nvim to archive directory
- [ ] Update home.nix to include keyboard, mako, archive/kitty, archive/nvim
- [ ] Format and commit changes
- [ ] Push changes to git

## Directory Structure:
```
~/.dotfiles/.config/
├── keyboard/          (from repo)
├── mako/             (new from repo)
├── archive/
│   ├── kitty/
│   └── nvim/
└── [existing configs...]
