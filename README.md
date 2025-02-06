# dotfiles
dotfiles for rails developer using zsh/emacs

- install homebrew
  ```
  % /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  ```
- clone this repo and
- brew bundle
  ```
  % cd brew
  % brew bundle
  ```
- chsh to brew version of zsh
  - add `/opt/homebrew/bin/zsh` to `/etc/shells`
  - chsh
    ```
    % chsh /opt/homebrew/bin/zsh
    ```
- stow
  ```
  % stow -t ~ -v --dotfiles brew emacs gh git karabiner raycast readline ruby zsh
  ```
- install gem-src
  ```
  % git clone https://github.com/amatsuda/gem-src.git "$(rbenv root)/plugins/gem-src"
  ```
