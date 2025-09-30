# jcubic Dotfiles

Bunch of bash customizations that I use on my Linux/Unix systems

## Usage

### Bash

clone the repo

Add below to your ~/.bashrc

```bash
. ~/path/dotfiles/bash/aliases
. ~/path/dotfiles/bash/less
. ~/path/dotfiles/bash/functions
. ~/path/dotfiles/bash/variables

singleline_prompt
```

You can also use multiline prompt:

```bash
multiline_prompt
```

NOTE: the prompt use `git` (developer tool) so if you don't have it instaled, make sure you
modify the prompt before using.

PS: Make sure to read what the code is doing. I'm not responsible if something breaks on your system.

### GNU Emacs

clone the repo and use this to load all the files:

```elisp
(add-to-list 'load-path "~/path/dotfiles/emacs/packages")
(eval-when-compile (require 'cl))
(require 'functions)
(mapc 'load (file-expand-wildcards
             "~/path/dotfiles/emacs/config/*.el"))
```

## License
Copyright (C) [Jakub T. Jankiewicz](https://jakub.jankiewicz.org/)<br/>
Released under [CC-0](https://creativecommons.org/public-domain/cc0/) license
