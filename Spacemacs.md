## Spacemacs key bindings

### Help

Key&nbsp;Binding   | Description
---------  | --------------
`C-x`      | _(meta)_ Help menu
`C-h`      | _(meta)_ Various options
`C-h m`    | Describe current major mode
`SPC ?`    | Search key major mode bindings
`SPC F1`   | Search command definitions
`SPC h d`  | _(meta)_ Descriptions
`SPC h T e`| Emacs tutorial
`SPC h T v`| Vi tutorial

### Vim

Key&nbsp;Binding   | Description
---------  | --------------
`[`, `]` | _(meta)_ Vim help menu


### Emacs

Key&nbsp;Binding   | Description
---------   | --------------
`M-f`, `M-b` | Word foreword/backward
`C-n`, `C-p` | Next/Previous line
`M-v`, `C-v` | Page up/down
`C-x C-t`    | Transpose lines
`C-s`, `C-r` | Search foreword/backward
`C-g g`      | Go to line
`C-x (`      | Start macro
`C-x )`      | End macro
`M-\`        | Delete all white spaces between

#### Windows

Key&nbsp;Binding   | Description
---------   | --------------
`SPC w 4`   | Windows split
`SPC w m`   | Toggle many to one windows
`M-1, 2..9` | Jump window
`SPC w W`   | Ace jump window
`SPC w H`   | Move window far left

#### Jumps

Key&nbsp;Binding   | Description
---------   | --------------
`SPC j l`   | Jump to line
`SPC j w`   | Jump to word
`SPC j D`   | Jump to file directory

#### Rings

Key&nbsp;Binding   | Description
---------   | --------------
`SPC r`     | _(meta)_ Rings
`SPC r y`   | Show killing ring

#### Files

Key&nbsp;Binding   | Description
---------   | --------------
`SPC f f`   | Find file relative from current dir
`SPC f E`   | **sudo** edit
`SPC f j`   | Jump from dir
`SPC f S`   | Save all files
`SPC f r`   | Show recent files
`SPC f t`   | Toggle `treemax` explorer
`SPC q s`   | Save all files and exit

### Projects

Key&nbsp;Binding   | Description
---------   | --------------
`SPC p p`   | Switch/Import projects
`SPC /`     | Search project using `ag`
`SPC *`     | Search _current_ word in project
`SPC p f`   | Find project file
`SPC p b`   | Show open project buffers
`SPC p !`   | Open shell in project root dir
`M-0`       | Toggle project `treemax`
`SPC p &`   | Async shell command run (relative to project root)
`SPC p k`   | Kill all project buffers
`SPC p r`   | Recent project files
`SPC p R`   | Project search and replace

### Auto Complete

Key&nbsp;Binding   | Description
--------- | --------------
`M-d`   | (in menu) Show documentation
`C-/`   | (in menu) Show Helm for fuzzy search

### REPL

Key&nbsp;Binding   | Description
---------   | --------------
`C-c C-p`   | Open REPL
`C-c C-c`   | Send file
`C-c C-e`   | Send statement
`C-c C-z`   | Go to REPL

### Development

Key&nbsp;Binding   | Description
---------   | --------------
`C-c`       | _(meta)_ Various major mode options
`SPC m h`   | _(meta)_ Documentation
`SPC c`     | _(meta)_ Compilation
`SPC c c`   | Compile project
`SPC d`     | _(meta)_ Debug commands
`SPC a u`   | Visual undo tree
`SPC e`     | _(meta)_ Errors
`SPC e l`   | Toggle errors list
`C-c C-s`   | Start REPL
`C-c m r`   | LSP refactor
`SPC j (`   | Check parents
`SPC j c`   | Go to last change
`SPC s s`   | Helm `swoop`
`SPC s o`   | Overlay symbol
`SPC j 0`   | Add mark
`SPC r m`   | Show all marks
`SPC j j`   | `avy-timer` (type and move to)
`SPC x w d` | Meaning of the word at point
`SPC ;`     | Comment region
`SPC m S e` | Add Sphinx doc to the method
`SPC m g/G` | _(meta)_ LSP **go to** commands
`SPC g`     | _(meta)_ (ma)Git commands
`SPC g s`   | Git status (type '?' for a next Git operation)
`C-c >`     | Indent right selected region

#### SQL

Key&nbsp;Binding   | Description
---------   | --------------
`SPC m b c` | Open SQLi
`SPC m b S` | (in SQLi) Save connection
`SPC m s L` | Send line
`M-TAB`     | Complete

### Misc

Key&nbsp;Binding   | Description
---------   | --------------
`SPC S a`   | Add word at point in dictionary
`SPC C-v m` | Rectangle mark
`SPC f e R` | Reload `.spacemacs`

### Major mode bindings

Key&nbsp;Binding   | Description
--------- | --------------
`SPC m`   | _(meta)_ Main major modes
