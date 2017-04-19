# Dotfiles

Here are my configuration files (especially **emacs** !)

# Emacs

Here are the languages, frameworks and technologies that I often use :

* NodeJS / Javascript (ES6)
* PostgreSQL / SQLite / SQL
* Rust
* C
* Angular / React
* Pug (Jade) / Handlebars / HTML
* SASS / LESS / CSS
* Emacs Lisp
* Markdown / JSON
* Git / SVN
* PHP when I have no choice :'(

Syntax highlighting features are available for this languages.

## Shortcuts :

### Window (Buffer) manipulation :

| Shortcut                                                              | Action        |
| --------------------------------------------------------------------- |:-------------:|
| <kbd>CTRL</kbd>+<kbd>x</kbd>  <kbd>CTRL</kbd>+<kbd>&#9654;</kbd>      | Vertically split the screen and let you choose a file to open in the right part of the screen  |
| <kbd>CTRL</kbd>+<kbd>x</kbd>  <kbd>CTRL</kbd>+<kbd>&#9660;</kbd>      | Horizontally split the screen and let you choose a file to open in the lower part of the screen|
| <kbd>CTRL</kbd>+<kbd>x</kbd>  <kbd>CTRL</kbd>+<kbd>x</kbd>            | Kill (exit) the current window, if there are others ones|
| <kbd>ALT</kbd>+(<kbd>SHIFT</kbd>) + <kbd>&#9654;</kbd>                | Move to window right|
| <kbd>ALT</kbd>+(<kbd>SHIFT</kbd>) + <kbd>&#9664;</kbd>                | Move to window left |
| <kbd>ALT</kbd>+(<kbd>SHIFT</kbd>) + <kbd>&#9650;</kbd>                | Move to window top  |
| <kbd>ALT</kbd>+(<kbd>SHIFT</kbd>) + <kbd>&#9660;</kbd>                | Move to window down |
| <kbd>CTRL</kbd>+<kbd>x</kbd>  <kbd>CTRL</kbd>+<kbd>m</kbd>            | Open a menu-bar (*neotree*)|


### Search

| Shortcut                                                              | Action        |
| --------------------------------------------------------------------- |:-------------:|
| <kbd>CTRL</kbd>+<kbd>f</kbd>                                          | Search (*helm-swoop*)|
| <kbd>CTRL</kbd>+<kbd>x</kbd> <kbd>CTRL</kbd>+<kbd>f</kbd>             | Search for a file to load in the current window (*helm-find-file*)|
| <kbd>CTRL</kbd>+<kbd>s</kbd>                                          | Search forward (*phi-search*)|
| <kbd>CTRL</kbd>+<kbd>r</kbd>                                          | Search backward (*phi-search-backward*)|


### Multiple cursors

| Shortcut                                                              | Action        |
| --------------------------------------------------------------------- |:-------------:|
| <kbd>CTRL</kbd>+<kbd>c</kbd> <kbd>CTRL</kbd>+<kbd>s</kbd>             | Set a new cursor at the next occurence of the selected word (*mc/mark-next-like-this-word*)|
| <kbd>CTRL</kbd>+<kbd>c</kbd> <kbd>CTRL</kbd>+<kbd>r</kbd>             | Set a new cursor at the previous occurence of the selected word (*mc/mark-previous-like-this-word*)|
| <kbd>CTRL</kbd>+<kbd>c</kbd> <kbd>ENTER</kbd>                         | Set cursors on region (*mc/edit-lines*)|

### Code manipulation

| Shortcut                                                              | Action        |
| --------------------------------------------------------------------- |:-------------:|
| <kbd>CTRL</kbd>+<kbd>c</kbd> <kbd>CTRL</kbd>+<kbd>&#9664;</kbd>       | Hide all blocks of code [`{...}`] (*hs-hide-all*)|
| <kbd>CTRL</kbd>+<kbd>c</kbd> <kbd>CTRL</kbd>+<kbd>&#9654;</kbd>       | Show all blocks of code (*hs-show-all*)|
| <kbd>CTRL</kbd>+<kbd>c</kbd> <kbd>&#9664;</kbd>                       | Hide current block of code [`{...}`] (*hs-hide-block*)|
| <kbd>CTRL</kbd>+<kbd>c</kbd> <kbd>&#9654;</kbd>                       | Show current block of code (*hs-show-block*)|
| <kbd>CTRL</kbd>+<kbd>c</kbd> <kbd>CTRL</kbd>+<kbd>c</kbd>             | Comment / Uncomment selected code (*comment-dwim*)|

#### Emmet (HTML)

You can write HTML with Emmet thanks to the *emmet-mode*.

| Shortcut                                                              | Action        |
| --------------------------------------------------------------------- |:-------------:|
| <kbd>CTRL</kbd>+<kbd>x</kbd> <kbd>CTRL</kbd>+<kbd>e</kbd>             | Show HTML for the selected emmet code (*emmet-preview*) |


## Special modes

### js2-mode

`js2-mode` handle Javascript syntax highlighting and auto-indent and comes with an integrated linter.
It shows undefined variables, functions with side effects, functions that doesn't always return a value ...

### company-mode

`company-mode` allows fast autocomplete (better than `autocomplete`).

### custom-prompt

<kbd>ALT</kbd>+<kbd>z</kbd> will execute `custom-prompt` function. This little function is located at the following place :

> .emacs.d/experiment.el

For the moment, here are the few features of this prompt (more will come) :

* `:*number*` : go to line *number*
* `d*number*` : delete the next *number* lines


## TODO

- [x] Auto-install missing packages
- [x] Use phi-search only in multiple-cursors mode
- [x] Create different configurations for emacs in terminal and emacs with GUI
- [ ] improve emacs with GUI
- [ ] Optimization : prevent useless minor-modes or scripts from being used while they are unnecessary
- [ ] Optimization : make it work with emacsclient (emacs as a daemon)
- [ ] Autocomplete (company-mode) : Make it work with Rust, SQL, C and HTML
- [ ] Magit : Create shortcut for magit (status, commit, push, blame, diff)
- [ ] Choose and include a tool for easy buffer management
