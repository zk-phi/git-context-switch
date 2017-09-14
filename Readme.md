# git-context-switch

Context switching for git.

## Demo

![Demo](screencast.gif)

## Dependencies

Emacs (>=24), as an interpreter.

## Installation

Make `git-context-switch.el` executable visible from PATH

```
cp git-context-switch.el /usr/local/bin/git-context-switch
chmod +x /usr/local/bin/git-context-switch
```

and (optionally) add an alias to `~/.gitconfig`.

```
[alias]
context = "!f() { git-context-switch $*; }; f"
```

## Usage

git-context-switch have following subcommands:

* `git context` ... Show current context name
* `git context list` ... Show list of all all contextsx.
* `git context create <name>` ... Create new context.
* `git context delete <name>` ... Delete context.
* `git context switch <name>` ... Switch to context.

You may have branch and stash list for each contexts.
