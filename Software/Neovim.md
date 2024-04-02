<img class="header-logo" src="/static/neovim.svg"/>

Neovim is a Vim fork and refactor that stays fully compatible with Vim's editing model and Vimscript v1, while adding some modern features on top.

The most important features of Neovim are:
- Vim compatibility
- built-in native LSP client
- Lua scripting interface as an alternative to Vimscript

## Vim compatibility

This is important for skill transfer. Long-time Vi/Vim users should have minimal barrier to entry when migrating to Neovim.

New users also benefit from this. If you learn Neovim, you will be able to use Vim on computers that have it. Vim is installed on most Linux distributions and macOS by default.

## LSP client

LSP provides real-time programming feedback such as error reporting and autocompletion suggestions right in the editor.

It is not configured by default, but can be set up easily for your languages of choice.

## Lua scripting

Vim is configurable and extendable with a bespoke language called Vimscript.
Neovim additionally supports Lua, which is more familiar and probably easier to learn.

Neovim's Lua engine is asynchronous and allows writing powerful plugins more ergonomically.
Since its introduction in Neovim 0.5, there has been an explosion of new plugins written in Lua.
You can usually spot a Lua plugin by its suffix `-lua` or `-nvim` (Vimscript plugins usually choose `-vim`).

In my experience, these plugins are less mature than their Vimscript ancestors, but are more flashy and featureful.
