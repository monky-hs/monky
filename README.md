[![BuildStatus](https://travis-ci.org/monky-hs/monky.svg?branch=master)](https://travis-ci.org/monky-hs/monky)

This is the 2.0 Branch of monky. 2.0 has a couple of breaking changes, please look at the bottom for a summary.

monky is an application to feed status bars (like dzen2/i3bar) with a lines to display.

If you have any more questions or suggestions, come and join us (or rather me) on freenode at #monky.

# Install
 * Clone this repo
 * Run `cabal install`. This installs a **monky** executable in *~/.cabal/bin*.

# How to
 * Install (well that's obvious)
 * Run **monky** once to create example in *~/.monky*
 * Edit the example until it fits your needs
 
### How do I configure this, which modules exist?
[Documentation](http://monky-hs.github.io/index.html) generated with haddock.
   
### Why does this config look like programming?
Because it is [Haskell](https://www.haskell.org/). The idea for this configuration style is taken shamelessly from [xmonad](http://xmonad.org/).

Don't worry, you don't have to know haskell to build a simple configuration.
And it enables those who know haskell to build whatever they like.

## Why should I use a statusbar

It looks sleek with tiling window managers, e.g. [xmonad](http://xmonad.org/).

## Notes

The unicode glyphs used by UTF8/I3 outputs are rather obscure and may not be provided by your fonts.


### For dzen
The default examples use `.xbm` files.
They have been build with the 
[nice icon pack from sm4tik](http://awesome.naquadah.org/wiki/Nice_Icons). (This
is hosted in the awesome wiki because the original source disappeared).

Copy them into `~/.monky/xbm/`.

For the fancy clock example you will also need the `.xbm` files in this repo.

## Examples:

![Ongy-PC](http://i.imgur.com/Jvdx4jy.png?1)
[config](http://lpaste.net/143261)

![Ongy-PC-NEW](http://i.imgur.com/oWzP924.png?1)
[config](http://lpaste.net/146044)

![Ongy-Laptop](http://i.imgur.com/EzHD3re.png?1)
[config](http://lpaste.net/143262)

### 2.0 branch

![Laptop-2.0](http://i.imgur.com/nQQ9ywX.jpg?1)
[config](http://lpaste.net/170301)

![PC-variaty](http://i.imgur.com/MUhvxY0.jpg?1)
[config](http://lpaste.net/639572968845869056)
[started](http://lpaste.net/1324624775158431744)

## 2.0 breaking changes
 * startLoop needs a seconds argument. Modules will be in monky.Outputs
 * pack was split into pollPack and evtPack
 * Module was split in PollModule and EvtModule for polling and event based
 * Examples use newtype wrappers. Orphan instances are now discouraged
 * Memory/CPU/Time have been merged, new get\* functions are used to choose mode
 * Prepend has been moved
 * the executable that's compiled by monky has been renamed to monky.exe to avoid confusion

### General changes
 * monky is now more modular and has an internal output datatype
 * monky moved to Data.Text from (text) over String
 * the event system has been simplified
 * config file does not have to import monky.\* anymore, monky.Example.\* is enough


# Why monky for status generation

* + On my machine, monky tends to use less cpu time than others, which is relevant for laptops
* - monky takes a fair amount of disk space (ghc dependency) and has bigger binaries (mostly ghc RTS)

## Why use monky instead of conky?

* + monky is contained in a single process
* + monky does not have a dependency on X
* - monky can not render it's own output

## Why use this not i3-status

* + monkys configuration is more powerfull
* + monky works better over the network
* + monky has the better modules (somewhat subjective)
* o both support multiple output modes with little to no config edits.
* - i3-status works better with i3bar (e.g. short texts)
* - i3-status may be easier to configure
