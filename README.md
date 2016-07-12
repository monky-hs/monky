[![BuildStatus](https://travis-ci.org/monky-hs/monky.svg?branch=master)](https://travis-ci.org/monky-hs/monky)

This is an experimental branch, it is expected to fail CI because a dependency
is not yet on hackage (Also the <$> import might be missing):
https://github.com/ongy/netlink-h://github.com/ongy/netlink-hs

Monky is an application to feed status bars (like dzen2) with a lines to display.

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


## Why use monky instead of conky?

* Monky is contained in a single process
* Monky does not have a dependency on X

## Why should I use a statusbar

It looks sleek with tiling window managers, e.g. [xmonad](http://xmonad.org/).

### Notes

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

