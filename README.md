[![BuildStatus](https://travis-ci.org/monky-hs/monky.svg?branch=master)]
Monky is an application to feed status bars (like dzen2) with a lines to display.

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
