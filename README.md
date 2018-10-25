# Haskell Pong

## Dev Instructions

### Init

* Create your package sandbox: `cabal sandbox init`
	* All packages get installed in `.cabal-sandbox` directory as long as you run `cabal` from `ROOT`
* Create package: `cabal init`
* Install dependency: `cabal install gloss==1.13.*`

### Dev Process

* Run code: `cabal run`
* Run `cabal clean` if you want to get rid of old compiled files from the `dist` folder
* Alternatively you can build the exe using `cabal build` and run it using `./dist/build/haskell-pong/haskell-pong`

### Generate Documentation

* `cabal haddock --executables`

### VS Code Formatter

* `stylish-haskell` - bad, not very opinionated 