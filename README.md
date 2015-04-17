# Blaz
*Blaz* is a normalization solver for the language of S-terms, i.e. combinators
built using a single primitive combinator **S** and term application. Blaz
implements the approach of [1] reducing the initial problem to membership
testing for context-free grammars. The internal membership solver is using the
well-known CYK algorithm. The solver can be accelerated by performing a 
given constant number of reductions before the CYK algorithm is used.

## Features
Blaz provides a CLI interface. Available features include:
* Normalization decidability of given new-line separated S-terms,
  provided either on stdin or in given files.
* Listing of all non-normalizing S-terms of given size.
* Normalization speed-up using the standard reduction strategy for a given
  number of steps, before the slow, but deterministic normalization algorithm is
  used.

## Usage
> ./blaz -?

## Building & installation
Blaz is packaged using cabal. In order to build from source use:
> cabal configure
> cabal build
> cabal install

## References
Blaz was created on the basis of the following articles and technical reports:

1. Zachos, Ramirez, Hilaris - Normalizing S-terms can be generated by a
   context-free grammar, In Proceedings of the 9th Panhellenic Conference in
   Informatics, pages 714-728, 2003.
2. Lange, Leiss - An efficient yet presentable version of the CYK algorithm
   (http://www.informatica-didactica.de/cmsmadesimple/uploads/Artikel/LangeLeiss2009/LangeLeiss2009.pdf)
3. Hutton - Functional Pearls: Monadic Parsing in Haskell
   (https://www.cs.nott.ac.uk/~gmh/pearl.pdf)
