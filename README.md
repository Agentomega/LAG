LAG
===

Logical Argument Generator

Table of Contents:

1. Installation
2. How to Run
3. Known Bugs
4. Future Work

1.  Installation
     
     LAG is written in Haskell and therefore requires the Haskell Platform,
     as well as certain packages within it.
     
     On Unix:
     sudo apt-get install haskell-platform
     
     On Windows:
     Binary installer at www.haskell.org
     
     You will need to manually add the Haskell binary directory to your PATH in Windows.
     On a 64 bit system, this is under:
     "C:\Program Files (x86)\Haskell Platform\<version number>\bin"
     
     Next, open the terminal or command line, and run the following:
     
     cabal update
     cabal install cabal-install
     cabal install random-extras random-fu regex-base regex-posix
     
     Optional:
     We used Sublime 2 and Sublime Haskell during development.
     
     Sublime may be found at:
     http://www.sublimetext.com/2
     
     Sublime Haskell:
     https://github.com/SublimeHaskell/SublimeHaskell
     
     Sublime Haskell comes with its own easy set of installation instructions.

2.  How to Run
     
     Open the terminal or command line, navigate to LAG/main, and run ghci
     
     :load main.hs
     
     As long as there are no build errors, it is now possible to run any function.
     
     The way we tested the code was to manually load operators like so:
     
     let operators = "aoc-"
     
     aoc stands for And Or Conditional, but may be replaced with any single character
     representation of the same connectives.  The '-' represents Not, and must be expressed
     as it is in the current version of the code.
     
     One may omit operators by leaving them out of the previous statement.
     
     The function convertAtoms takes in an integer and outputs a list of Atoms.  For example
     
     let atoms = convertAtoms 6
     
     Sets atoms to the list ["A", "B", "C", "D", "E", "F"].
     
     Generating a conclusion:
     
     Once we have a list of operators and a list of atoms, we may generate a conclusion
     as follows:
     
     getConclusion operators (atoms ++ "$")
     
     What this does is add a wildcard character to the list of atoms.  This is what allows us
     to recursively define our grammar.  Wherever there is a "$" in a sentence, we replace it with another
     sentence.  This returns a string which contains a random conclusion.
     
     We then parse the string into a tree-like list of tuples using a regular expression.
     
     extractTriples statement
     
     Returns a list of tuples which contain the first portion of a statement, the connective, and the second portion
     of a statement.  In the case of a negation, the negation is the connective with only the right side filled in.
     
     A tuple may then be used to determine which transforms are available to it using possibleTransforms
     and the mapping of regular expression to integers.  More on regular expressions/mappings in the rules.txt file.
     
     A transform may be an equivalence or an inference rule.  Equivalences generate no helper premises/steps,
     but may be used to re-write a statement in an equivalent form.  Inference rule transforms take in the end-result
     of an inference rule application, and generates the missing portions of the premise that result in that conclusion.
     
3.  Known Bugs:
     
     At present, the regular expression in parseLevel only parses binary connectives.
     
     disjunctionIntroTransform

4.  Future Work:
     
     Per rules.txt, there are several more transformations to be implemented.
     
     In order to generate a set of premises that contain all atoms, one must also keep track
     of atoms used.  One can then use the getConclusion function to generate the random portions
     of an inference rule transform with the missing atoms (and $) as the input.
     
     Each helper premise may also be applied to inference rule transforms to obtain more premises.
     This can be repeated until the desired number of premises is met.  However, we have yet to develop
     an algorithm to select a helper premise from the list of current premises, and replace it with the new set
     of statements.  Furthermore, how do we ensure we keep picking new premises to transform as opposed to
     premises that were generated from the last pick?
     
     Double negations - these may be removed from all statements at the end by searching for "--" and replacing
     with "".  This may be doable with regular expressions.
     
     Everything can be combined together into the main function once complete.