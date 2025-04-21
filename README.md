## miniDusa

miniDusa is a finite-choice logic programming DSL within Racket similar to
[Dusa](https://dusa.rocks/). We implement the fact-set semantics as described by the
[Finite-Choice Logic Programming](https://dl.acm.org/doi/pdf/10.1145/3704849)
paper by Chris Martens, et al. This DSL was implemented as our final project for
Northeastern University’s [CS 3620 (Hack Your Own Language)](https://mballantyne.net/hyol/)
taught by Michael Ballantyne during Spring 2025.

## Installing and running

Check out this Git repository, change directory into it, and run:

<!-- once we get this on Racket packages, this needs to be changed -->
```
raco pkg install
```

Then import as

```
(require minidusa)
```

Once installed, you can access the documentation via:

```
raco docs minidusa
```

Finally, you can run the tests with:

```
raco test -p minidusa
```