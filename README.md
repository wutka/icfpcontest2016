#ICFP Programming Contest - 2016

I have been using the team name O Caml, My Caml for almost every contest
since 2005, even though I haven't always used OCaml.

## The Problem
The contest this year was folding 2d origami shapes. You are given the
silhouette of the target shape and also the silhouette with all the
fold lines visibile (the skeleton) and you have to figure out how to fold
a square sheet of paper into that silhouette.

## My Strategy
In approaching this, I considered a top-down vs bottom-up. I wasn't sure
at the beginning how to approach top-down (start with the square and
try to figure out the folds to get down to the silhouette), so I instead
went with bottom-up where I start with the skeleton and try to unfold
along various line segments until I get to a square. Ultimately, this is
a very limited approach for various reasons. The number of combinations
gets very high, the skeleton doesn't have nearly enough detail (for
example, when you have several layers, you might fold one layer over
a particular line, or multiple layers, but the skeleton doesn't show
layers).

There is some problem in the way I generate the solutions so that even
for some problems that the program solves, the generated solution was
rejected by the server. I wasn't able to fix this in the alloted time.

## Building and Running
I used the stack tool to build this, just do:
```
stack build
```

To run it, you just give it a problem file on the command-line and
it prints the solution to stdout:
```
stack exec origami-exe problemfile
```
