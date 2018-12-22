################################################################################
####################             Project Euler              ####################
####################               Problem 15               ####################
####################              Lattice paths             ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################           December 22, 2018            ####################
################################################################################

# Starting in the top left corner of a 2 x 2 grid, and only being able
# to move to the right and down, there are exactly 6 routes
# to the bottom right corner.

# How many such routes are there through a 20 x 20 grid?


## -----------------------------------------------------------------------------

################################################################################

## Once we start in the top left corner of an $M \times N$ grid, and only
## being able to move to the right and down, there are exactly $N$ horizontal
## segments and exactly $M$ vertical segments to go through in order to get
## to the bottom right corner. In other words, total length of any route from
## top left corner to bottom right one has got a length of exactly
## $M + N$ segments; the routes differ in order of horizontal and vertical
## segments (or in order or right and down moves).
## 
## Thus, for an $M \times N$ grid there are exactly
##
##    $$ {{M + N} \choose {M}} = {{M + N} \choose {N}} $$
##
## routes from top left corner to the bottom right one.


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

n <- 20
m <- 20

print(
    choose(m + n, m)
)   # 137846528820


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





