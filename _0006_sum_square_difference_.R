################################################################################
####################             Project Euler              ####################
####################               Problem 6                ####################
####################         Sum square difference          ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################          September 7, 2018             ####################
################################################################################

# The sum of the squares of the first ten natural numbers is,
# 1 ^ 2 + 2 ^ 2 + ... + 10 ^ 2 = 385

# The square of the sum of the first ten natural numbers is,
# (1 + 2 + ... + 10) ^ 2 = 552 = 3025

# Hence the difference between the sum of the squares of the first
# ten natural numbers and the square of the sum is 3025 − 385 = 2640.

# Find the difference between the sum of the squares of the first
# one hundred natural numbers and the square of the sum.


## -----------------------------------------------------------------------------

################################################################################

## We can derive both the sum of the squares of the first "n" natural numbers
## and the square of the sum of the first "n" natural numbers from Faulhaber's
## formula.

## We get

## 1 ^ 2 + 2 ^ 2 + ... + n ^ 2 = n * (n + 1) * (2 * n + 1) / 6

## and

## (1 + 2 + ... + n) ^ 2 = (n * (n + 1) / 2) ^ 2.

## The diference then simply follows the form

## (n * (n + 1) / 2) ^ 2 - n * (n + 1) * (2 * n + 1) / 6.


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

n <- 100

(n * (n + 1) / 2) ^ 2 - n * (n + 1) * (2 * n + 1) / 6  # 25164150

# in more "R-like" way as
# > sum(c(1:n)) ^ 2 - sum(c(1:n) ^ 2)                  # 25164150


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





