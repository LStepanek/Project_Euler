################################################################################
####################             Project Euler              ####################
####################               Problem 23               ####################
####################           Non-abundant sums            ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################           December 26, 2018            ####################
################################################################################

# A perfect number is a number for which the sum of its proper divisors
# is exactly equal to the number. For example, the sum of the proper divisors
# of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is
# a perfect number.

# A number n is called deficient if the sum of its proper divisors is less
# than n and it is called abundant if this sum exceeds n.

# As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
# number that can be written as the sum of two abundant numbers is 24.
# By mathematical analysis, it can be shown that all integers greater than
# 28123 can be written as the sum of two abundant numbers. However, this upper
# limit cannot be reduced any further by analysis even though it is known that
# the greatest number that cannot be expressed as the sum of two abundant
# numbers is less than this limit.

# Find the sum of all the positive integers which cannot be written as the sum
# of two abundant numbers.


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

getMyProperDivisors <- function(n){
    
    # '''
    # Returns all positive integer divisors of a given natural number "n"
    # which are less than "n".
    # '''
    
    my_divisors <- NULL
    
    for(i in 1:n){
        
        if(n %% i == 0){
            
            my_divisors <- c(
                
                my_divisors,
                i
                
            )
            
        }
        
    }
    
    return(
        
        my_divisors[-which(my_divisors == n)]
        
    )
    
}


## -----------------------------------------------------------------------------

n <- 30000
my_abundant_numbers <- NULL

for(i in 1:n){
    
    if(
        sum(getMyProperDivisors(i)) > i
    ){
        
        my_abundant_numbers <- c(
            
            my_abundant_numbers,
            i
            
        )
        
    }
    
    flush.console()
    print(
        paste(
            "Process completed for ",
            format(
                round(
                    i / n * 100,
                    digits = 3
                ),
                nsmall = 3
            ),
            " %.",
            sep = ""
        )
    )
    
}


# my_anticandidates <- NULL

# for(first_abundant_number in my_abundant_numbers){
    
    # for(second_abundant_number in my_abundant_numbers){
        
        # if(
            # ! (first_abundant_number + second_abundant_number) %in%
            # my_anticandidates
        # ){
            
            # my_anticandidates <- c(
                
                # my_anticandidates,
                # (
                    # first_abundant_number + second_abundant_number
                # )
                
            # )
            
        # }
        
        # flush.console()
        # print(
            # paste(
                # "Process completed for ",
                # format(
                    # round(
                        # (
                            # (
                                # which(
                                    # my_abundant_numbers ==
                                    # first_abundant_number
                                # ) - 1
                            # ) +    which(
                                # my_abundant_numbers ==
                                # second_abundant_number
                            # ) / length(my_abundant_numbers)
                        # ) / length(my_abundant_numbers) * 100,
                        # digits = 3
                    # ),
                    # nsmall = 3
                # ),
                # " %.",
                # sep = ""
            # )
        # )
        
    # }
    
# }


my_anticandidates <- sort(
    unique(
        as.vector(
            outer(
                my_abundant_numbers,
                my_abundant_numbers,
                "+"
            )
        )
    )
)


my_candidates <- NULL

for(i in 1:n){
    
    if(! i %in% my_anticandidates){
        
        my_candidates <- c(
            
            my_candidates,
            i
            
        )
        
    }
    
}


## -----------------------------------------------------------------------------

print(
    sum(
        my_candidates
    )
)   # 4179871


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





