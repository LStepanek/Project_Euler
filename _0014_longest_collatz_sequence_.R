################################################################################
####################             Project Euler              ####################
####################               Problem 14               ####################
####################         Longest Collatz sequence       ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################           December 22, 2018            ####################
################################################################################

# The following iterative sequence is defined for the set of positive
# integers:

#    n -> n / 2        (n is even)
#    n -> 3 * n + 1    (n is odd)

# Using the rule above and starting with 13, we generate the following
# sequence:

#    13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

# It can be seen that this sequence (starting at 13 and finishing at 1)
# contains 10 terms. Although it has not been proved yet (Collatz Problem),
# it is thought that all starting numbers finish at 1.

# Which starting number, under one million, produces the longest chain?

# NOTE: Once the chain starts the terms are allowed to go above one million.


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

getMyNextCollatzTerm <- function(
    
    x
    
){
    
    # '''
    # Returns next term of Collatz sequence, i. e. when the last term
    # of current Collatz sequence is "x", then the next one will be
    # either
    #
    #    x / 2         if x is even,
    #
    # or
    #
    #    3 * x + 1     if x is odd.
    #
    # '''
    
    return(
        
        if(
            x %% 2 == 0
        ){
            x / 2
        }else{
            3 * x + 1
        }
        
    )
    
}


## -----------------------------------------------------------------------------

n <- 1000000

my_collatz_sequences <- list()

for(i in 1:n){
    
    my_collatz_sequence <- c(i)
    
    while(
        my_collatz_sequence[
            length(
                my_collatz_sequence
            )
        ] != 1
    ){
        
        if(
            my_collatz_sequence[
                length(
                    my_collatz_sequence
                )
            ] < i
        ){
            
            ## I am using a dynamic programming approach -----------------------
            
            my_collatz_sequence <- c(
                
                my_collatz_sequence,
                my_collatz_sequences[[
                    my_collatz_sequence[
                        length(
                            my_collatz_sequence
                        )
                    ]
                ]][-1]
                
            )
            
        }else{
            
            my_collatz_sequence <- c(
                
                my_collatz_sequence,
                getMyNextCollatzTerm(
                    my_collatz_sequence[
                        length(
                            my_collatz_sequence
                        )
                    ]
                )
                
            )
            
        }
        
    }
    
    my_collatz_sequences[[i]] <- my_collatz_sequence
    
    flush.console()
    print(
        paste(
            "Just saved ",
            i,
            "-th Collatz sequence.",
            sep = ""
        )
    )
    
}


## -----------------------------------------------------------------------------

print(
    which(
        unlist(
            lapply(
                my_collatz_sequences,
                length
            )
        ) == max(
            unlist(
                lapply(
                    my_collatz_sequences,
                    length
                )
            )
        )
    )
)   # 837799


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





