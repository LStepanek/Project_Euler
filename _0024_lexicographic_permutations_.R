################################################################################
####################             Project Euler              ####################
####################               Problem 24               ####################
####################       Lexicographic permutations       ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################            January 5, 2019             ####################
################################################################################

# A permutation is an ordered arrangement of objects. For example, 3124 is
# one possible permutation of the digits 1, 2, 3 and 4. If all of the
# permutations are listed numerically or alphabetically, we call it
# lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

#    012   021   102   120   201   210

# What is the millionth lexicographic permutation of the digits 0, 1, 2, 3,
# 4, 5, 6, 7, 8 and 9?


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

getMyPermutations <- function(
    
    x
    
){
    
    # '''
    # Returns a matrix of all permutations of a given vector "x".
    # Returned permutations are ordered row by row in the outputted
    # matrix.
    # The vector "x" is supposed to consist only of unique items.
    # '''
    
    if(length(x) == 1){
        
        return(as.matrix(x))
        
    }else{
        
        my_permutations <- getMyPermutations(x[-1])
        
        my_output <- cbind(
            
            rep(
                x[1],
                dim(my_permutations)[1]
            ),
            my_permutations
            
        )
        
        if(length(x) > 2){
            
            for(i in 1:(dim(my_permutations)[2] - 1)){
                
                my_output <- rbind(
                    
                    my_output,
                    cbind(
                        
                        my_permutations[
                            ,
                            1:i
                        ],
                        rep(
                            x[1],
                            dim(my_permutations)[1]
                        ),
                        my_permutations[
                            ,
                            (i + 1):dim(my_permutations)[2]
                        ]
                        
                    )
                    
                )
                
            }
            
        }
        
        my_output <- rbind(
            
            my_output,
            cbind(
                
                my_permutations,
                rep(
                    x[1],
                    dim(my_permutations)[1]
                )
                
            )
            
        )
        
        return(
            my_output
        )
        
    }
    
}


## -----------------------------------------------------------------------------

my_permutations <- getMyPermutations(c(0:9))


## -----------------------------------------------------------------------------

print(
    sort(
        apply(
            my_permutations,
            1,
            paste,
            collapse = ""
        )
    )[1000000]
)   # 2783915460


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





