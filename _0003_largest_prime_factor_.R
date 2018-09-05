################################################################################
####################             Project Euler              ####################
####################               Problem 3                ####################
####################          Largest prime factor          ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################          September 5, 2018             ####################
################################################################################

# The prime factors of 13195 are 5, 7, 13 and 29.

# What is the largest prime factor of the number 600851475143 ?


## -----------------------------------------------------------------------------

################################################################################

## Once we get all positive integer divisors of a given number,
## we are able to choose the ones that are prime numbers.
## Finally, we can pick the largest prime from them.


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

getMyDivisors <- function(n){
    
    # '''
    # Returns all positive integer divisors of a given natural number "n".
    # '''
    
    my_divisors <- NULL
    
    for(i in 1:floor(sqrt(n))){
        
        if(n %% i == 0){
            
            my_divisors <- c(
                
                my_divisors,
                i
                
            )
            
            if(! (n / i) %in% my_divisors){
                
                my_divisors <- c(
                    
                    my_divisors,
                    n / i
                    
                )
                
            }
            
        }
        
    }
    
    return(
        
        sort(my_divisors)
        
    )
    
}


## -----------------------------------------------------------------------------

isPrime <- function(n){
    
    # '''
    # Returns TRUE, if and only if the given natural number "n" is a prime.
    # '''
    
    return(
        sum(
            unlist(
                lapply(
                    
                    1:floor(sqrt(n)),
                    function(i) return(n %% i == 0)
                    
                )
            )
        ) == 1
    )
    
}


## -----------------------------------------------------------------------------

n <- 600851475143

max(
    getMyDivisors(n)[
        unlist(
            lapply(
                getMyDivisors(n),
                isPrime
            )
        )
    ]
)   # 6857


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





