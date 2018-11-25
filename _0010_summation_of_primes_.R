################################################################################
####################             Project Euler              ####################
####################               Problem 10               ####################
####################          Summation of primes           ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################           November 24, 2018            ####################
################################################################################

# The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

# Find the sum of all the primes below two million.


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

getAllMyPrimes <- function(
    
    x
    
){
    
    # '''
    # Returns a vector of prime numbers less than or equal to the given
    # real number "x". In other words, it is one of possible
    # implementations of Pi(x) function but instead of number of such
    # primes, a vector of all such primes is returned.
    # '''
    
    if(
        x < 2
    ){  
        return(NULL)
    }
    
    if(
        x >= 2 & x < 5
    ){  
        return(c(2, 3)[c(2, 3) <= x])
    }
    
    if(
        x >= 5
    ){
        
        ## my R implementation of Sieve of Eratosthenes ------------------------
        
        my_primes <- c(2:floor(x))[c(2:floor(x)) %% 6 %in% c(1, 5)]
        
        i <- 1
        
        while(i <= length(my_primes)){
            
            j <- i + 1
            
            while(j <= length(my_primes)){
                
                if(
                    my_primes[j] %% my_primes[i] == 0
                ){
                    my_primes <- my_primes[-j]
                }
                
                j <- j + 1
                
            }
            
            i <- i + 1
            
        }
        
        return(
            c(2, 3, my_primes)
        )
        
    }
    
}


## -----------------------------------------------------------------------------

n <- 2000000

sum(
    getAllMyPrimes(n - 1)
)   # 142913828922


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





