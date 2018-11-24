################################################################################
####################             Project Euler              ####################
####################               Problem 7                ####################
####################             10001st prime              ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################           November 24, 2018            ####################
################################################################################

# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
# we can see that the 6-th prime is 13.

# What is the 10 001-st prime number?


## -----------------------------------------------------------------------------

################################################################################

## I simply used a prime-counting function implementing well-known
## Sieve of Eratosthenes, by which I got a reasonable (and a little bit
## understimating) guess of the 10001-st prime.

## Then I found a number around the guess of the 10001-st prime that
## is for sure a prime and such that I was able to determine which one of
## all primes that one is.

## Finally, I started looking at numbers from the prime I was sure about
## is is a prime for sure while I eventually got 10001-st prime.


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

getMyPrimeCountingFunction <- function(
    
    x
    
){
    
    # '''
    # Returns a count of prime numbers less than or equal to the given
    # real number "x". In other words, it is one of possible
    # implementations of Pi(x) function.
    # '''
    
    if(
        x < 2
    ){
        
        return(0)
        
    }else{
        
        ## my R implementation of Sieve of Eratosthenes ------------------------
        
        my_primes <- c(2:floor(x))
        
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
            length(my_primes)
        )
        
    }
    
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

n <- 10001

i <- 10 ** max(
    c(1:100)[
        unlist(
            lapply(
                
                1:100,
                function(i) 10 ** i / log(10 ** i)
                
            )
        ) <= n
    ]
)   # the 10001-st prime is somewhere around 100000


while(!isPrime(i)){
    
    i <- i + 1
    
}   # now I get that 100003 the least prime greater than the guess 100000


which_one <- getMyPrimeCountingFunction(
    
    i
    
)   # now I know that prime 100003 is exactly the 9593-rd prime in a row


while(which_one <= n){
    
    if(
        isPrime(i)
    ){
        which_one <- which_one + 1
    }
    
    i <- i + 1
    
}

print(
    i - 1
)   # the 10001-st prime is equaled to 104743


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





