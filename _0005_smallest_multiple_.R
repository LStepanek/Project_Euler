################################################################################
####################             Project Euler              ####################
####################               Problem 5                ####################
####################            Smallest multiple           ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################          September 7, 2018             ####################
################################################################################

# 2520 is the smallest number that can be divided by each of the numbers
# from 1 to 10 without any remainder.

# What is the smallest positive number that is evenly divisible,
# i. e. divisible with no remainder, by all of the numbers from 1 to 20?


## -----------------------------------------------------------------------------

################################################################################

## my dirty solution using R ---------------------------------------------------

i <- 1
is_divisible <- FALSE

while(!is_divisible){
    
    if(
        all(
            i %% c(1:20) == 0
        )
    ){
        
        is_divisible <- TRUE
        
    }else{
        
        i <- i + 1
        
    }
    
    flush.console()
    print(
        paste(
            "Checked for i = ",
            i,
            ".",
            sep = ""
        )
    )
    
}

print(i)   # 232792560


## -----------------------------------------------------------------------------

## my better solution using R --------------------------------------------------

getMyFactorization <- function(n){
    
    # '''
    # Return a list of all prime divisors of the given natural number "n"
    # and their powers.
    # '''
    
    if(n == 1){
        
        return(
            list(
                "divisors" = 1,
                "powers" = 1
            )
        )
        
    }else{
        
        ## my R implementation of Sieve of Eratosthenes ------------------------
        
        my_primes <- c(2:n)
        
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
        
        
        ## ---------------------------------------------------------------------
        
        my_output <- list(
            
            "divisors" = NULL,
            "powers" = NULL
            
        )
        
        for(my_prime in my_primes){
            
            i <- 0
            
            while(n %% my_prime ^ i == 0){
                
                i <- i + 1
                
            }
            
            if(i - 1 > 0){
                
                my_output[["divisors"]] <- c(
                    
                    my_output[["divisors"]],
                    my_prime
                    
                )
                my_output[["powers"]] <- c(
                    
                    my_output[["powers"]],
                    i - 1
                    
                )
                
            }
            
        }
        
        return(
           my_output
        )
        
    }
    
}


## -----------------------------------------------------------------------------

greatestCommonDivisor <- function(a, b){
    
    # '''
    # Return a greatest common divisor for the given natural
    # numbers "a" and "b" using Euclid's algorithm.
    # '''
    
    if(
        max(c(a, b)) %% min(c(a, b)) == 0
    ){
        
        return(
            min(c(a, b))
        )
        
    }else{
        
        return(
            greatestCommonDivisor(
                min(c(a, b)),
                max(c(a, b)) %% min(c(a, b))
            )
        )
        
    }
    
}


leastCommonMultiple <- function(
    
    a,
    b,
    algorithm = c("euklid", "factorization")
    
){
    
    # '''
    # Return a least common multiple for the given natural
    # numbers "a" and "b" using either "euklid" or "factorization"
    # algorithm.
    # '''
    
    if(
        ! algorithm %in% c("euklid", "factorization")
    ){
        stop("Please select 'euklid' or 'factorization' type of algorithm.")
    }
    
    if(
        algorithm == "euklid"
    ){
        
        return(
            a * b / greatestCommonDivisor(a, b)
        )
        
    }
    
    if(
        algorithm == "factorization"
    ){
        
        my_output <- list(
            "divisors" = NULL,
            "powers" = NULL
        )
        
        for(my_number in c(a, b)){
            
            assign(
                paste(
                    c("a", "b")[which(c(a, b) == my_number)],
                    "_factorized",
                    sep = ""
                ),
                getMyFactorization(my_number)
            )
            
        }
        
        for(my_number in a_factorized[["divisors"]]){
            
            my_output[["divisors"]] <- c(
                
                my_output[["divisors"]],
                my_number
                
            )
            
            if(my_number %in% b_factorized[["divisors"]]){
                
                my_output[["powers"]] <- c(
                    
                    my_output[["powers"]],
                    max(
                        c(
                            a_factorized[["powers"]][
                                a_factorized[["divisors"]] == my_number
                            ],
                            b_factorized[["powers"]][
                                b_factorized[["divisors"]] == my_number
                            ]
                        )
                    )
                    
                )
                
            }else{
                
                my_output[["powers"]] <- c(
                    
                    my_output[["powers"]],
                    a_factorized[["powers"]][
                        a_factorized[["divisors"]] == my_number
                    ]
                    
                )
                
            }
            
        }
        
        for(my_number in b_factorized[["divisors"]]){
            
            if(! my_number %in% a_factorized[["divisors"]]){
                
                my_output[["divisors"]] <- c(
                    
                    my_output[["divisors"]],
                    my_number
                    
                )
                my_output[["powers"]] <- c(
                    
                    my_output[["powers"]],
                    b_factorized[["powers"]][
                        b_factorized[["divisors"]] == my_number
                    ]
                    
                )
                
            }
            
        }
        
        grand_total <- 1
        
        for(i in 1:length(my_output[["divisors"]])){
            
            grand_total <- grand_total * (
                my_output[["divisors"]][i] ^ my_output[["powers"]][i]
            )
            
        }
        
        return(
            grand_total
        )
        
    }
    
}


## -----------------------------------------------------------------------------

my_least_common_multiple <- 1

for(i in 2:20){
    
    my_least_common_multiple <- leastCommonMultiple(
        my_least_common_multiple,
        i,
        algorithm = "euklid" # "factorization" option is much more slower ...
    )
    
}

print(
    my_least_common_multiple
)   # 232792560


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





