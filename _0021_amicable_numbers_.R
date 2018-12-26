################################################################################
####################             Project Euler              ####################
####################               Problem 21               ####################
####################            Amicable numbers            ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################           December 25, 2018            ####################
################################################################################

# Let d(n) be defined as the sum of proper divisors of n (numbers less
# than n which divide evenly into n). If d(a) = b and d(b) = a,
# where a =/= b, then a and b are an amicable pair and each of a and b
# are called amicable numbers.

# For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22,
# 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are
# 1, 2, 4, 71 and 142; so d(284) = 220.

# Evaluate the sum of all the amicable numbers under 10000.


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

n <- 9999
my_amicable_numbers <- NULL

for(i in 2:n){
    
    if(
        sum(
            getMyProperDivisors(
                sum(
                    getMyProperDivisors(i)
                )
            )
        ) == i &
        sum(
            getMyProperDivisors(i)
        ) != i
    ){
        
        my_amicable_numbers <- c(
            
            my_amicable_numbers,
            i
            
        )
        
    }
    
    flush.console()
    print(
        paste(
            "Number ",
            i,
            " was checked whether it is amicable.",
            sep = ""
        )
    )
    
}


## -----------------------------------------------------------------------------

print(
    sum(
        my_amicable_numbers
    )
)   # 31626


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





