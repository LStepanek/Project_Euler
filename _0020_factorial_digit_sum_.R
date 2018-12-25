################################################################################
####################             Project Euler              ####################
####################               Problem 20               ####################
####################          Factorial digit sum           ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################           December 25, 2018            ####################
################################################################################

# n! means n x (n − 1) x ... x 3 x 2 x 1

# For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800, and the sum
# of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

# Find the sum of the digits in the number 100!


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

sumNumbersAsCharacters <- function(
    
    x,
    y
    
){
    
    # '''
    # Sums up two numbers "x" a "y" coded as characters and returns the sum
    # as a character as well in order to preserve all number's digits.
    # '''
    
    x <- as.numeric(unlist(strsplit(x, split = "")[[1]]))
    y <- as.numeric(unlist(strsplit(y, split = "")[[1]]))
    
    if(length(x) >= length(y)){
        
        y <- c(
            
            rep(0, length(x) - length(y)),
            y
            
        )
        
    }else{
        
        x <- c(
            
            rep(0, length(y) - length(x)),
            x
            
        )
        
    }
    
    my_sum <- NULL
    my_overdigit <- 0

    for(
        i in c(length(x):1)
    ){
        
        my_sum <- c(
            
            sum(
                c(
                    x[i],
                    y[i],
                    my_overdigit
                )
            ) %% 10,
            my_sum
            
        )
        
        my_overdigit <- sum(
            c(
                x[i],
                y[i],
                my_overdigit
            )
        ) %/% 10
        
    }
    
    if(my_overdigit > 0){
        
        my_sum <- c(
            
            as.numeric(
                strsplit(
                    as.character(
                        my_overdigit
                    ),
                    split = ""
                )[[1]]
            ),
            my_sum
            
        )
        
    }
    
    return(
        paste(
            my_sum,
            collapse = ""
        )
    )
    
}


## -----------------------------------------------------------------------------

n <- 100
my_output <- 1

for(i in 2:n){
    
    temp_output <- my_output
    
    for(j in 1:(i - 1)){
        
        temp_output <- sumNumbersAsCharacters(
            
            as.character(temp_output),
            as.character(my_output)
            
        )
        
    }
    
    my_output <- temp_output
    
    flush.console()
    print(
        paste(
            "Factorial of ",
            i,
            " just calculated.",
            sep = ""
        )
    )
    
}


## -----------------------------------------------------------------------------

print(
    do.call(
        "sum",
        lapply(
            strsplit(
                my_output,
                split = ""
            ),
            as.numeric
        )
    )
)   # 648


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





