################################################################################
####################             Project Euler              ####################
####################               Problem 25               ####################
####################      1000-digit Fibonacci number       ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################             January 5, 2019            ####################
################################################################################

# The Fibonacci sequence is defined by the recurrence relation:
#
#    F_{n} = F_{n−1} + F_{n−2}, where F_{1} = 1 and F_{2} = 1.
#
# Hence the first 12 terms will be:

#    F_{1} = 1
#    F_{2} = 1
#    F_{3} = 2
#    F_{4} = 3
#    F_{5} = 5
#    F_{6} = 8
#    F_{7} = 13
#    F_{8} = 21
#    F_{9} = 34
#    F_{10} = 55
#    F_{11} = 89
#    F_{12} = 144
#
# The 12th term, F_{12}, is the first term to contain three digits.
#
# What is the index of the first term in the Fibonacci sequence
# to contain 1000 digits?


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

sumNumbersAsCharacters <- function(
    
    x,
    y
    
){
    
    # '''
    # Sums up two numbers "x" a "y" coded as characters and returns the sum
    # as a character as well in order to preserve all numbers' digits.
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

my_fibonacci_sequence <- c("1", "1")

n <- 1000

while(
    nchar(
        my_fibonacci_sequence[
            length(
                my_fibonacci_sequence
            )
        ]
    ) < n
){
    
    my_fibonacci_sequence <- c(
        
        my_fibonacci_sequence,
        sumNumbersAsCharacters(
            
            my_fibonacci_sequence[
                length(
                    my_fibonacci_sequence
                ) - 1
            ],
            my_fibonacci_sequence[
                length(
                    my_fibonacci_sequence
                )
            ]
            
        )
        
    )
    
    flush.console()
    print("################################################################")
    print(
        paste(
            "i = ",
            length(
                my_fibonacci_sequence
            ),
            " | ",
            "# of digits = ",
            nchar(
                as.character(
                    my_fibonacci_sequence[
                        length(
                            my_fibonacci_sequence
                        )
                    ]
                )
            ),
            sep = ""
        )
    )
    
}


## -----------------------------------------------------------------------------

print(
    length(
        my_fibonacci_sequence
    )
)   # 4782


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





