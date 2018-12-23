################################################################################
####################             Project Euler              ####################
####################               Problem 17               ####################
####################          Number letter counts          ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################           December 22, 2018            ####################
################################################################################

# If the numbers 1 to 5 are written out in words: one, two, three, four,
# five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

# If all the numbers from 1 to 1000 (one thousand) inclusive were written
# out in words, how many letters would be used?

# NOTE: Do not count spaces or hyphens. For example, 342 (three hundred
# and forty-two) contains 23 letters and 115 (one hundred and fifteen)
# contains 20 letters. The use of "and" when writing out numbers is
# in compliance with British usage.


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

numberToWords <- function(
    
    my_number,
    return_as_number_of_letters = FALSE
    
){
    
    # '''
    # Returns the given natural number "my_number" (less than or equaled
    # to 1000) written out in words.
    #
    # For example, for 342 a character "three hundred and forty-two"
    # or for 115 a character "one hundred and fifteen" is returned.
    #
    # If "return_as_number_of_letters" == TRUE, then number of letters
    # contained in the word representing the given "my_number" is
    # returned (spaces and hyphens are not counted).
    # '''
    
    ## firstly I initialize my basic vocabulary of numbers in English ----------
    
    my_vocabulary <- setNames(
        
        object = c(
            1, 2, 3, 4, 5,
            6, 7, 8, 9, 10,
            11, 12, 13, 14, 15,
            16, 17, 18, 19, 20,
            30, 40, 50, 60, 70,
            80, 90
        ),
        nm = c(
            "one", "two", "three", "four", "five",
            "six", "seven", "eight", "nine", "ten",
            "eleven", "twelve", "thirteen", "fourteen", "fifteen",
            "sixteen", "seventeen", "eighteen", "nineteen", "twenty",
            "thirty", "forty", "fifty", "sixty", "seventy",
            "eighty", "ninety"
        )
        
    )
    
    
    ## now I am deciding whether the given "my_number" is less than 100,
    ## or 1000 or equaled to 1000 ----------------------------------------------
    
    for(
        my_degree in c(
            
            "my_units",
            "my_tens",
            "my_hundreds",
            "my_thousands"
            
        )
    ){
        
        assign(
            
            my_degree,
            (
                my_number %/% (
                    10 ** (
                        which(
                            c(
                                "my_units",
                                "my_tens",
                                "my_hundreds",
                                "my_thousands"
                            ) == my_degree
                        ) - 1
                    )
                )
            ) %% 10
            
        )
        
    }
    
    my_output <- NULL
    
    for(
        my_degree in c(
            
            "my_thousands",
            "my_hundreds"
            
        )
    ){
        
        if(get(my_degree) > 0){
            
            my_output <- c(
                
                my_output,
                paste(
                    names(
                        my_vocabulary[
                            my_vocabulary == get(my_degree)
                        ]
                    ),
                    gsub(
                        "^my_",
                        "",
                        gsub(
                            "s$",
                            "",
                            my_degree
                        )
                    ),
                    sep = " "
                )
                
            )
            
        }
        
    }
    
    if(my_tens == 1){
        
        my_output <- c(
            
            my_output,
            names(
                my_vocabulary[
                    my_vocabulary == 10 * my_tens + my_units
                ]
            )
            
        )
        
    }else{
        
        if(my_units > 0){
            
            my_output <- c(
                
                my_output,
                paste(
                    if(my_tens > 1){
                        
                        paste(
                            names(
                                my_vocabulary[
                                    my_vocabulary == 10 * my_tens
                                ]
                            ),
                            "-",
                            sep = ""
                        )
                        
                    },
                    names(
                        my_vocabulary[
                            my_vocabulary == my_units
                        ]
                    ),
                    sep = ""
                )
                
            )
            
        }else{
            
            my_output <- c(
                
                my_output,
                names(
                    my_vocabulary[
                        my_vocabulary == 10 * my_tens
                    ]
                )
                
            )
            
        }
        
    }
    
    
    ## finally, I am returning an output ---------------------------------------
    
    if(
        return_as_number_of_letters
    ){
        
        return(
            nchar(
                gsub(
                    "[- ]",
                    "",
                    paste(
                        my_output,
                        collapse = " and "
                    )
                )
            )
        )
        
    }else{
        
        return(
            paste(
                my_output,
                collapse = " and "
            )
        )
        
    }
    
}


## -----------------------------------------------------------------------------

n <- 1000

print(
    do.call(
        sum,
        lapply(
            1:n,
            numberToWords,
            return_as_number_of_letters = TRUE
        )
    )
)   # 21124


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





