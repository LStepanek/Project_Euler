################################################################################
####################             Project Euler              ####################
####################               Problem 4                ####################
####################       Largest palindrome product       ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################          September 5, 2018             ####################
################################################################################

# A palindromic number reads the same both ways. The largest palindrome
# made from the product of two 2-digit numbers is 9009 = 91 * 99.

# Find the largest palindrome made from the product of two 3-digit numbers.


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

isPalindrome <- function(x){
    
    # '''
    # Returns TRUE, if and only if the given character "x" is a palindrome,
    # i. e. can be read the same both ways.
    # '''
    
    if(
        nchar(x) <= 1
    ){  
        return(TRUE)
        
    }else{
        
        if(
            substr(x, 1, 1) != substr(x, nchar(x), nchar(x))
        ){
            
            return(FALSE)
            
        }else{
            
            return(
                isPalindrome(substr(x, 2, nchar(x) - 1))
            )
            
        }
        
    }
    
}


## -----------------------------------------------------------------------------

my_candidates <- rev(
    as.character(
        sort(
            unique(
                c(
                    outer(
                        100:999,
                        100:999,
                        "*"
                    )
                )
            )
        )
    )
)

i <- 1
is_palindrome <- FALSE

while(!is_palindrome){
    
    if(
        isPalindrome(my_candidates[i])
    ){
        is_palindrome <- TRUE
    }else{
        i <- i + 1
    }
    
}

print(
    as.numeric(my_candidates[i])
)   # 906609


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





