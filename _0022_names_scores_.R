################################################################################
####################             Project Euler              ####################
####################               Problem 22               ####################
####################              Names scores              ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################           December 26, 2018            ####################
################################################################################

# Using names.txt (right click and 'Save Link/Target As...'),
#
#    https://projecteuler.net/project/resources/p022_names.txt,
#
# a 46K text file containing over five-thousand first names, begin by sorting
# it into alphabetical order. Then working out the alphabetical value for
# each name, multiply this value by its alphabetical position in the list
# to obtain a name score.

# For example, when the list is sorted into alphabetical order, COLIN, which
# is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So,
# COLIN would obtain a score of 938 × 53 = 49714.

# What is the total of all the name scores in the file?


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

getMyAsciiSort <- function(
    
    x
    
){
    
    # '''
    # Sorts a character vector "x" containing non-alphanumeric characters
    # by the ASCII order (like C).
    # '''
    
    return(
        x[
            order(
                sapply(
                    x, 
                    function(i){
                        paste(
                            strtoi(
                                charToRaw(i),
                                base = 16
                            ),
                            collapse = ""
                        )
                    }
                )
            )
        ]
    )
    
}


getMyNameScore <- function(
    
    my_name
    
){
    
    # '''
    # Returns a score for the given character "my_name"; i. e. a sum of
    # indices of all english letters included in the "my_name" is returned.
    #
    # For example, when the list is sorted into alphabetical order, COLIN,
    # which is worth 3 + 15 + 12 + 9 + 14 = 53.
    # '''
    
    my_letters <- strsplit(
        
        my_name,
        split = ""
        
    )[[1]]
    
    return(
        do.call(
            "sum",
            lapply(
                my_letters,
                function(i){
                    which(LETTERS == i)
                }
            )
        )
    )
    
}


## -----------------------------------------------------------------------------

my_names <- getMyAsciiSort(
    do.call(
        "c",
        strsplit(
            gsub(
                '\"',
                "",
                readLines(
                    
                    con = paste(
                        "https://projecteuler.net/project/resources/",
                        "p022_names.txt",
                        sep = ""
                    ),
                    encoding = "UTF-8",
                    warn = FALSE
                    
                )
            ),
            split = ","
        )
    )
)


my_scores <- NULL

for(my_name in my_names){
    
    my_scores <- c(
        
        my_scores,
        which(my_names == my_name) * getMyNameScore(my_name)
        
    )
    
    flush.console()
    print(
        paste(
            "Process completed for ",
            format(
                round(
                    which(my_names == my_name) / length(my_names) * 100,
                    digits = 2
                ),
                nsmall = 2
            ),
            " %.",
            sep = ""
        )
    )
    
}


## -----------------------------------------------------------------------------

print(
    sum(
        my_scores
    )
)   # 871198282


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





