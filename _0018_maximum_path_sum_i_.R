################################################################################
####################             Project Euler              ####################
####################               Problem 18               ####################
####################           Maximum path sum I           ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################            December 24, 2018           ####################
################################################################################

# By starting at the top of the triangle below and moving to adjacent numbers
# on the row below, the maximum total from top to bottom is 23.

#       3
#      7 4
#     2 4 6
#    8 5 9 3

# That is, 3 + 7 + 4 + 9 = 23.

# Find the maximum total from top to bottom of the triangle below:

#                                75
#                              95  64
#                            17  47  82
#                          18  35  87  10
#                        20  04  82  47  65
#                      19  01  23  75  03  34
#                    88  02  77  73  07  63  67
#                  99  65  04  28  06  16  70  92
#                41  41  26  56  83  40  80  70  33
#              41  48  72  33  47  32  37  16  94  29
#            53  71  44  65  25  43  91  52  97  51  14
#          70  11  33  28  77  73  17  78  39  68  17  57
#        91  71  52  38  17  14  91  43  58  50  27  29  48
#      63  66  04  68  89  53  67  30  73  16  69  87  40  31
#    04  62  98  27  23  09  70  98  73  93  38  53  60  04  23

# NOTE: As there are only 16384 routes, it is possible to solve this problem
# by trying every route. However, Problem 67, is the same challenge with
# a triangle containing one-hundred rows; it cannot be solved by brute force,
# and requires a clever method! ;o)


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

my_html <- readLines(
    
    con = "https://projecteuler.net/problem=18",
    encoding = "UTF-8"
    
)

my_triangle <- lapply(
    lapply(
        gsub(
            "<.*?>",
            "\\1",
            my_html[
                grepl(
                    "[0-9]{2}<[br|/p]",
                    my_html[
                        !grepl(
                            "NOTE",
                            my_html
                        )
                    ]
                )
            ]
        ),
        function(x) strsplit(x, split = " ")[[1]]
    ),
    as.numeric
)

my_grid <- matrix(
    
    rep(NA, length(my_triangle) ** 2),
    nrow = length(my_triangle)
    
)

for(k in 1:length(my_triangle)){
    
    for(j in 1:length(my_triangle)){
        
        my_grid[k + 1 - j, j] <- my_triangle[[k]][j]
        
    }
    
}


## -----------------------------------------------------------------------------

my_path_sums[[1]] <- list(
    
    "path_sum" = my_grid[1, 1],
    "coordinates_of_path_end" = c(1, 1)
    
)

k <- 1

while(k < length(my_triangle)){
    
    temp_path_sums <- list()
    
    for(i in 1:length(my_path_sums)){
        
        temp_path_sums[[length(temp_path_sums) + 1]] <- list(
            
            "path_sum" = my_path_sums[[i]][["path_sum"]] + my_grid[
                my_path_sums[[i]][[
                    "coordinates_of_path_end"
                ]][1] + 1,
                my_path_sums[[i]][[
                    "coordinates_of_path_end"
                ]][2]
            ],
            "coordinates_of_path_end" = c(
                my_path_sums[[i]][[
                    "coordinates_of_path_end"
                ]][1] + 1,
                my_path_sums[[i]][[
                    "coordinates_of_path_end"
                ]][2]
            )
            
        )
        
        temp_path_sums[[length(temp_path_sums) + 1]] <- list(
            
            "path_sum" = my_path_sums[[i]][["path_sum"]] + my_grid[
                my_path_sums[[i]][[
                    "coordinates_of_path_end"
                ]][1],
                my_path_sums[[i]][[
                    "coordinates_of_path_end"
                ]][2] + 1
            ],
            "coordinates_of_path_end" = c(
                my_path_sums[[i]][[
                    "coordinates_of_path_end"
                ]][1],
                my_path_sums[[i]][[
                    "coordinates_of_path_end"
                ]][2] + 1
            )
            
        )
        
    }
    
    my_path_sums <- temp_path_sums
    
    k <- k + 1
    
}


## -----------------------------------------------------------------------------

print(
    max(
        unlist(
            lapply(
                my_path_sums,
                "[[",
                "path_sum"
            )
        )
    )
)   # 1074


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





