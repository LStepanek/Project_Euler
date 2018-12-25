################################################################################
####################             Project Euler              ####################
####################               Problem 19               ####################
####################            Counting Sundays            ####################
####################                                        ####################
####################    Solved by R by Lubomir Stepanek     ####################
####################            December 25, 2018           ####################
################################################################################

# You are given the following information, but you may prefer to do some
# research for yourself.

#    1 Jan 1900 was a Monday.
#    Thirty days has September,
#    April, June and November.
#    All the rest have thirty-one,
#    Saving February alone,
#    Which has twenty-eight, rain or shine.
#    And on leap years, twenty-nine.
#    A leap year occurs on any year evenly divisible by 4,
#    but not on a century unless it is divisible by 400.

# How many Sundays fell on the first of the month during the twentieth
# century (1 Jan 1901 to 31 Dec 2000)?


## -----------------------------------------------------------------------------

################################################################################

## my solution using R ---------------------------------------------------------

isLeap <- function(
    
    my_year
    
){
    
    # '''
    # Return TRUE if and only if the year "my_year" coded as a numeric
    # is leap one.
    #
    # A leap year is any year evenly divisible by 4, but not on a century
    # unless it is divisible by 400.
    # '''
    
    return(
        my_year %% 400 == 0 | (my_year %% 4 == 0 & my_year %% 100 != 0)
    )
    
}


my_weekdays <- c(
    
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
    
)


my_table <- NULL
first_day_of_year <- "Monday"  # since we know that 1 Jan 1900 was a Monday

for(my_year in c(1900:2000)){
    
    is_leap <- isLeap(as.numeric(my_year))
    
    n_of_days_per_months <- setNames(
        
        object = c(
            
            31,        # January
            if(is_leap){29}else{28},
                       # February
            31,        # March
            30,        # April
            31,        # May
            30,        # June
            31,        # July
            31,        # August
            30,        # September
            31,        # October
            30,        # November
            31         # December
            
        ),
        nm = c(
            
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December"
            
        )
        
    )
    
    
    my_year_table <- data.frame(
        
        "year" = rep(
            as.character(my_year),
            times = sum(n_of_days_per_months)
        ),
        "month" = unlist(
            lapply(
                names(n_of_days_per_months),
                function(i) rep(i, n_of_days_per_months[i])
            )
        ),
        "day_of_month" = unlist(
            lapply(
                n_of_days_per_months,
                function(i) as.character(
                    c(1:i)
                )
            )
        ),
        "weekday" = c(
            my_weekdays[
                which(
                    my_weekdays == first_day_of_year
                ):length(
                    my_weekdays
                )
            ],
            rep(
                my_weekdays,
                times = ceiling(sum(n_of_days_per_months) / 7)
            )
        )[
            1:sum(n_of_days_per_months)
        ],
        stringsAsFactors = FALSE
        
    )
    
    first_day_of_year <- c(
        my_weekdays[
            which(
                my_weekdays == first_day_of_year
            ):length(
                my_weekdays
            )
        ],
        rep(
            my_weekdays,
            times = ceiling(sum(n_of_days_per_months) / 7) + 1
        )
    )[
        sum(n_of_days_per_months) + 1
    ]
    
    my_table <- rbind(
        
        my_table,
        my_year_table
        
    )
    
    flush.console()
    print(
        paste(
            "Year ",
            my_year,
            " was processed.",
            sep = ""
        )
    )
    
}


## -----------------------------------------------------------------------------

print(
    dim(
        my_table[
            my_table[, "year"] %in% as.character(1901:2000) &
            my_table[, "day_of_month"] == "1" &
            my_table[, "weekday"] == "Sunday"
            ,
        ]
    )[1]
)   # 171


## -----------------------------------------------------------------------------

################################################################################
################################################################################
################################################################################





