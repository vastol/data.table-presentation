
#=====================================================================
#                 Code for data.table presentation 
#                       Vasily Tolkachev 
#                   (vasily.tolkachev@gmail.com)
#                          21.01.2016
#=====================================================================

library(MASS)
library(data.table)
library(dplyr)

data.table(Boston)
as.data.table(Boston)

data = data.table(Boston)
data

class(data)

# subset rows from 11 to 20
data[11:20]

# subset rows from 11 to 20 of variable age (result is a vector, not a data.table)
data[11:20, age]

# to get data.table after subsetting, use list() or .() in j argument 
data[11:20, list(age)]

# equivalently:
data[11:20, .(age)]

# the usual data.frame subsetting style (with columns in j argument)
data[11:20, 1, with = FALSE]

# Find all rows where tax variable is equal to 216
data[tax == 216]

# Find the range of crim (criminality) variable
data[, range(crim)]

# Display values of rad (radius) variable
data[, table(rad)]

# with with = FALSE you could also select all columns between some two
data[, indus:age, with = FALSE]

# Add a new variable with :=
data[, rad.f := as.factor(rad) ]
data[, levels(rad.f)]

# i.e. we defined a new factor variable(rad.f) in the data table
# from the integer variable radius (rad), which describes accessibility to radial highways

# Compute mean of house prices for every level of rad.f
data[, mean(medv), by = rad.f ]

# Recall that j argument is a function, so in this case it’s 
# a function calling a variable medv:
data[, medv ]

# Below it’s a function which is equal to 5
data[, 5 ]

# Select several variables (result is a data.table)
data[, list(nox, age, black)]

# Or equivalently:  
data[, .(nox, age, black)]

# Compute several functions
data[, .( mean(nox), sd(age), mad(black) )]

# Compute these functions for groups (levels) of rad.f
data[, .( mean(nox), sd(age), mad(black) ), by = rad.f]

# Compute functions for every level of rad.f and return a data.table with column names
data[, .( Var1 = mean(nox), Var2 = sd(age), Var3 = mad(black) ), by = rad.f]

# Add many new variables with `:=`().
# If a variable attains only a single value, copy it for each observation
data[, `:=`( Var1 = mean(nox), Var2 = sd(age), Var3 = mad(black) )]

# Compute a more complicated function for groups. 
# It’s a weighted mean of house prices, with dis 
# (distances to Boston employment centers) as weights
data[, sum(medv * dis)/sum(dis), by = rad.f ]
#=====================================================================


# Dynamic variable creation.
# Now let’s create a variable of weighted means (mean_w),
# and then use it to create a variable for weighted standard deviation (std_w).

data[,  `:=`(mean_w = mean_w <- sum(medv * dis)/sum(dis),
             std_w = sqrt( sum( dis * (medv - mean_w)^2 )/sum(dis) ) ),
        by = rad.f ][]

# To use some variables with long names, specify them in SDcols and use SD instead:
data[, `:=`( x = sum(.SD[[1]]^2) / sum(.SD[[1]]),
             y = sum(.SD[[2]]^2) / sum(.SD[[2]]) ),
        by = rad.f,
        .SDcols = c("medv", "age") ][]

# Multiple expressions in j could be handled with { }
par(mfrow = c(1,2))
data[, { hist(log(crim), col = "royalblue3")
         plot(rm, medv, pch = 16)
         grid()
        } ]

# Separate data.table with dynamically created variables can be done by
data[, { list(mean_w = mean_w <- sum(medv * dis)/sum(dis),
              std_w = sqrt( sum( dis * (medv - mean_w)^2 )/sum(dis) ) 
              )},
      by = rad.f ]
#=====================================================================

# Changing a subset of observations. 
# Let’s create another factor variable crim.f 
# with 3 levels standing for low, medium and severe crime rates per capita:

data[           , crim.f := "low"]
data[ crim >= 1 , crim.f := "medium"]
data[ crim >= 10, crim.f := "severe"]
data[           , crim.f := as.factor(crim.f)][]

table(data$crim.f)

# Chaining
data[ , crim.f := "low"] [ crim >= 1, crim.f := "medium"] 
data[ crim >= 10, crim.f := "severe"][, crim.f := as.factor(crim.f)]
levels(data$crim.f)

# Equivalent chaining in one command
data[ , crim.f := "low"] [ 
  crim >= 1, crim.f := "medium"] [
    crim >= 10, crim.f := "severe"][,
      , crim.f := as.factor(crim.f)]
#=====================================================================

# we can also apply functions in j on two groups
data[, .(mean(medv), sd(medv)), by = .(rad.f, crim.f) ]

# .N function count the number observations in a group:
data[, .N, by = .(rad.f, crim.f) ]

# Another useful function is .SD which contains values 
# of all variables except the one used for grouping
data[, .SD, by =  crim.f ]

# Use setnames() and setcolorder() functions to change column names or reorder them:
setnames(data, c("rm", "zn"), c("rooms_average", "proportion_zoned") )[]

# set the key
key(data)
setkey(data, rad.f, crim.f)

# use binary search (fast, O(log(n) )
data[ "7" ]
data[ .("7", "low") ]

data[ .("24", unique(crim.f)), nomatch = 0L]
data[ .(unique(rad.f), "medium"), nomatch = 0L ]


# DO NOT use vector scan (slow, O(n) )
data[rad.f =="7" & crim.f == "low"]

# Avoid using data.frame’s vector scan inside data.table:
data[ data$rad.f == "7" & data$crim.f == "low", ]

# avoid using $ inside the data.table,
# whether it’s for subsetting, or updating some subset of the observations:
data[ data$rad.f == "7", ] = data[ data$rad.f == "7", ] + 1

# data.table used to work with dplyr well, but now it is usually slow:
# best to combine it with pipeline operator %>%
data %>% filter(rad == 1)



#=====================================================================
#                       Speed Comparisons
#                 Artificial Big dataset example
# https://github.com/Rdatatable/data.table/wiki/Benchmarks-%3A-Grouping
#=====================================================================

rm(list=ls())

N = 2 * 10^7 # max allowed
K = 100

set.seed(1)
DT = data.table(
  id1 = sample(sprintf("id%03d", 1:K), N, TRUE),        # large groups (char)
  id2 = sample(sprintf("id%03d", 1:K), N, TRUE),        # large groups (char)
  id3 = sample(sprintf("id%010d", 1:(N/K)), N, TRUE),   # small groups (char)
  id4 = sample(K, N, TRUE),                             # large groups (int)
  id5 = sample(K, N, TRUE),                             # large groups (int)
  id6 = sample(N/K, N, TRUE),                           # small groups (int)
  v1 =  sample(5, N, TRUE),                             # int in range [1,5]
  v2 =  sample(5, N, TRUE),                             # int in range [1,5]
  v3 =  sample( round(runif(100, max=100), 4), N, TRUE) # numeric e.g. 23.5749
)

print(object.size(DT), units = "Mb")

system.time( DT[, sum(v1), keyby=id1] )
system.time( DT[, sum(v1), keyby="id1,id2"] )
system.time( DT[, list(sum(v1),mean(v3)), keyby=id3] )
system.time( DT[, lapply(.SD, mean), keyby=id4, .SDcols=7:9] )
system.time( DT[, lapply(.SD, sum), keyby=id6, .SDcols=7:9] )

## dplyr run

set.seed(1)
DF = data.frame(stringsAsFactors=FALSE,
                id1 = sample(sprintf("id%03d",1:K), N, TRUE),
                id2 = sample(sprintf("id%03d",1:K), N, TRUE),
                id3 = sample(sprintf("id%010d",1:(N/K)), N, TRUE),
                id4 = sample(K, N, TRUE),                          
                id5 = sample(K, N, TRUE),                         
                id6 = sample(N/K, N, TRUE),                       
                v1 =  sample(5, N, TRUE),                         
                v2 =  sample(5, N, TRUE),                       
                v3 =  sample(round(runif(100,max=100),4), N, TRUE)
)

print(object.size(DF), units = "Mb")

system.time( DF %>% group_by(id1) %>% summarise(sum(v1)) )
system.time( DF %>% group_by(id1,id2) %>% summarise(sum(v1)) )
system.time( DF %>% group_by(id3) %>% summarise(sum(v1),mean(v3)) )
system.time( DF %>% group_by(id4) %>% summarise_each(funs(mean), 7:9) )
system.time( DF %>% group_by(id6) %>% summarise_each(funs(sum), 7:9) )



