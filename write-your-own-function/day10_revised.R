library(gapminder)
str(gapminder)

min(gapminder$lifeExp)  ## [1] 23.599
max(gapminder$lifeExp)  ## [1] 82.603
range(gapminder$lifeExp) ## [1] 23.599 82.603 gives min and max 

## some natural solutions
max(gapminder$lifeExp) - min(gapminder$lifeExp)  ## [1] 59.004
with(gapminder, max(lifeExp) - min(lifeExp))   ## [1] 59.004  calculating difference between max and min
range(gapminder$lifeExp)[2] - range(gapminder$lifeExp)[1]  ## [1] 59.004
with(gapminder, range(lifeExp)[2] - range(lifeExp)[1])  ## [1] 59.004  difference between 2nd and 1st element if range
diff(range(gapminder$lifeExp))  ## [1] 59.004 difference function for a two element vector

max_minus_min <- function(x) max(x) - min(x)
max_minus_min

max_minus_min(gapminder$lifeExp)
names(gapminder)
max_minus_min(gapminder$year)
max_minus_min(gapminder$pop)
max_minus_min(gapminder$gdpPercap)

max_minus_min(gapminder$continent)  #it works on numeric only so on factor it shows error

max_minus_min(1:10)   ## [1] 9
max_minus_min(1)
max_minus_min(runif(10000))  ## [1] 0.9980762 ###it is easier to find and set a specific simulation number
max_minus_min(runif(100))
max_minus_min(rnorm(100))   ###as number increases the diff in range also increases

max_minus_min(c(TRUE, TRUE, FALSE, TRUE, TRUE))   ## logical like as 0, 1...so diff would be 1
max_minus_min(c(0,1, 0, 0, 1))  #same res

max_minus_min(gapminder) ## hey sometimes things "just work" on data.frames!
## Error in FUN(X[[1L]], ...): only defined on a data frame with all numeric variables
max_minus_min(gapminder$country) ## factors are kind of like integer vectors, no?
## Error in Summary.factor(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, : 'max' not meaningful for factors
max_minus_min("eggplants are purple") ## i have no excuse for this one
## Error in max(x) - min(x): non-numeric argument to binary operator

#it is not working...create a new data frame and works on that adding all variable values
max_minus_min(gapminder[c('lifeExp', 'gdpPercap', 'pop')])   ## [1] 1318683072

####Check the validity of arguments: stopifnot:better way to check error

mmm <- function(x) {
  stopifnot(is.numeric(x))
  max(x) - min(x)
}

mmm(character(rnorm(10)))
mmm(gapminder)
mmm(gapminder$country)
mmm(gapminder[c('lifeExp', 'gdpPercap', 'pop')])   ## not numeric

###########assetain function to make the code more powerful
####basic stop command and showing better error message
#assertthat on CRAN and GitHub the Hadleyverse option
#ensurer on CRAN and GitHub general purpose, pipe-friendly
#assertr on CRAN and GitHub explicitly data pipeline oriented
#assertive on CRAN and Bitbucket rich set of built-in functions


mmm2 <- function(x) {
  if(!is.numeric(x)) {
    stop("I am so sorry, but this function only works for numeric input.\n", 
         "You have provided an object of class: ", class(x))
  }
  max(x) - min(x)
}

mmm2(character(rnorm(10)))
mmm2(gapminder)
mmm2(gapminder$country)
mmm2(gapminder[c('lifeExp', 'gdpPercap', 'pop')])   ## not numeric


###################work on the quantiles

quantile(gapminder$lifeExp)
quantile(gapminder$lifeExp, probs = 0.5)
median(gapminder$lifeExp)
quantile(gapminder$lifeExp, probs = c(0.25, 0.75))
boxplot(gapminder$lifeExp, plot = FALSE)$stats  ###only the boxplot statistics

the_probs <- c(0.25, 0.75)  ###define the probabilities
the_quantiles <- quantile(gapminder$lifeExp, probs = the_probs) ###setting probabilities
max(the_quantiles) - min(the_quantiles)  ###find max and min
IQR(gapminder$lifeExp) # hey, we've reinvented IQR

###Turn the working interactive code into a function of diff between quantiles

qdiff1 <- function(x, probs) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x = x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}
qdiff1(gapminder$lifeExp, probs = c(0.25, 0.75))
IQR(gapminder$lifeExp) # hey, we've reinvented IQR

qdiff1(gapminder$lifeExp, probs = c(0, 1))  ##quantiles with prob 0 and 1 means min and max
mmm(gapminder$lifeExp)  # same thing as returned from qdiff1 with prob 0 and 1


###############another way to do 
qdiff2 <- function(x, probs) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x = x, probs = probs)
quantile(x, probs = max(probs)) - quantile(x, probs = min(probs))
}

qdiff2(gapminder$lifeExp, probs = c(0.25, 0.75))

####arguments can be names as well

qdiff3 <- function(zeus, hera) {
  stopifnot(is.numeric(zeus))
  the_quantiles <- quantile(x = zeus, probs = hera)
  return(max(the_quantiles) - min(the_quantiles))
}
qdiff3(zeus = gapminder$lifeExp, hera = 0:1)

###better name

qdiff4 <- function(my_x, my_probs) {
  stopifnot(is.numeric(my_x))
  the_quantiles <- quantile(x = my_x, probs = my_probs)
  return(max(the_quantiles) - min(the_quantiles))
}
qdiff4(my_x = gapminder$lifeExp, my_probs = 0:1)

#############how the function return.......last line of the program....set what will be returned...

qdiff5 <- function(x, probs) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs)
  y <- max(the_quantiles) - min(the_quantiles)
  return(y)
}

qdiff5(x=1:10, probs=c(0,1))

####if not return is used then use this

qdiff6 <- function(x, probs) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs)
  y <- max(the_quantiles) - min(the_quantiles)
  #return(y)
}
qdiff6(x=1:10, probs=c(0,1))








