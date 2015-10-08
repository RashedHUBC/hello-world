library(gapminder)
str(gapminder)

## get to know the functions mentioned above
min(gapminder$lifeExp)  ## [1] 23.599
max(gapminder$lifeExp)  ## [1] 82.603
range(gapminder$lifeExp) ## [1] 23.599 82.603

## some natural solutions
max(gapminder$lifeExp) - min(gapminder$lifeExp)  ## [1] 59.004
with(gapminder, max(lifeExp) - min(lifeExp))   ## [1] 59.004
range(gapminder$lifeExp)[2] - range(gapminder$lifeExp)[1]  ## [1] 59.004
with(gapminder, range(lifeExp)[2] - range(lifeExp)[1])  ## [1] 59.004
diff(range(gapminder$lifeExp))  ## [1] 59.004

max_minus_min <- function(x) max(x) - min(x)
max_minus_min(gapminder$lifeExp)

max_minus_min(1:10)   ## [1] 9
max_minus_min(runif(1000))  ## [1] 0.9980762

max_minus_min(gapminder$gdpPercap)   ## [1] 113282
max_minus_min(gapminder$pop)   ## [1] 1318623085

max_minus_min(gapminder) ## hey sometimes things "just work" on data.frames!
## Error in FUN(X[[1L]], ...): only defined on a data frame with all numeric variables
max_minus_min(gapminder$country) ## factors are kind of like integer vectors, no?
## Error in Summary.factor(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, : 'max' not meaningful for factors
max_minus_min("eggplants are purple") ## i have no excuse for this one
## Error in max(x) - min(x): non-numeric argument to binary operator

max_minus_min(gapminder[c('lifeExp', 'gdpPercap', 'pop')])   ## [1] 1318683072
max_minus_min(c(TRUE, TRUE, FALSE, TRUE, TRUE))   ## [1] 1

####Check the validity of arguments: stopifnot

mmm <- function(x) {
  stopifnot(is.numeric(x))
  max(x) - min(x)
}
mmm(gapminder)
## Error: is.numeric(x) is not TRUE
mmm(gapminder$country)
## Error: is.numeric(x) is not TRUE
mmm("eggplants are purple")
## Error: is.numeric(x) is not TRUE
mmm(gapminder[c('lifeExp', 'gdpPercap', 'pop')])
## Error: is.numeric(x) is not TRUE
mmm(c(TRUE, TRUE, FALSE, TRUE, TRUE))
## Error: is.numeric(x) is not TRUE

###if then stop

mmm2 <- function(x) {
  if(!is.numeric(x)) {
    stop('I am so sorry, but this function only works for numeric input!')
  }
  max(x) - min(x)
}
mmm2(gapminder)
## Error in mmm2(gapminder): I am so sorry, but this function only works for numeric input!

###Packages for formal checks at run time: 

## install if you do not already have!
## install.packages("assertthat")
library(assertthat)
mmm3 <- function(x) {
  assert_that(is.numeric(x))
  max(x) - min(x)
}
mmm3(gapminder)
## Error: x is not a numeric or integer vector

##other uses for assertthat or ensurer

assert_that(nrow(gapminder) == 1704)






