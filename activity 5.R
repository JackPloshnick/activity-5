#### Class activity 5

## 2/8/18


Hogwarts_Names <-  c("name")

Hogwarts_Names <- as.list(Hogwarts_Names)

class(Hogwarts_Names) <- "student"

class(Hogwarts_Names)

make_values <- function(name){
  UseMethod("student", name)
}


make_values.student <- function(name){
  name$courage = sample(1:100,1)
  name$ambition = sample(1:100,1)
  name$intellegence = sample(1:100,1)
  name$effort = sample(1:100,1)
  return(name)
}


make_values.student(Hogwarts_Names)


##### Part 2

thingy <- matrix(c(1:16),
                 nrow=4)
matrix

?sort

sort.student <- function(x,decreasing = FALSE, na.last = NA, matrix){
  a = c(x$courage, x$ambition, x$intellegence, x$effort) 
  something = t(matrix) %*% a
  return(something)
}

sort.student(Hogwarts_Names,decreasing = FALSE, na.last = NA, thingy)











