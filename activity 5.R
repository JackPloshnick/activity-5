#### Class activity 5

## 2/8/18


make_values <- function(name){
  UseMethod("student", name)
}


make_values.student <- function(name){
  courage = sample(1:100,1)
  ambition = sample(1:100,1)
  intellegence = sample(1:100,1)
  effort = sample(1:100,1)
  a = c(courage, ambition, intellegence, effort)
  class(a) = "student"
  return(a)
}

jacob <- make_values.student()

print(jacob)

class(jacob)

##### Part 2

sorting_matrix <- diag(4)


?sort

sort.student <-function(x){
  x <- make_values.student() 
   sorted = t(sorting_matrix) %*% x
  if(max(sorted) == sorted[1]){
    print("GRYFFINDOR")
  }
  if(max(sorted) == sorted[2]){
    print("SLYTHERIN")
  }
  if(max(sorted) == sorted[3]){
    print("RAVENCLAW")
  }
  if(max(sorted) == sorted[4]){
    print("HUFFLEPUFF")
  }
}

sort.student("jacob")


### Part 3


sort.student_part3 <-function(x){
  x <- make_values.student() 
  sorted = t(sorting_matrix) %*% x
  if(max(sorted) == sorted[1]){
    class(x) = c("student", "GRYFFINDOR")
    return(x)
  }
  if(max(sorted) == sorted[2]){
    class(x) = c("student", "SLYTHERIN")
    return(x)
  }
  if(max(sorted) == sorted[3]){
    class(x) = c("student", "RAVENCLAW")
    return(x)
  }
  if(max(sorted) == sorted[4]){
    class(x) = c("student", "HUFFLEPUFF")
    return(x)
  }
}



smith <- sort.student_part3()

class(smith)

###### part 4
 "Gryffindor_Tower" <- new.env()
"Black_Lake" <- new.env()
"Ravenclaw_Tower" <- new.env()
"Basement" <- new.env()


curfew <- function(x){
  UseMethod("curfew",x)
}

curfew.GRYFFINDOR <- function(x){
Gryffindor_Tower$student <- x
  }

curfew.SLYTHERIN <- function(x){
  Black_Lake$student <- x
}

curfew.RAVENCLAW <- function(x){
  Ravenclaw_Tower$student <- x
}

curfew.HUFFLEPUFF <- function(x){
  Basement$student <- x
}


curfew(smith)

ls.str(Basement)





