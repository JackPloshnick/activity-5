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

sorting_matrix <- matrix(c(4,3,2,1,3,4,2,1,1,2,4,3,2,1,3,4),
                 nrow=4)
print(sorting_matrix)

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











