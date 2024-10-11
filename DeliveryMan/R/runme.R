library(DeliveryMan)

manhattan <- function(from, to) {
  #calculates and returns the manhattan distance
  return(abs(from$x - to$x) + abs(from$y - to$y))
}
  
FindPackage <- function(car, packages) {
  #find the cheapest package to collect based on its manhattan distance
  d = Inf
  for (i in seq_along(packages[,1])) {
    if (packages[i,5] == 0){
      package = list(x = packages[i, 1], y = packages[i, 2])
      deliver = list(x = packages[i, 3], y = packages[i, 4])
      distance = manhattan(car, package)
      if (distance < d) {
        d = distance
        index = i
      }
    }
  }
  return(list(x = packages[index, 1], y = packages[index, 2]))
}
  
ifVisited <- function(node, visited) {
  # checks if a certain node has already been visited (i.e., expanded)
  for (i in seq_along(visited)) {
    if (node[1] == visited[[i]]$x && node[2] == visited[[i]]$y) {
      return(TRUE)
    } else {
      next
    }
  }
  return(FALSE)
}
  
findG <- function(from, to, road) {
  # determines if we want to check horizontal or vertical g cost for a node
  if (from$x != to$x & from$y == to$y) {                  
    g = Hori_Cost(from, to, road)
  } 
  else if (from$y != to$y & from$x == to$x) {                  
    g = Vert_Cost(from, to, road) 
  }
  return(g)
}
  
  
Vert_Cost = function(from, to, road) {
  # returns the vertical cost (g)
  if (from$y > to$y) {
    return(road$vroads[from$x, from$y-1])
  } else {
    return(road$vroads[from$x, from$y])
  }
}
  
Hori_Cost <- function(from, to, road) {
  # returns the horizontal cost (g)
  if (to$x < from$x) {
    return(road$hroads[from$x-1, from$y])
  } else {
    return(road$hroads[from$x, from$y])
  }
}
  
neighbours <- function(expanded, road) {
  # creates all the neighbours of a node, as long as they exists on the game field.
  xDim = dim(road$hroads)[2]
  yDim = dim(road$vroads)[1]
  x = expanded$x
  y = expanded$y
  neighbours = matrix(, nrow = 4, ncol = 2, byrow = TRUE)
    
  neighbours[, 1] = c(x - 1, x, x, x + 1)
  neighbours[, 2] = c(y, y + 1, y - 1, y)
    
  neighbours = neighbours[neighbours[, 1] > 0, ]
  neighbours = neighbours[neighbours[, 2] > 0, ]
  neighbours = neighbours[neighbours[, 1] <= xDim , ]
  neighbours = neighbours[neighbours[, 2] <= yDim , ]
    
  return(neighbours)} 
  
FindMove <- function(expanded, neighbour) {
  #returns which move the car wants to make
  if (expanded$x < neighbour$x) {
    return(6)
  } 
  if (expanded$x > neighbour$x) {
    return(4)
  } 
  if (expanded$y < neighbour$y){
    return(8)
  } 
  if (expanded$y > neighbour$y) {
    return(2)
  }
}
  
addnode <- function(frontier, expanded, g, neighbour, ToGo) {
  #adds a node to the frontier
  h = manhattan(neighbour, ToGo)
  move = FindMove(expanded, neighbour)
  XX = sapply(frontier, function(item)item$x)   # creates list of all the x values of the frontier
  YY = sapply(frontier, function(item)item$y)   # creates a list of all the y values of the frontier
  index = which(neighbour$x == XX & neighbour$y == YY) # returns the index if a neighbour exists in the frontier already
  if (length(index) != 0) { # if the index was not 0 (aka it exists in the frontier)
    if (frontier[[index]]$g >= g) {  # if the new path to the neighbour is cost the same or less
      frontier[[index]]$g = g    # we add it to the frontier
      frontier[[index]]$f = g + h
      frontier[[index]]$route = append(expanded$route, c(move))
    }
  } else {   # if the neighbour was not in the frontier at all, we add it
    frontier = append(frontier, list(list(
      x = neighbour$x,
      y = neighbour$y,
      g = g,
      h = h,
      f = g + h,
      route = append(expanded$route, c(move))
  )))
  }
  return(frontier)  
}

#A-star Search algorithm
astar <- function(road, car, ToGo) {
  h = manhattan(car, ToGo)
  frontier = list(list(
    x = car$x,
    y = car$y,
    g = 0,
    h = h,
    f = h,
    route = list()
  ))
  visited = list()
  
  # "eternity loop"
  while (TRUE) {
    cheapest = sapply(frontier, function(item)item$f) #list of all frontiers f values
    cheapest_index = which.min(cheapest)  # index of the cheapest index of the frontier
    expanded = frontier[[cheapest_index]] # add the cheapest from the frontier to the expanded
    frontier = frontier[-cheapest_index]  # remove the cheapest node from the frontier
    visited = append(visited, list(expanded))    # add the cheapest to the visited set now when it has been expanded
    
    neighbours = neighbours(expanded, road) # creates neighbours
      
    if (expanded$x == ToGo$x & expanded$y == ToGo$y) {    # if we are at the goal
      return(expanded$route[1])
    }
    for (i in seq_along(neighbours[, 1])) {    # for every neighbour
      neighbour = list(x = neighbours[i,1], y = neighbours[i, 2])
      if (ifVisited(neighbour, visited) == TRUE) {   # if it has been visited
        next    # skip it
      } 
      else {   # add it to the frontier
        g = expanded$g + findG(expanded, neighbour, road)
        frontier = addnode(frontier, expanded, g, neighbour, ToGo)
      }
    }
  }
}

# my function, runs the A star search and returns moves to the game.
myFunction <- function(road, car, packages) {
  if (car$load == 0) {
    ToGo = FindPackage(car, packages)
  } else {
    ToGo = list(x = packages[car$load, 3], y = packages[car$load, 4])
  } 
  if (car$x == ToGo$x & car$y == ToGo$y) {
    car$nextMove = 5
    return(car)
  }
  car$nextMove = astar(road, car, ToGo)
  return(car)
}
######### RESULT ######################
# on one of our machines we recieved: #
# mean = 173.306                      #
# Std Dev = 38.29592                  #
# Time taken : 72.02762               #
#######################################