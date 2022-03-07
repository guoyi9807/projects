lgrid <- matrix(NA, nrow = 8, ncol = 8)
lgrid[1,] <- c("r", "l", "q", "s", "t", "z", "c", "a")
lgrid[2,] <- c("i", "v", "d", "z", "h", "l", "t", "p")
lgrid[3,] <- c("u", "r", "o", "y", "w", "c", "a", "c")
lgrid[4,] <- c("x", "r", "f", "n", "d", "p", "g", "v")
lgrid[5,] <- c("h", "j", "f", "f", "k", "h", "g", "m")
lgrid[6,] <- c("k", "y", "e", "x", "x", "g", "k", "i")
lgrid[7,] <- c("l", "q", "e", "q", "f", "u", "e", "b")
lgrid[8,] <- c("l", "s", "d", "h", "i", "k", "y", "n")

# Part 1
# Convert the letters of abscissa to numbers for later search.
change_character_to_number <- function(current_area_x){
  coordinate = current_area_x
  switch(
    coordinate,
    "A" = current_area_x <- 1,
    "B" = current_area_x <- 2,
    "C" = current_area_x <- 3,
    "D" = current_area_x <- 4,
    "E" = current_area_x <- 5,
    "F" = current_area_x <- 6,
    "G" = current_area_x <- 7,
    "H" = current_area_x <- 8
  )
}

# Count the frequency of each letter in square, 
# because my rule is that the first three letters in the collection can only hold letters with a frequency of 4 or 3.
letter <- as.vector(t(lgrid))
letter_frequency <- rep(0, times = 64)
for (i in 1:length(letter)){
  for (j in 1:length(letter)) {
    if(letter[j] == letter[i]){
      letter_frequency[i] = letter_frequency[i] + 1
    }
  }
}

# Checks if the current letter position is at the board boundary.
check_border <- function(current_area_x, current_area_y){
  if (current_area_x - 1 == 0 || current_area_y - 1 == 0){
    return(TRUE)
  }else if(current_area_x + 1 == 9 || current_area_y + 1 == 9){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

# If the current letter position is at the boundary, move the abscissa value to any position.
move_x <- function(current_area_x){
  current_area_x = sample(c(1:8), size = 1)
  return(current_area_x)
}

# If the current letter position is at the board boundary, move the ordinate value to any position.
move_y <- function(current_area_y){
  current_area_y = sample(c(1:8), size = 1)
  return(current_area_y)
}

# If the current letter position is not on the board boundary,
# the next time you move the abscissa, move it to any of the eight surrounding squares.
move_to_neighbor_x <-function(current_area_x){
  move <- sample(LETTERS[1:8] , size = 1)
  switch (move,
          "A" = current_area_x <- current_area_x - 1, 
          "B" = current_area_x <- current_area_x - 1,
          "C" = current_area_x <- current_area_x - 1,
          "D" = current_area_x <- current_area_x,
          "E" = current_area_x <- current_area_x,
          "F" = current_area_x <- current_area_x + 1,
          "G" = current_area_x <- current_area_x + 1,
          "H" = current_area_x <- current_area_x + 1,
  )
  return(current_area_x)
}

# If the current letter position is not on the board boundary,
# the next time you move the ordinate, move it to any of the eight surrounding squares.
move_to_neighbor_y <-function(current_area_y){
  move <- sample(LETTERS[1:8] , size = 1)
  switch (move,
          "A" = current_area_y <- current_area_y - 1,
          "B" = current_area_y <- current_area_y,
          "C" = current_area_y <- current_area_y + 1,
          "D" = current_area_y <- current_area_y - 1,
          "E" = current_area_y <- current_area_y + 1,
          "F" = current_area_y <- current_area_y - 1,
          "G" = current_area_y <- current_area_y,
          "H" = current_area_y <- current_area_y + 1,
  )
  return(current_area_y)
}

# Check whether the current location square is green.
check_green <- function(current_area_x, current_area_y){
  if (current_area_x == 2 && current_area_y == 6){
    return(TRUE)
  }else if (current_area_x == 3 && current_area_y == 7){
    return(TRUE)
  }else if (current_area_x == 6 && current_area_y == 2){
    return(TRUE)
  }else if (current_area_x == 7 && current_area_y == 3){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

# Part 2
# My rule is to select the first three letters randomly, 
# but only the letters that appear four or three times could be selected,
# because this would reduce the number of searches for the next two letters.

#The last two letters are discussed separately:
#  1. If the first three letters are different, for example, ABC, 
#     the last two letters must be selected from the first three letters, for example, AB, AC, and BC.
#  2. If two of the first three letters are the same, for example, AAB, 
#     the last two letters can be selected in three situations: 
#     1) The first case is that the fourth letter is the same as the frequency of 1 in the first three letters, for example, AABB, 
#     and the last letter can be selected randomly. 
#     2) In the second case, the fourth letter has the same frequency of 2 as the first three letters, such as AABA, 
#     and the last letter must be B. 
#     3) In the third case, the fourth letter does not appear in the first three cases, such as AABC,
#     and the last letter must not have been repeated before, such as B or C.
#  3. If the first three letters are the same, for example, AAA, there are two scenarios for selecting the last two letters. 
#     In the first case, the fourth letter is the same as the first three, such as AAAA, 
#     and the last letter is chosen arbitrarily. 
#     In the second case, the fourth letter is chosen arbitrarily, such as AAAB, 
#     while the fifth letter could only be chosen from the first four letters, such as AAABB or AAABA.

# To facilitate searching, create a temp_collection here that holds the collection after the repeated letters
# have been removed from the collection.

# The following is an implementation that translates the above rules into concrete code.
number_of_moves <- function(current_area_x, current_area_y,p){
  current_area_x <- change_character_to_number(current_area_x)
  palindrome_flag <- FALSE
  collection <- c()
  temp_collection <- c()
  move = 0
  while(palindrome_flag == FALSE){
    # Gets the coordinates of the current position and the corresponding value.
    value <- lgrid[current_area_x, current_area_y]
    freq <- letter_frequency[(current_area_x - 1) * 8 + current_area_y]
    cat("Current x: ", LETTERS[current_area_x], "\t", "Current y: ", current_area_y, "\t", "Value: ",value,"\n")
    # Checks whether the current position of square is green.
    if(check_green(current_area_x, current_area_y) == TRUE){
      different_operation = sample(1:2, size= 1, prob= c(p,1-p))
      if (different_operation == 1){
        collection <- c("f", "f", "h", "k")
        temp_collection <- c("h","k")
      }else{
        collection <- collection[collection!= value]
        if(is.element(value, temp_collection)){
          temp_collection <- temp_collection[temp_collection != value]
        }
      }
    }
    
    # Add the first three values according to their frequency.
    if(length(collection) < 3){
      if(freq == 4 || freq == 3){
        collection <- c(collection, value)
        # Store value to temp_collection and remove duplicate elements.
        if(is.element(value, temp_collection)) {
          temp_collection <- temp_collection[temp_collection != value] 
        } else {
          temp_collection <- c(temp_collection, value)
        }
      }
    }
    
    # Add the fourth value
    else if (length(collection) == 3){
      # The first three letters are all different ,for example abc.
      if(collection[1] != collection[2] && collection[1] != collection[3] && collection[2] != collection[3]){
        if(is.element(value, collection)){
          collection <- c(collection, value)
          if(is.element(value, temp_collection)) {
            temp_collection <- temp_collection[temp_collection != value] 
          } else {
            temp_collection <- c(temp_collection, value)
          }
        }
      }
      # The first three letters are the same, for example aaa.
      else if(collection[1] == collection[2] && collection[1] == collection[3] && collection[2] == collection[3]){
        collection <- c(collection, value)
        if(is.element(value, temp_collection)) {
          temp_collection <- temp_collection[temp_collection != value] 
        } else {
          temp_collection <- c(temp_collection, value)
        }
      }
      # Two of the first three letters are the same, for example aab.
      else{
        collection <- c(collection, value)
        if(is.element(value, temp_collection)) {
          temp_collection <- temp_collection[temp_collection != value] 
        } else {
          temp_collection <- c(temp_collection, value)
        }
      }
    }
    
    # Add the fifth value.
    # After the duplicate element is deleted, there are only three cases for the element in the temp_collection.
    else if(length(collection) == 4){
      if(length(temp_collection) == 2){
        if(is.element(value, temp_collection)){
          collection <- c(collection, value)
        }
      }
      else if(length(temp_collection) == 1){
        if(is.element(value, collection)){
          collection <- c(collection, value)
        }
      }
      else if(length(temp_collection) == 0){
        collection <- c(collection, value)
      }
    }
    if(length(collection) == 5){
      palindrome_flag <- TRUE
    }
    else{
      palindrome_flag <- FALSE
    }
#    palindrome_flag <- if(length(collection) == 5) ? break else FALSE
    cat("Collection", collection, "\n")
    move = move + 1
    
    # Checks if the current position is at the checkerboard boundary.
    if(check_border(current_area_x, current_area_y)){
      current_area_x <- move_x(current_area_x)
      current_area_y <- move_y(current_area_y)
    }else{
      temp_x = current_area_x
      temp_y = current_area_y
      current_area_x <- move_to_neighbor_x(current_area_x)
      current_area_y <- move_to_neighbor_y(current_area_y) 
      # Avoid moving to the current position when moving to the surrounding square.
      while(temp_x == current_area_x && temp_y == current_area_y){
        current_area_x <- move_to_neighbor_x(current_area_x)
        current_area_y <- move_to_neighbor_y(current_area_y)
      }
    }
  }
  cat("Total moves: ", move, "\n")
  cat("Palindrome: ", collection, "\n")
  return(move)
}


# Part 3
# Start with a random, non-edge white square to start and run 1000 experiments with probability 0.95.
get_start_x <- function(){
  x <- sample(c(1:8), size = 1)
  return(x)
}

get_start_y <- function(){
  y <- sample(c(1:8), size = 1)
  return(y)
}

current_area_x <- get_start_x()
current_area_y <- get_start_y()

while (check_border(current_area_x, current_area_y)) {
  while (check_green(current_area_x, current_area_y)) {
    current_area_x <- get_start_x()
    current_area_y <- get_start_y()
  }
}

move_count <- replicate(1000, number_of_moves(current_area_x, current_area_y , 0.05))
hist(move_count, main ="", xlab = "", mar=c(2,2,1,1), 
     mgp=c(1.2, 0.3, 0))
# Part 4
# Start with D4 and separately run 1000 experiments with probability 0.1, 0.3, 0.5, 0.7, 0.9 and calculate the average number of moves.
current_area_x = "D"
current_area_y = 4
prob<-c(0.1,0.3,0.5,0.7,0.9)
average_moves<-c()
for (i in 1:length(prob)) {
  p=prob[i]
  num_moves <- replicate(1000, number_of_moves(current_area_x, current_area_y, p))
  average_moves<-c(average_moves,mean(num_moves))
}
for (j in 1:length(average_moves)){
  cat("Average number of moves with probability ",prob[j]," :", 
      average_moves[j],"\n")
}

# Part 5
move_count_D4 <- replicate(1000, number_of_moves("D", 4, 0.95))
move_count_F6 <- replicate(1000, number_of_moves("F", 6, 0.05))
move_count <- c(move_count_D4, move_count_F6)
for (k in 1:length(move_count)) {
  if(move_count[k] <= 20){
    group1 <- c(move_count[k])
  } else if (move_count[k] > 20 && move_count[k] <= 40){
    group2 <- c(move_count[k])
  }else if (move_count[k] > 40 && move_count[k] <= 60){
    group3 <- c(move_count[k])
  }else if (move_count[k] > 60 && move_count[k] <= 80){
    group4 <- c(move_count[k])
  }else{
    group5 <- c(move_count[k])
  }
}
(chi_results <- chisq.test(x = matrix(c(group1, group2, group3, group4, group5, ncol = 1))))

# Part 6
moves_A <- c(25,13,16,24,11,12,24,26,15,19,34)
moves_B <- c(35,41,23,26,18,15,33,42,18,47,21,26)

t.test(x = moves_A, y = moves_B, alternative = "two.sided", paired = F)
