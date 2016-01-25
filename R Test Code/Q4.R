Q4 <- function(testQ1 = FALSE, testQ2 = FALSE, testQ3 = FALSE)
{
  vmax = 10
  values = c(1:vmax)
  prob = c(1:vmax)
  values_in_new_box = c(rep(0,length(values)))
  constant = vmax/sum(values)
  prob = constant*values/vmax
  current_weight = 0
  number_of_boxes = 1
  boxes_total_weight = 0
  items = 10000
  for(i in 1:items)
  {
    random_value = sample(values, 1, TRUE, prob)
    boxes_total_weight = boxes_total_weight + random_value
    
    current_weight = current_weight + random_value
    if( current_weight > vmax )
    {
      number_of_boxes = number_of_boxes + 1
      current_weight = random_value
      values_in_new_box[random_value] = values_in_new_box[random_value] + 1
    }
  }
  
  number_of_items_per_box = items/number_of_boxes
  average_weight_per_box = boxes_total_weight/number_of_boxes
  
  values_in_new_box = values_in_new_box/number_of_boxes
  
  if(testQ1 == TRUE)
  {
    return(number_of_items_per_box)
  }
  if(testQ2 == TRUE)
  {
    return(average_weight_per_box)
  }
  if(testQ3 == TRUE)
  {
    return(values_in_new_box)
  }
}

MatrixTransitions <- function(i,j, wmax, prob)
{
  if(i >= j)
  {
    if( (i+j) <= wmax ) # i am going to state less than me but i can't start a new box
    {
      return(0)
    }
    else #i am going to a state less than me and the only way to get there is to get that weight item since i will start a new box
    {
      return(prob[j])
    }
  }
  else
  {
    if( (i+j) <= wmax )#i am going to a state ahead of me but we can't start a new box in that state so i just use a smaller weight to get there
    {
      return(prob[j-i])
    }
    else
    {
      return(prob[j-i] + prob[j])#i am going to a state ahead of me and i can get there by a small weight or by starting a new box at that weight
    }
  }
}

CreateExampleTransitionMatrix <- function(size)
{
  pij = matrix(rep(0,size*size), size, size)
  prob = c(1:size)
  constant = size/sum(prob)
  prob = constant*prob/size
  
  for(i in 1:size)
  {
    for(j in 1:size)
    {
      pij[i,j] = ExampleMatrix(i,j,size,prob)
    }
  }
  
  return(pij)
}


