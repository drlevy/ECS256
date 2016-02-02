ProblemCSimulation <- function(servers, queue_size, portion_complex, arrival_rate, service_rate, m_service_rate, customer_leave_rate, output_missed_customers = FALSE, output_customers_left = FALSE)
{
  cubed = (queue_size+1)^3
  
  pij = array(0, c(cubed, cubed))
  qi = array(0,cubed)
  invalid_rows = 0
  
  for(i in 0:(queue_size))
  {
    for(j in 0:queue_size)
    {
      for(k in 0:(queue_size))
      {
        give_to_i = FALSE
        take_from_i = FALSE
        
        give_to_j = FALSE
        take_from_j = FALSE
        
        give_to_k = FALSE
        take_from_k = FALSE
        
        complex_give_to_j = FALSE
        
        state_i = GenerateStateIndex(i,j,k,queue_size)
        pij[state_i, state_i] = 0
        qi[state_i] = GenerateProbability(i,j,k,queue_size, arrival_rate, service_rate, m_service_rate, customer_leave_rate)
        sum_prob = 0
        if( qi[state_i] != 0.0 )
        {
          sum_prob = 1.0/qi[state_i]
        }
        
         #state j to state j is negative of the holding time of the state
        
        if((i > servers) || ((0 < k) && (i != servers)) || ((i+j+k) > queue_size) )
        {
          invalid_rows = union(invalid_rows, c(state_i))
        }
        else
        {
          if( i + j + k == queue_size )
          {
            
            if( i != 0 )
            {
              if( k == 0 )
              {
                take_from_i = TRUE
              }
              else
              {
                complex_give_to_j = TRUE
              }
            }
            
            if( j != 0 )
            {
              take_from_j = TRUE
            }
            
            if( k != 0 )
            {
              take_from_k = TRUE
            }
            
          }
          else if( i + j + k < queue_size )
          {
            if( k == 0 )
            {
                if( i != servers )
                {
                  give_to_i = TRUE
                  give_to_j = TRUE
                }
                else
                {
                  give_to_k = TRUE
                }
               
                
                if( i != 0 )
                {
                  take_from_i = TRUE
                }
                
                if( j != 0 )
                {
                  take_from_j = TRUE
                }
            }
            else if( (k > 0) && (i == servers)) # can't have a queue with servers available
            {
              #i,k are both > 0
              if( j != 0 )
              {
                take_from_j = TRUE
              }
              
              if( k != 0 )
              {
                take_from_k = TRUE
                complex_give_to_j = TRUE
              }
              
              give_to_k = TRUE
            }
          }
        }
        
        if(take_from_i == TRUE)
        {
          state_j = GenerateStateIndex(i-1, j, k, queue_size)
          pij[state_i,state_j] = i*(service_rate * sum_prob)
        }
        
        if(give_to_i == TRUE)
        {
          state_j = GenerateStateIndex(i+1, j, 0, queue_size)
          pij[state_i,state_j] = (arrival_rate * sum_prob)*(1.0-portion_complex)
        }
        
        if(take_from_j == TRUE)
        {
          state_j = GenerateStateIndex(i, j-1, k, queue_size)
          pij[state_i,state_j] = (m_service_rate * sum_prob)
        }
        
        if(give_to_j == TRUE)
        {
          state_j = GenerateStateIndex(i, j+1, k, queue_size)
          pij[state_i,state_j] = (arrival_rate * sum_prob)*portion_complex
        }
        
        if(take_from_k == TRUE)
        {
          state_j = GenerateStateIndex(i, j, k-1, queue_size)
          pij[state_i,state_j] = ((customer_leave_rate +(i*service_rate*(1.0-portion_complex))) * sum_prob)
        }
        
        if(give_to_k == TRUE)
        {
          state_j = GenerateStateIndex(i, j, k+1, queue_size)
          pij[state_i,state_j] = (arrival_rate * sum_prob)
        }
        
        if(complex_give_to_j == TRUE)
        {
          for(z in 2:queue_size)
          {
            if( (k - z) >= 0 )
            {
              state_j = GenerateStateIndex(i, j+z-1, k-z, queue_size)
              pij[state_i,state_j] = sum_prob*i*service_rate*(portion_complex^(z-1))*(1.0-portion_complex)
            }
          }
          
          state_j = GenerateStateIndex(i-1, j+k, 0, queue_size)
          pij[state_i,state_j] = sum_prob*i*service_rate*(portion_complex^k)
        }
        
      }
    }
  }
        
  for(i in length(invalid_rows):1)
  {
    if( invalid_rows[i] != 0 )
    {
      pij = pij[-invalid_rows[i],]
      pij = pij[,-invalid_rows[i]]
      qi = qi[-invalid_rows[i]]
    }
  }
  
  for(i in 1:length(pij[,1]))
  {
    sum = sum(pij[i,])
    if(sum < 0.999)
    {
      stop_here = TRUE
    }
  }
  
  markov = mc(pij,qi)
  pis = stn(markov)
  set_left = setdiff(1:cubed, invalid_rows)
  sum = 0
  
  for(i in 1:length(set_left))
  {
    if(IsIndexEqualToB( set_left[i], queue_size) == TRUE)
    {
      sum = sum + pis[i]
    }
  }
  
  get_call = arrival_rate/(arrival_rate + service_rate + m_service_rate + customer_leave_rate)
  if( output_missed_customers == TRUE)
  {
    return(sum*get_call)
  }
  
  sum = 0
  
  for(z in 1:length(set_left))
  {
    dim = queue_size
    index = set_left[z] - 1
    
    i = trunc( index / ((dim+1)*(dim+1)) )
    index = index - (i*((dim+1)*(dim+1)))
    
    j = trunc( index / (dim+1) )
    index = index - (j*(dim+1))
    
    k = index
    
    if(k > 0 || j > 0)
    {
      sum = sum + pis[z]*(k+j)
    }
  }
  
  leave_line = customer_leave_rate/(arrival_rate + service_rate + m_service_rate + customer_leave_rate)
  if( output_customers_left == TRUE )
  {
    return(sum*leave_line)
  }
  
  
  return(markov)
}

IsIndexEqualToB <- function(index, dim)
{
  index = index - 1
  i = trunc( index / ((dim+1)*(dim+1)) )
  index = index - (i*((dim+1)*(dim+1)))
  
  j = trunc( index / (dim+1) )
  index = index - (j*(dim+1))
  
  k = index
  if((i + j + k) == dim)
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}


GenerateStateIndex <- function(i,j,k, dim)
{
  dim = dim + 1
  return_value =(i*(dim)*(dim) + j*(dim) + k) + 1
  return(return_value)
}

GenerateProbability <- function(i, j, k, b, arrival_rate, service_rate, m_service_rate, customer_leave_rate)
{
  if( i + j + k > b)
  {
    return(0)
  }
  
  sum = 0
  if( i != 0 )
  {
    sum = sum + i*service_rate
  }
  if( j != 0 )
  {
    sum = sum + m_service_rate
  }
  if( k != 0)
  {
    sum = sum + customer_leave_rate
  }
  
  if(( i + j + k ) != b)
  {
    sum = sum + arrival_rate
  }
  
  return(sum)
}
