# simpleEdgeListGen(numNode = 20, k = NULL, interval = 10, aveDur = NULL)
#    generate a edge list with edge numNode.


# getCrossSectionNet(nD, timeStamps = NULL, numSample = NULL)
#   generate a list of cross sectioanl network from networkDynamic object
  
  
getCrossSectionNet <- function(nD, timeStamps = NULL, numSample = NULL) {
  # if non of the timeStamps or numSample are specified, assume numSample = 10;
  
  ## get the time changes in nD
  time = get.change.times(nD)
  
  ## process the input arguments, make everything consistent with timeStamps
  if(!is.null(timeStamps) && !is.null(numSample)){
    print("warning: timeStamps and numSample are both specified. only timeStamps are using")
  }
  
  if(is.null(timeStamps)){
    
    if (is.null(numSample)){
      numSample = 10;
    }
    timeStemps = floor(seq(from = time[1], to = time[length(time)], by = time[length(time)]/(numSample-1)))
  }
  
  
  ## make a list of network objects
  net.list = list()
  count = 1;
  for(i in timeStemps){
    if ( (i < time[1]) || (i > time[length(time)])){
      print("warning: time point request is out of range")
    }
    net.list[[count]] = network.collapse(nD, at = i)
    count = count + 1
  }
  
  return(net.list) 
}














simpleEdgeListGen <- function(numNode = 20, k = NULL, interval = 10, aveDur = NULL){
  
  ## Generate a simple edge list for nD:
  ## numNode : default 20.
  ## k (eg 6) : controls the likely number of partners
  ## interval (eg  100): controls how many time unit should the ties be generated over.
  ##aveDur (eg 13): set for average duration of a tie.
  
  if (is.null(aveDur)){
    aveDur = round(interval / 9);
  }
  if (is.null(k)){
    aveDur = round(numNode / 4);
  }
  
  # initialize the adjacency matrix.
  edge.spells = c(onset = 1, termini = 1, tail = 1, head =1)
  
  for (i in 1 : numNode){
    numTies = floor(runif(1, min=0.7, max=k+0.9)); # number of ties: makes it more likely for k alters, less likely 0 alters.
    
    # who this alter is tied with.
    tiedWith = round(runif(numTies, min = 0.5, max = numNode + 0.5))
    
    # calculate the ties. 
    onset = round(runif(numTies,min = 0, max = interval)) ; # onset of each tie. 
    dur = round(rnorm(numTies,sqrt(aveDur),1)^2)  # randomize duration
    termini = onset + dur 
    
    # form the format: [onset, terminus, tail_vertex.id, head_vertex.id]
    tail = tiedWith;
    head =  rep(i, numTies);
    this.spells = t( matrix(rbind(onset, termini, tail, head), nrow = 4))
    edge.spells = rbind(edge.spells, this.spells)
  }
  
  edge.spells = edge.spells[-1,]
  return(edge.spells)
}



