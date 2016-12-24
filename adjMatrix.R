adjMatrix <- function(){
  names <- read.csv("ryanairAirports.csv")
  len = length(names[,1])
  a <- matrix(0, nrow = len, ncol = len)
  rownames(a) <- names[,1]
  colnames(a) <- names[,1]
  pairs <- unique(read.csv("brianAirRoutes.csv", stringsAsFactors = FALSE))
  for(i in 1:length(pairs[,1])){
    a[pairs[i,2],pairs[i,1]] <- -1
    a[pairs[i,1],pairs[i,2]] <- -1
    a[pairs[i,1],pairs[i,1]] <- length(pairs[pairs == pairs[i,1]])
    a[pairs[i,2],pairs[i,2]] <- length(pairs[pairs == pairs[i,2]])
  }
  return(a)
}