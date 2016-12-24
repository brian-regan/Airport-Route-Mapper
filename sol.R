## create hubs and link them all with each other
allHubs <- c("STN", "ALC", "WMI", "CHQ", "CIA", "HHN", "BGY", "ACE", "MRS", "KRK", "BUD", "MLA", "KUN", "TMP", "RYG", "SXF", "LRH", "OPO")
edges <- data.frame("Source" = character(), "Target" = character(), stringsAsFactors = FALSE)

joinHubs <- function(hubs){
  len <- length(hubs)
  c <- 1
  for(i in (1:(len-1))){
    for(j in ((i+1):len)){
      edges[c,] <- c(hubs[i],hubs[j])
      c <- c + 1
    }
  }
  return(edges)
}

## calculate distance between two airports
dist <- function(s, t){
  LatLong <- read.csv("LatLong.csv")
  x1 <- LatLong[which(LatLong$FAA == s),"Latitude"]
  y1 <- LatLong[which(LatLong$FAA == s),"Longtitude"]
  x2 <- LatLong[which(LatLong$FAA == t),"Latitude"]
  y2 <- LatLong[which(LatLong$FAA == t),"Longtitude"]
  return(as.numeric(sqrt((x1 - x2)^2 + (y1-y2)^2))) ## CHECK THIS WITH CALCULATOR
}


## pair an outlier airport with nearest hub
nearest <- function(o, hubs){
  current <- hubs[1]
  for(hub in hubs){
    if(dist(o, hub) > dist(o, current)){
      ## do nothing
    } else {
      current <- hub
    }
  }
  return(c(o, current))
}

## remove hubs from list of airports
rem <- function(hubs, airports = read.csv("ryanairAirports.csv")){
  airports <- airports[1]
  for(hub in hubs){
    airports <- airports[airports != hub]
  }
  return(airports)
}

## pair every outlier with nearest hub
go <- function(hubs = allHubs, outliers = rem(allHubs), edges = joinHubs(allHubs)){
  c <- length(edges[,1]) + 1
  for(o in outliers){
    print(nearest(o, hubs))
    edges[c,] <- nearest(o, hubs)
    c <- c + 1
  }
  write.csv(edges, "brianAirRoutes.csv")
  return(edges)
}

## edit so that hubs is input each time
## combine airports in London (and similar situations)?
## calculate rough runtime and try to optimize?