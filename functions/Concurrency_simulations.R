
##########################################
# 
# Running concurrent simulations
#
# Series:
# Little Useless-useful R functions #24
# Created: June 4, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


mutoutser <- function(links){
  nr <- nrow(links)
  nc <- ncol(links)
  tot = 0
  for (i in 1 : ( nr - 1)) {
    for (j in ( i + 1): nr) {
      for (k in 1 : nc)
        tot <- tot + links[i,k] * links[j,k]
    }
  }
  r <- tot / (nr * (nr - 1) / 2 )
 print(r)
}


sim <- function(nr,nc) {
  lnk <-  matrix(sample(0:1 , (nr*nc) , replace=TRUE) , nrow=nr)
  system.time(mutoutser(lnk))
}

sim(500,500)


## Optimised version

mutoutser1 <- function (links) {
  nr <- nrow(links)
  nc <- ncol(links)
  tot <- 0
  for ( i in 1 : ( nr - 1)) {
    tmp <- links[ ( i + 1): nr,] %*% links[i,]
    tot <- tot + sum(tmp)
  }
  
  r <- tot / (nr * (nr - 1) / 2 )
  print(r)
}


sim1 <- function(nr,nc) {
  lnk <- matrix(sample (0:1 ,(nr*nc), replace=TRUE) ,nrow=nr)
  print (system.time(mutoutser1(lnk)))
}

sim1(500,500)


# comparison
 sim(1000,1000)
sim1(1000,1000)


