#' @title Crossover Method
#' @name crossover1
#' @description The crossover method of the genetic algorithm, which takes
#' the selected individuals after the \code{\link{selection1}} function and
#' produces new offsprings through permutation.
#'
#' @export
#'
#' @importFrom gtools permutations
#'
#' @param se6 The selected individuals. The output of \code{\link{selection1}}
#' (list)
#' @param u The crossover point rate. (numeric)
#' @param uplimit The upper limit of allowed permutations. The current
#' algorithm has an upper bound of 300 permutations. (numeric)
#' @param crossPart The crossover method. Either "EQU" or "RAN". (character)

#' @return Returns a binary coded matrix of all permutations and all grid
#' cells, 0 indicates no turbine and 1 indicates a turbine in the grid cell.
#' (matrix)
#'
#' @examples \dontest{
#'  ## Create two random parents with an index and random binary values
#'  Parents <- data.frame(cbind(ID=1:20,bin=sample(c(0,1),20,replace=T,prob = c(70,30)),
#'                          bin.1=sample(c(0,1),20,replace=T,prob = c(30,70))))
#'  Parents
#'
#'  ## Create random Fitness values for both individuals
#'  FitParents <- data.frame(cbind(ID=1,Fitness=1000,Fitness.1=20))
#'  FitParents
#'
#'  ## Assign both values to a list
#'  CrossSampl <- list(Parents,FitParents);
#'  str(CrossSampl)
#'
#'  ## Cross their data at equal locations with 2 crossover parts
#'  crossover1(CrossSampl, u=1.1, uplimit=300, crossPart = "EQU")
#'
#'  ## with 3 crossover parts and equal locations
#'  crossover1(CrossSampl, u=2.5, uplimit=300, crossPart = "EQU")
#'
#'  ## or with random locations and 5 crossover parts
#'  crossover1(CrossSampl, u=4.9, uplimit=300, crossPart = "RAN")
#' }
#'
#' @author Sebastian Gatscha
crossover1        <- function(se6,u, uplimit,crossPart) {
  #se6 = selec6best; crossPart=crossPart1;uplimit = CrossUpLimit #selec6best # parentsall
  #rm(selOut,se6,numel,i,a1,a2,b1,b2,a3,b3,j,c1,c2,d1,d2,c3,d3)
  #se6 = parents_new; crossPart=crossPart1;

  cat(paste("crossover point rate: ",u+1))
  se6fit <- se6[[2]][1,-1]; se6 <- se6[[1]]

  se6 = se6[,-1]; parid <- sample(1:length(se6));
  z = seq(1, length(parid),2); all <- vector("list", length(z));

  #crossPart = "ran"; crosEquEartN = 5
  crossPart = toupper(crossPart)

  sene2fit <- vector(mode = "list",length = length(z))
  for (e in 1:length(z)) {
    #cat(e)
    #e=1
    r = z[[e]];r
    # Sene ist der genCode des 1ten Elternteils, Sene1 der des 2ten Elternteils
    sene <- se6[,parid[r]];
    sene1 <- se6[,parid[r+1]];

    senefit <- se6fit[,parid[r]];senefit;   sene1fit <- se6fit[,parid[r+1]];
    sene2fit[[e]] <- senefit+sene1fit/2;



    if (crossPart == "EQU"){
      ## Equal Parts
      # In how many parts should the genCode be split?
      #t <- sort(sample(numel,4,replace=F));t
      crosEquPartN <- base::trunc(u+1)
      t1 <- ceiling(length(sene)/crosEquPartN);
      #split the genCode in equal parts, that are t long. a is from parent1 and b for parent2.
      a <- base::split(sene,as.numeric(gl(length(sene),t1,length(sene)))) ;
      b <- base::split(sene1,as.numeric(gl(length(sene1),t1,length(sene1))));
      #cat(paste(crosEquPartN, " equal crossover parts are generated"))
    }
    if (crossPart == "RAN"){
      ## Random Parts
      # Split the genCode in u parts, that are randomly distributed
      #u= sample(seq(6,(length(sene)/2),1),1);u
      u1 <- sort(sample(2:(length(sene)-1), u, replace=F));
      a <- splitAt(sene,u1);
      b <- splitAt(sene1,u1);
      #cat(paste(length(a), " random crossover parts are generated"))
    }

    x1 <- rbind(a,b);      perm <- gtools::permutations(n=2,r=ncol(x1),v=1:nrow(x1),repeats.allowed=T);
    # for every possible permutation
    permut <- list()
    for (pp in 1:nrow(perm)){
      # for every col/genetic code pieces take either from parent 1(a) or parent 2(b)
      gclist <-list()
      for (gnp in 1:length(perm[pp,])){
        parent01 <- perm[pp,gnp];
        if (parent01 ==1){
          gc <- a[[gnp]];
        } else {
          gc <- b[[gnp]]
        }
        gclist[[gnp]] <- gc
      }
      permut[[pp]] <- unlist(gclist);
    }
    permut <- do.call("cbind",permut);
    all[[e]] <- permut
  }

  nuCh <- ncol(all[[1]]);
  sene2fit_n <- do.call("cbind",sene2fit);
  sene2fit_n <- (sene2fit_n/mean(sene2fit_n))

  fitChi <- rep(x = sene2fit_n,each=nuCh)

  nI <- do.call("cbind", all);

  if (length(fitChi) != ncol(nI)){
    cat(paste("\nCrossover. Amount of Turbines is wrong. Fix BUG"))
    break()
  }

  cat(paste("\nHow many parental pairs are at hand: ",length(z)))
  cat(paste("\nHow many permutations are possible: ", length(z)*(2^(trunc(u)+1))))

  partaksur = ncol(nI)
  if (partaksur >= uplimit){
    partaksur = uplimit
    cat(paste("\nPopulation max limit reached: ", uplimit ))
  }

  # Select only some of the available permutations. Take fitness value as prop value.
  partak <- sort(sample(1:length(nI[1,]),partaksur,prob = fitChi));
  cat(paste("\nHow many permutations are selected: ", length(partak), "\n"))
  nI <- nI[,partak]

  return(nI)
}




