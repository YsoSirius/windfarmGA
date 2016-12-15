#' @title Crossover Method
#' @name crossover1
#' @description The crossover method, which takes the selected individuals
#' after the selection method and produces offspring through crossover and
#' permutations.
#' @export
#'
#' @importFrom gtools permutations
#'
#' @param se6 The selected individuals. (list)
#' @param u The crossover point rate. (numeric)
#' @param uplimit The upper limit of allowed permutations. Default is 300.
#' (numeric)
#' @param crossPart The crossover method. Either "EQU" or "RAN". (character)

#' @return Returns a binary coded matrix of all permutations and all grid
#' cells, 0 indicates no turbine and 1 indicates a turbine in the grid cell.
#' (matrix)
#'
#' @author Sebastian Gatscha
crossover1        <- function(se6,u, uplimit,crossPart) {
  #se6 = selec6best; crossPart=crossPart1;uplimit = CrossUpLimit #selec6best # parentsall
  #rm(selOut,se6,numel,i,a1,a2,b1,b2,a3,b3,j,c1,c2,d1,d2,c3,d3)
  #se6 = parents_new; crossPart=crossPart1;

  print(paste("crossover point rate: ",u+1))
  se6fit <- se6[[2]][1,-1]; se6 <- se6[[1]]

  se6 = se6[,-1]; parid <- sample(1:length(se6));
  z = seq(1, length(parid),2); z; all <- vector("list", length(z));

  #crossPart = "ran"; crosEquEartN = 5
  crossPart = toupper(crossPart)

  sene2fit <- vector(mode = "list",length = length(z))
  for (e in 1:length(z)) {
    #print(e)
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
      #print(paste(crosEquPartN, " equal crossover parts are generated"))
    }
    if (crossPart == "RAN"){
      ## Random Parts
      # Split the genCode in u parts, that are randomly distributed
      #u= sample(seq(6,(length(sene)/2),1),1);u
      u1 <- sort(sample(2:(length(sene)-1), u, replace=F));
      a <- splitAt(sene,u1);
      b <- splitAt(sene1,u1);
      #print(paste(length(a), " random crossover parts are generated"))
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
    print(paste("Crossover. Amount of Turbines is wrong. Fix BUG"))
    break()
  }

  print(paste("How many parental pairs are at hand: ",length(z)))
  print(paste("How many permutations are possible: ", length(z)*(2^(trunc(u)+1))))

  partaksur = ncol(nI)
  if (partaksur >= uplimit){
    partaksur = uplimit
    print(paste("Population max limit reached: ", uplimit ))
  }

  # Select only some of the available permutations. Take fitness value as prop value.
  partak <- sort(sample(1:length(nI[1,]),partaksur,prob = fitChi));
  print(paste("How many permutations are selected: ", length(partak) ))
  nI <- nI[,partak]

  return(nI)
}
