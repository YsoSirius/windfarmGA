#' @title Adjust the amount of turbines per windfarm
#' @name trimton
#' @description  Adjust the mutated individuals to the required amount of
#' turbines.
#'
#' @export
#' @importFrom dplyr select group_by summarise_each %>%
#' @importFrom utils globalVariables
#'
#' @param mut A binary matrix with the mutated individuals (matrix)
#' @param nturb A numeric value indicating the amount of required turbines
#' (numeric)
#' @param allparks A data.frame consisting of all individuals of the
#' current generation (data.frame)
#' @param nGrids A numeric value indicating the total amount of grid cells
#' (numeric)
#' @param trimForce A boolean value which determines which adjustment
#' method should be used. TRUE uses a probabilistic approach and
#' FALSE uses a random approach (logical)
#'
#' @return Returns a binary matrix with the correct amount of turbines
#' per individual (matrix)
#'
#' @author Sebastian Gatscha


##utils::globalVariables(c("Rect_ID", "Parkfitness", "AbschGesamt"));

trimton           <- function(mut, nturb, allparks, nGrids, trimForce){
  # library(dplyr);
  # mut = mut; nturb = n; allparks = allparks; nGrids = AmountGrids; trimForce =trimForce
  nGrids1 <- 1:nGrids

  lepa <- length(mut[1,])
  mut1 = list();
  for (i in 1:lepa) {
    mut1[[i]] = mut[,i]
    e = mut[,i]==1
    ## How many turbines are in the current park?
    ele = length(e[e==T]);
    ## How much turbines are there too many?
    zviel = ele - nturb;
    ## Which grid cell IDs have a turbine
    welche <- which(e==TRUE);

    trimForce <- toupper(trimForce)
    if (1==1){

      # Calculate probability, that Turbine is selected to be eliminated.
      indivprop <- dplyr::select(allparks, Rect_ID, Parkfitness, AbschGesamt);
      # Group mean wake effect and fitness value of a grid cell.
      indivprop <- indivprop %>% dplyr::group_by(Rect_ID) %>% dplyr::summarise_each(dplyr::funs(mean));

      k = 0.5

      propwelche <- data.frame(cbind(RectID=welche,Prop=rep(mean(indivprop$AbschGesamt),length(welche))));
      propexi <- indivprop[indivprop$Rect_ID %in% welche,];
      propexi <- as.data.frame(propexi)
      npt <- (1+((max(propexi$AbschGesam)-propexi$AbschGesam)/(1+max(propexi$AbschGesam))))
      npt0 <- (1+((max(propexi$Parkfitness)-propexi$Parkfitness)/(1+max(propexi$Parkfitness))))
      NewProb <- 1/(npt/(npt0^k))

      propwelche[welche %in%  indivprop$Rect_ID,]$Prop <- NewProb;

      propwelcheN <-  data.frame(cbind(RectID=nGrids1,Prop=rep(min(indivprop$AbschGesamt),length(nGrids1)))); nrow(propwelcheN)
      propexiN <- indivprop[indivprop$Rect_ID %in% nGrids1,]; nrow(propexiN)
      propexiN <- as.data.frame(propexiN)
      npt1 <- (1+((max(propexiN$AbschGesam)-propexiN$AbschGesam)/(1+max(propexiN$AbschGesam))))
      npt2 <- (1+((max(propexiN$Parkfitness)-propexiN$Parkfitness)/(1+max(propexiN$Parkfitness))))^k
      NewProb1 <- (npt1/npt2)
      propwelcheN[propwelcheN$RectID %in%  indivprop$Rect_ID,]$Prop <- NewProb1; nrow(propwelcheN)
      if (!all(propwelcheN$RectID %in%  indivprop$Rect_ID==TRUE)){
        qu <- min(NewProb1)
        propwelcheN[!propwelcheN$RectID %in%  indivprop$Rect_ID,]$Prop <- qu
      }
      propwelcheN <- propwelcheN[!propwelcheN$RectID %in% welche,];
      ## P1 - Deleting Turbines
      prob1 = propwelche$Prop;
      ## P2 - Adding Turbines
      prob2 = propwelcheN$Prop;
    }

    if (zviel != 0) {
      if (zviel > 0) {
        if (trimForce == TRUE){
          # Delete turbines with Probability
          smpra <- sort(sample(welche, zviel,replace=F,prob = prob1));
          prob1[which(welche==smpra[1])]
        } else {
          # Delete them randomly
          smpra <- sort(sample(welche, zviel,replace=F));
        }
        # Delete the 1 entry and make no turbine.
        mut1[[i]][smpra] = 0
      } else {
        if (trimForce == TRUE){
          # Add turbines with Probability
          smpra <- sort(sample(propwelcheN$RectID, (-zviel),replace=F, prob = prob2));
        } else {
          # Add turbines randomly
          smpra <- sort(sample(propwelcheN$RectID, (-zviel),replace=F));
        }
        # Assign 1 to binary code. So Turbine is created here.
        mut1[[i]][smpra] = 1;
      }
    }
  }
  mut1 <- do.call("cbind", mut1)
  return(mut1)
}
