library(dplyr)
library(tidyr)
library(tibble)

#Load the CLOSERg data
CLOSERg
colnames(CLOSERg) <- c("id","t", "gender", "a","l")

#Naive analysis of the data
for (s in 0:1) {
  for (w in 0:1) {
    for (t in 0:1) {
      print(paste("For Gender = ", s, "Work = ", w, "and Time = ", t, "Avg M Score = ", 
                  mean(CLOSERg[which(CLOSERg$gender == s & CLOSERg$a == w & CLOSERg$t == t),]$l)))
    }
  }
}


#Returns the residual standard error of a given regression model
RStanErr <- function(model) {
  RSE <- sqrt(deviance(model)/df.residual(model))
  return(rnorm(1,0,RSE))
}

#G-Computation Algorithm
gform <- function(data) {
  data <- select(data, id, gender, t, l, a)
  
  #Preparing a vector of all possible interventions
  apos <- rep(list(0:1), max(select(data, t)))
  
  #Creating a data frame to contain the data that will be used for the final regression
  findf <- data.frame() 
  
  #Finding all l_0 values from the data
  initL <- data$l[which(data$t == 0)]
  
  #Creating an index variable to keep track of which individual is being observed
  indx <- 1
  
  #Iterating over all l_0 values from the data and all possible interventions
  for (l0 in initL) {
    for (g in 1:nrow(expand.grid(apos))) {
      #Setting the interventions as an intervention
      interv <- as.vector(unlist(expand.grid(apos)[g,]))
      #Reglist contains vectors of our a_0,l_0,a_1... etc for use in forming a linear model
      reglist <- list()
      #Lvec contains the values of our lstars; as noted in Daniel et al., lstar_0 = l_0
      Lvec <- c(l0)
      
      #Creating a data frame to contain our sorted data
      df <- data[order(data$t),]
      
      #Code will loop over all time points in the data, unless T=1, in which case
      #it will prepare for G-Comp manually
      if (max(select(data, t)) == 1) {
        reglist[[1]] <- as.vector(df$l[which(df$t == 0)])
        reglist[[2]] <- as.vector(df$a[which(df$t == 0)])
        reglist[[3]] <- as.vector(df$l[which(df$t == 1)])
  
        
        regm <- as.data.frame(matrix(unlist(reglist), ncol = length(reglist)))
        
        varvec <- c(l0, interv[1])
        
      } else {
        for (i in 0:max(select(data, t))) {
          if ((i != 0) & (i < max(select(data, t)))) {
            #Creating a matrix where the columns are the l_(i-1), a_(i-1), l_(i-1)*a_(i-1), l_i
            lpri <- as.vector(df$l[which(df$t == i-1)])
            apri <- as.vector(df$a[which(df$t == i-1)])
            
            enu <- 1
            reglist[[enu]] <- lpri
            enu <- enu + 1
            reglist[[enu]] <- apri
            enu <- enu + 1
            reglist[[enu]] <- lpri*apri
            enu <- enu + 1
            reglist[[enu]] <- as.vector(df$l[which(df$t == i)])
            
            #Turning the list into a data frame
            regm <- as.data.frame(matrix(unlist(reglist), ncol = length(reglist)))
            
            #Forming a linear model of l_i against l_(i-1), a_(i-1), l_(i-1)*a_(i-1)
            ind <- as.character(colnames(regm)[ncol(regm)])
            form <- paste(ind, " ~ .")
            
            lmod <- glm(form, family = "binomial", data = regm)
            
            
            #Forming a vector of our lstars and specified intervention in order to
            #get an estimate for lstar_i
            varvec <- c()
            enu2 <- 1
            varvec[enu2] <- Lvec[i]
            enu2 <- enu2 + 1
            varvec[enu2] <- interv[i]
            enu2 <- enu2 + 1
            varvec[enu2] <- Lvec[i]*interv[i]
            
            
            #Turning the vector into a dataframe so that predict() can parse it
            predf <- as.data.frame(t(varvec))
            colnames(predf) <- colnames(regm[-c(ncol(regm))])
            
            #Adding the prediction for lstar_i to the vector of lstar values
            Lvec[i+1] <- as.integer(as.logical(runif(1,0,1) < predict(lmod, predf, type = "response")))
          }
        }
        
        enu3 <- 1
        
        #Creating a matrix where the columns are the a_0, l_0,...,a_(i-1),l_(i-1), l_i
        for (k in 0:(max(select(data, t))-1)) {
          reglist[[enu3]] <- as.vector(df$l[which(df$t == k)])
          enu3 <- enu3 + 1
          reglist[[enu3]] <- as.vector(df$a[which(df$t == k)])
          enu3 <- enu3 + 1
        }
        

        reglist[[enu3]] <- as.vector(df$l[which(df$t == max(df$t))])
        
        regm <- as.data.frame(matrix(unlist(reglist), ncol = length(reglist)))
        
        varvec <- c(Lvec[1], interv[1], Lvec[2], interv[2], Lvec[3], interv[3], 
                    Lvec[4], interv[4])
      }
      
      #Forming a linear regression of l_T against a_0,l_0,...,a_(T-1),l_(T-1)
      indy <- as.character(colnames(regm)[ncol(regm)])
      formy <- paste(indy, " ~ .")
      lmody <- lm(formy, data = regm)
      
      
      #Creating a vector of our lstars and interventions to use with our logistic regression
      #model to predict the value for those specific values
      predfy <- as.data.frame(t(varvec))
      colnames(predfy) <- colnames(regm[-c(ncol(regm))])
      
      #Bind the predicted L_T value to the findf data frame
      LT <- c(predict(lmody, predfy)+RStanErr(lmody), sum(interv), data$gender[min(which(data$id == indx))])
      findf <- rbind(findf, LT)
    }
    indx <- indx + 1
  }
  
  colnames(findf) <- c("LT","SumA", "Gender")
  
  #Create a linear model for the Y and the SumA values in findf
  finmod <- lm(LT ~ SumA + Gender, data = findf)
  
  return(summary(finmod))
  break()
} 

#Takes the average of the MSM coefficient over several iterations 
#WARNING: Is computationally prohibitive for large bloop values
AvBoot <- function(data, bloop) {
  int_vec <- NULL
  a_vec <- NULL
  gen_vec <- NULL

  for (i in 1:bloop) {
    mod <- gform(data)
    
    int_vec <- c(int_vec, mod$coefficients[1])
    a_vec <- c(a_vec, mod$coefficients[2])
    gen_vec <- c(gen_vec, mod$coefficients[3])
    
    paste(print(i))
  }
  
  return(paste("Intercept = ", mean(int_vec), "SumA = ", mean(a_vec), 
                     "Gender = ", mean(gen_vec)))
}

AvBoot(CLOSERg, 100)

#Standard Analysis of the Data
sCLOSERg <- NULL
for (id in 1:max(CLOSERg$id)) {
  idrow <- CLOSERg[which(CLOSERg$id == id),]
  nvec <- c(idrow[1,1], idrow[1,3], idrow[1,4] + idrow[2,4], idrow[1,5], idrow[2,5])
  sCLOSERg <- rbind(sCLOSERg, nvec)
}
colnames(sCLOSERg) <- c("id", "gender", "SumA", "L1", "Y") 
rownames(sCLOSERg) <- NULL
sCLOSERg <- as.data.frame(sCLOSERg)

slm <- lm(Y ~ SumA + gender, data = sCLOSERg)
summary(slm)

sLlm <- lm(Y ~ SumA + gender + L1, data = sCLOSERg)
summary(sLlm)
