library(dplyr)
library(tidyr)
library(tibble)

##Organising and reformatting the data
#Loading the data
CLOSERtabdat <- read.table(CLOSER_training_dataset_complete_cases.tab)

#Restricting the data to deal with the relevant variables
CLOSERdat <- na.omit(CLOSERtabdat[c("n622", "n16math", "n16eng", "bmi23", "bmi33", 
                                    "n5113", "marstpar", "econstat", "n500520", 
                                    "n5931", "n504262", "n5920", "n504273", "mal24n4", "mal24n5")])

#Adding an ID column and renaming columns for clarity of analysis
CLOSERdat <- cbind(id = seq.int(nrow(CLOSERdat)), CLOSERdat)
colnames(CLOSERdat) <- c("id", "Gender", "MathGrade", "EngGrade", "BMI23", "BMI33", 
                         "Mar23", "Mar33", "Work23", "Work33", "Smok23", "Smok33", 
                         "Drink23", "Drink33", "Mal23", "Mal33")

#Creating a function to convert data values into other ones which can be used
#with the gformula process
ConReplace <- function(var, orvals, nuvals) {
  for (i in 1:length(orvals)){
    CLOSERdat[var][CLOSERdat[var] == orvals[i]] <<- nuvals[i]
  }
}

#Replacing the Gender variable with interpretable values
ConReplace("Gender", c(1,2), c(1, 0))

#Binarising values to allow the gformula process to properly handle them
ConReplace("Mar23", 1:5, c(0,1,0,0,0))
ConReplace("Mar33", 0:12, c(0,0,0,0,1,0,1,0,0,0,0,0,0))
ConReplace("Work23", 0:13, c(0,1,1,1,1,0,0,0,0,0,0,0,0,0))
ConReplace("Work33", 1:10, c(1,1,1,1,0,1,0,0,0,0))
ConReplace("Smok23", 1:2, c(1,0))
ConReplace("Smok33", 1:2, c(1,0))
ConReplace("Drink23", 1:5, c(1,1,1,0,0))
ConReplace("Drink33", 1:5, c(1,1,1,1,0))


#Reformatting data so that is correctly parsed by the function
CLOSERg <- CLOSERdat[c("id", "Gender", "Work23", "Work33", "Mal23", "Mal33")]
CLOSERg <- gather(CLOSERg, key ="a", value ="Work", -c(id, Gender, Mal23, Mal33))
CLOSERg <- CLOSERg[order(CLOSERg$id),]
CLOSERg <- gather(CLOSERg, key ="c", value ="Mal", -c(id, Gender, a, Work))
CLOSERg <- CLOSERg[order(CLOSERg$id),]
CLOSERg <- CLOSERg[which((CLOSERg$a == "Work23" & CLOSERg$c == "Mal23") | (CLOSERg$a == "Work33" & CLOSERg$c == "Mal33")),][c("id","Gender","Work","Mal")]
row.names(CLOSERg) <- NULL
CLOSERg <- add_column(CLOSERg, t = rep(c(0,1), max(CLOSERg$id)), .after = "id")

write.table(CLOSERg, "INSERT FILE PATH/CLOSERg.txt",
            quote=FALSE, row.names = FALSE)