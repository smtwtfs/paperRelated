# setwd("C:/Users/Yue/Documents/Academic/Research/DynamicNets/paperRelated")
# 
# source("functions.R")
# covars <- c('RECTIME1','AGE','GENDER','SPGNDR','ETHNIC','RACE','PNAME','SPF','SPTYPE','SP2PLY');
# # 'SPFSEX','SPLSEX',
# SPdata = extractSPdata(covars)
# save(SPdata, file = 'SPdata.RDATA')


load('SPdata.RDATA')
source("functions.R")

summ.list = list();
# sex
covars = c('GENDER', 'SPGNDR')
summ = getColSummary(covars, SPdata)

covars = c("RACE", "SPRACE")
summ = getColSummary(covars, SPdata)



## Process SP data:
##    If SPLSEX has value and SPFEX does not have value, (started w/ partner more than 12 months ago)
##       SPFSEX = RECTIME1 - 12 - 1;

SPtry = getSPtry(data,5, 3,varTable)
SPtry = fillLSEX(SPdata, 3)
max = findMaxPartners(SPdata,varTable$pos[which(varTable$varName == 'SPTYPE')])



queryTimePoint = function (time, data,varTable){
  posTYPE = varTable$pos[which(varTable$varName == 'SPTYPE')]
  posLSEX = varTable$pos[which(varTable$varName == 'SPLSEX')]
  posFSEX = varTable$pos[which(varTable$varName == 'SPFSEX')]
  posRECT = varTable$pos[which(varTable$varName == 'RECTIME1')]
  
  for i in 1:max(varTable$number){
    is.na(data[,posTYPE+i-1])
    
    
  }
}



queryTimePoint = function (time, data,varTable){
  posTYPE = varTable$pos[which(varTable$varName == 'SPTYPE')]
  posLSEX = varTable$pos[which(varTable$varName == 'SPLSEX')]
  posFSEX = varTable$pos[which(varTable$varName == 'SPFSEX')]
  posRECT = varTable$pos[which(varTable$varName == 'RECTIME1')]
  for (i in 1:dim(data)[1]){
    RECT = data[i,posRECT] # Recording time
    for (j in 1:max(varTable$number)){
      is.na(data[1,posTYPE +j -1 ])
      
      
      if (!is.na(data[i,posType + j - 1])){
        if(data[i,posTYPE + j - 1] == "Partner last yr" || data[i,posTYPE + j - 1] == "Both s/c & year p"){
          if(!is.na(data[i,posLSEX+j-1]) || is.na(data[i,posFSEX+j-1]))
            if(data[i,posLSEX+j-1] >= time && data[i,posFSEX+j-1] <= time){
              print(paste("data: ", i, ", L: ", data[i,posLSEX+j-1], ", F: ", data[i,posfSEX+j-1], sep = ""))
              
              
            } 
        }
      }      
    }
  }
}
debug(queryTimePoint(1103, SPdata,varTable))








## Unfinished function
## ## needs to finish later
compressData = function (data,varTable){
  posType = varTable$pos[which(varTable$varName == 'SPTYPE')]
  for (i in 1:dim(data){
    pointer = 1
    for(j in 1:28){
      if (data[i,posType + j - 1] 
          
    }
  }
}  ## Unfinished function
##### needs to finish later


## Find the max value"
findMaxPartners = function(data,startPos){
  numOfSP = NULL;
  ## find subject with maximum number of partners. 
  for (i in 1:28){
    numOfSP[i] = sum(table(data[,startPos +i-1])[2:3])
    
  }
  return(numOfSP)
}


fillLSEX = function(data, numOfPartners){
  ## if a subject's SPFSEX is not filled and SPLSEX is filled, 
  ## Then: SPFSEX = RECTIME1 - 13. (assume that the first sex is right before the 1 year limit.) 
  for (i in 1:dim(data)[1]){ # iterate through the subjects.
    for (j in 1:numOfPartners){  # iterate through the 28 people recorded.
      FS = paste('SPFSEX', j, sep = "")
      LS = paste('SPLSEX', j, sep = "")
      if (is.na(data[i,which(names(data) == FS)]) && !is.na(data[i,which(names(data) == LS)]) ){
        
        data[i,which(names(data) == FS)] <- data$RECTIME1[i] - 13
      }
    }
  }
  return(data)
}



getSPtry  <- function(data, numOfSubs, numOfPartners, varTable){ 
  SPtry<- data.frame("CASEID" = data[1:numOfSubs,1], stringsAsFactors=FALSE) 
  for (i in 2:dim(varTable)[1]){
    startIndex = varTable$pos[i];    
    print(startIndex)
    for (j in 1:min(numOfPartners,varTable$number[i])){
      print(j)
      SPtry[paste(varTable$varName[i],j, sep="")] <- data[1:numOfSubs,startIndex+j-1]                       
    }            
  }
  return(SPtry)
}





