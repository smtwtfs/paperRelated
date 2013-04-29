


getColSummary <- function(covars, data){
  summ = NULL;
  for(i in 1:length(covars)){
    
    index <- grep(covars[i], colnames(data))  
    
    if(length(index)){
      for (i in 1:length(index)){
        if(is.null(summ)){
          summ = summary(data[,index[i]])
        }else{
          summ = summ + summary(data[,index[i]])
        }
      }
    }
  }
  return(summ)
}






extractSPdata = function(covars) {
  
  # data.RDATA is too large to load everytime
  # extract a smaller dataset SPdata is necessary. 
  # covars are the variables names that might be used.
  
  
  # covars <- c('RECTIME1','AGE','GENDER','ETHNIC','RACE','PNAME','SPTYPE','SPFSEX','SPLSEX','SP2PLY');
  
  data<-read.spss("C:/Users/Yue/Documents/Academic/Research/DynamicNets/US/Data/nhsls.sav",to.data.frame=TRUE,use.value.labels=TRUE)
  
  index <- which(colnames(data) == "CASEID")
  SPdata<- data.frame("CASEID" = data[,index], stringsAsFactors=FALSE)
  # varTable: "posInData" is the position corresponding to the original "data", 
  # while "pos" is the varTable corresponding to the extrated "SPdata"
  varTable = data.frame("varName" = "CASEID", "pos" = 1, "posInData" = 1, "number" = 1, stringsAsFactors = FALSE)
  
  ##########################################
  ### Sexual Partners over the last year
  ##########################################
  ## Does 2 things:
  #  1: get related data into 'SPdata'
  
  #  2: get the lookup table for both SPdata and data
  
  
  added <- NULL;
  posInSPdata = 1;
  for(i in 1:length(covars)){
    if (!covars[i] %in% added) {
      ## find columns that have the name.
      index <- grep(covars[i], colnames(data))
      if (length(index)){
        varTable<-rbind(varTable,data.frame(varName=covars[i],pos = posInSPdata+1, posInData=index[1], number = length(index)))
        
        for(j in 1:length(index)){
          thisIndex = index[j]
          SPdata[paste(colnames(data)[thisIndex], sep="")] <- data[,thisIndex]
        }
        posInSPdata = posInSPdata + j;
        added<-c(added,covars[i])
      }
    }
  }
  return(SPdata)
}
