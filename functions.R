extractSPdata = function(covars) {
  
  # data.RDATA is too large to load everytime
  # extract a smaller dataset SPdata is necessary. 
  # covars are the variables names that might be used.
  
  
 # covars <- c('RECTIME1','AGE','GENDER','ETHNIC','RACE','PNAME','SPTYPE','SPFSEX','SPLSEX','SP2PLY');
  
  data<-read.spss("C:/Users/Yue/Documents/Academic/Research/DynamicNets/US/Data/nhsls.sav",to.data.frame=TRUE,use.value.labels=TRUE)
  
  ###### Martina's Notes ##########################################################
  #################################################################
  ### NHSLS 1994: complicated sex partner elicitation -- 
  ### up to 4 spouse/cohabs (any time frame) plus up to 19 other partners (restricted to the last year). 
  ### Â different questions asked for different types of partners: current s/c, past s/c, one-night stands, other partners. 
  ### Â for s/c have date cohab began/ended/current status. Â for other partners date of first/last sex and current status.
  
  
  ##############################################################################
  ###### Martina's Notes#############################################################
  
  ## month/day/year recoded as century months (starting from January 1900)
  
  ## Cross-section (household sample with equal probability)
  ## 3159
  ## Denoted with 2 (SAMPLE variable)
  ## Over sample of Blacks and Hispanics
  ## 273
  ## Coded with 1 (SAMPLE variable)
  
  ## Household is self weighted
  ## HHSIZE Number of eligible aduls (ages 18-59) in the household
  ## can be used to reweight the sample (if desired)
  
  ## RWEIGHT -- Sample weight for respondents
  ## Created by NORC for combining the household sample with the oversample
  ## also reweights for household size and nonresponse (post-stratification adjustment)
  ## scaled to sum to the total combine sample size of 3432
  
  
  ## Use 1990 census for spatial information 
  ## Note this might be recorded in 1980 census information
  ## City and State variables have been recoded to the census region level
  
  index <- which(colnames(data) == "CASEID")
  SPdata<- data.frame("CASEID" = data[,index], stringsAsFactors=FALSE)
  # varTable: "posInData" is the position corresponding to the original "data", 
  # while "pos" is the varTable corresponding to the extrated "SPdata"
  varTable = data.frame("varName" = "CASEID", "pos" = 1, "posInData" = 1, "number" = 1, stringsAsFactors = FALSE)
  
  ##########################################
  ### Sexual Partners over the last year
  ##########################################
  b
  ## Does 2 things:
  #  1: get related data into 'SPdata'
  
  #  2: get the lookup table for both SPdata and data
  
  
  added <- NULL;
  posInSPdata = 1;
  for(i in 1:length(covars)){
    if (!covars[i] %in% added) {
      ## find columns that have the name.
      index <- grep(covars[i], colnames(data))
      if (length(ii)){
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
  setwd("C:/Users/Yue/Documents/Academic/Research/DynamicNets/paperRelated")
  save(SPdata, file = "SPdata.RDATA");
}
