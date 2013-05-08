source("C:/Users/Yue/Documents/Academic/Research/DynamicNets/paperRelated/MissingnessFit_Func.R")

### step 1, simulate a network
edge.spells = simpleEdgeListGen(15, 4, 20, 5)
netD = networkDynamic(edge.spells = edge.spells)


### step 2: added missingness, include:
  #2a: Last k
  #2b: Past N year







### fit cross section model to both org data, and the data with missingness

# get cross Sections of the nD.
net.list = getCrossSectionNet(nD, numSample = 10)
