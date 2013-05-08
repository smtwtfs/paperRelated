g.sim <- simulate(network(16) ~ edges + mutual, coef=c(0, 0))
summary(g.sim ~ edges + mutual)



#################

n = 5;

# random network object:
mat = matrix(round(runif(n^2, max = 0.8, min = -0.4)), nrow = n)
net <- network(mat, directed = FALSE)

# adding attributes:
net%v%"race" = c("W","W", "B", "H", "O")
net%v%"gender" = c("F", "M", "F", "F", "M")

g.sim <- simulate(~edges+kstar(2), coef=c(-1.8,0.03),
                  basis=net, control=control.simulate(
                    MCMC.burnin=100000,
                    MCMC.interval=1000))




logit<-function(p)log(p/(1-p))
coef.form.f<-function(coef.diss,density) -log(((1+exp(coef.diss))/(density/(1-density)))-1)

# Construct a network with 20 nodes and 20 edges
n<-20
target.stats<-edges<-20
g0<-network.initialize(n,dir=TRUE)
g1<-san(g0~edges,target.stats=target.stats,verbose=TRUE)

S<-10

# To get an average duration of 10...
duration<-10
coef.diss<-logit(1-1/duration)

# To get an average of 20 edges...
dyads<-network.dyadcount(g1)
density<-edges/dyads
coef.form<-coef.form.f(coef.diss,density)

# ... coefficients.
print(coef.form)
print(coef.diss)

# Simulate a networkDynamic
dynsim<-simulate(g1,formation=~edges,dissolution=~edges,coef.form=coef.form,coef.diss=coef.diss,time.slices=S,verbose=TRUE)

# "Resume" the simulation.
dynsim2<-simulate(dynsim,time.slices=S,verbose=TRUE)






















