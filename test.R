s = runif(200)
s[s>0.85 ] = 1
s[s<0.9] = 0
m = matrix(s[1:100],10,10)

for(i in 1:10)
  m[i,i] = 0

net1 = network(m,directed=FALSE)
m = matrix(s[101:200],10,10)
for(i in 1:10)
  m[i,i] = 0

net2 = network(m,directed=FALSE)

network.list= list(net1,net2)
onset = [1,5]
termini = [5,10]

nD = networkDynamic(network.list = network.list)

plot(net)


stergm.fit = stergm(network.list, formation = ~edges, dissolution = ~edges, estimate = "CMLE")

