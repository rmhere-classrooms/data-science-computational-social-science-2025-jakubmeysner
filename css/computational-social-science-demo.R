# remember about setting proper working directory in R
# https://support.posit.co/hc/en-us/articles/200711843-Working-Directories-and-Workspaces-in-the-RStudio-IDE

# how to define functions
plus_two <- function(value=3) {
  return(value + 2)
}

# load the igraph library for processing graphs
library(igraph)

# RANDOM GRAPH (Erdos-Renyi graph)
# number of nodes and rewiring probability
g <- erdos.renyi.game(p.or.m=0.05, n=25)

# summarize the graph
# D/Undirected
# N/Unnamed vertices
# W/Unweighted edges
# B/no-type vertices

summary(g)

# plot this simple graph
plot(g)

# let's add weights to the edges
E(g)$weight <- runif(length(E(g)), 0.01, 1)
V(g)
# now our graph is weighted
summary(g)
plot(g, layout=layout.circle(g))

# let's plot it now with edge width representing the weight (multiplied to see the results)
plot(g, edge.width=E(g)$weight*10, vertex.size=10)

# the same thing, but with different (cirle) layout
plot(g, edge.width=E(g)$weight*5, vertex.size=10, layout=layout.circle(g)) 

# what is the degree of vertices?
degree(g)

# like I said, just numbers, how to aggregate those?
hist(degree(g))

# if we increase the number of nodes, what the distribution of will look like?
# any guesses?

g_larger <-erdos.renyi.game(1000, 0.1)
hist(degree(g_larger))

# how many paths are between two nodes, say 1 and 2
edge.disjoint.paths(g, 1, 2)

# computing betweenness for small graph
betweenness(g)

# computing betweenness for larger graph (...)
betweenness(g_larger)

# longest shortest path (diameter)
diameter(g_larger)

# connected components
cl <- clusters(g)
cl
plot(g, vertex.color=cl$membership)

# most probably each node is in the same component, but let us change the probabilities

g_smallprob <-erdos.renyi.game(50, 0.02)

cl <- clusters(g_smallprob)
cl
plot(g_smallprob, vertex.color=cl$membership)

# lets look at the neigborhood of a verex
gn <- graph.neighborhood(g, order = 2)
plot(gn[[2]])

# BARABASI-ALBERT (PREFFERENTIAL ATTACHMENT model)

g <- barabasi.game(1000)
layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)

# PageRank
pr <- page.rank(g)$vector

plot(g, vertex.size=pr*300,
     vertex.label=NA, edge.arrow.size=.2)

# Loading a network based on a real dataset of interactions

# firstly, we load a csv into a data frame
dfGraph <- read.csv2("out.radoslaw_email_email", skip=2, sep= " ")[, 1:2]

# next, we convert it into an igraph graph object
g <- graph.data.frame(dfGraph, directed = F)

# let us remove loops and multiple edges
g <- simplify(g)

# performing clustering
fgc <- cluster_louvain(g)
groups(fgc)

# which node has the highest betweenness?
V(g)[betweenness(g)==max(betweenness(g))]

# all nodes will be inactivated (new label)
V(g)$activated = F

# but the one with the highest betweenness will be made activated
V(g)[V(g)[betweenness(g)==max(betweenness(g))]]$activated = T

# let us mark nodes with colors depending on inactivated/activated status
for(i in V(g)) {
  if(V(g)[i]$activated == T) {
    V(g)[i]$color = "red"
  } else {
    V(g)[i]$color = "green"
  }
}

# plot the graph
plot(g, vertex.color=V(g)$color, layout=layout.circle(g))

# Now, it is your turn to work on the solution, have fun!
# In case of any questions, feel free to reach me:
# https://www.ii.pwr.edu.pl/~michalski/

# If you are interested in doing research in the area of network science,
# just let me know.

# Our research group - Network Science Lab
# https://networks.pwr.edu.pl
