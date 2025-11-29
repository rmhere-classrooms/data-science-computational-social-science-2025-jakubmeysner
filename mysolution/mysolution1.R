library(igraph)

g <- sample_gnp(n = 100, p = 0.05)

summary(g)

V(g)
E(g)

E(g)$weight <- runif(length(E(g)), min = 0.01, max = 1)

summary(g)

degree(g)
hist(degree(g))

count_components(g)

pr <- page_rank(g)
plot(g, vertex.size = pr$vector * 325, vertex.label = NA)
