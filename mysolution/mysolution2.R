library(igraph)

g <- sample_pa(n = 1000)

l <- layout_with_fr(g)
plot(g, layout = l, vertex.size = 2, vertex.label = NA, edge.arrow.size = 0.1)

V(g)[betweenness(g) == max(betweenness(g))]

diameter(g)

# Grafy tworzone z wykorzystaniem modelu Erdosa-Renyi'ego powstają poprzez losowego tworzenie krawędzi między parami
# wierzchołków, co prowadzi do rozkładu stopni zbliżonego do rozkładu Poissonna i braku wyrażnych hubów. W przypadku
# modelu Barabasiego-Alberta natomiast grafy generowane są w sposób przyrostowy - kolejne wierzchołki dokładane są z
# preferencją dla już istniejących wierzchołków o wysokim stopniu, prowadząc do rozkładu potęgowe i powstawania hubów.
