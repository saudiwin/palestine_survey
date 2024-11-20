# directed acyclic graph for Palestine study

require(dagitty)


pal_graph1 <- dagitty("dag{
                      D -> C
                      C -> A
}")

pal_graph2 <- dagitty("dag{
                      C -> D
                      D -> A
}")
