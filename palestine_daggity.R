# directed acyclic graph for Palestine study

# Load required libraries
library(ggdag)
library(ggplot2)
library(patchwork) # For combining plots

# Define DAGs for each panel
dag1 <- dagify(
  A ~ W,
  W ~ D,
  labels = c(D = "D", W = "W", A = "A")
)

dag2 <- dagify(
  W ~ D,
  D ~ A,
  labels = c(A = "A", D = "D", W = "W")
)

dag3 <- dagify(
  D ~ W,
  W ~ A,
  labels = c(A = "A", D = "D", W = "W")
)

# Create DAG plots
plot1 <- ggdag(dag1) + theme_dag() + ggtitle("(1)")
plot2 <- ggdag(dag2) + theme_dag() + ggtitle("(2)")
plot3 <- ggdag(dag3) + theme_dag() + ggtitle("(3)")

# Combine the plots side by side
plot1 + plot2 + plot3
