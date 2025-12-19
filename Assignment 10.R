library(ggplot2)

# TASK 1

data("iris")
iris.scaled <- as.data.frame(scale(iris[, 1:4]))
summary(iris.scaled)

# TASK 2
iris.pca <- princomp(iris.scaled)
summary(iris.pca)

# Comp.1 + Comp.2 > 0.95 -> we need only 2 comp.

# TASK 3

pca.scores <- data.frame(Comp.1 = iris.pca$scores[, 1], 
                         Comp.2 = iris.pca$scores[, 2], 
                         Species = iris$Species)

ggplot(pca.scores, aes(x = Comp.1, y = Comp.2, color = Species)) +
  geom_point(size = 3) +
  theme_bw()

# TASK 4

hc <- hclust(dist(pca.scores[, 1:2]))
plot(hc)

cluster.predict <- as.factor(cutree(hc, k=3))

pca.scores$Group <- cluster.predict

ggplot(pca.scores, aes(x = Comp.1, y = Comp.2, shape = Group, color = Species)) +
  geom_point(size = 3) +
  guides(
    shape = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  theme_bw()

# TASK 5

cl <- kmeans(pca.scores[, 1:2], 3)

pca.scores$Group2 <- as.factor(cl$cluster)

ggplot(pca.scores, aes(x = Comp.1, y = Comp.2, shape = Group2, color = Species)) +
  geom_point(size = 3) +
  theme_bw()
