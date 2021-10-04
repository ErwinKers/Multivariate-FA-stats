
# loading in libraries
library(readxl)
library(factoextra)
library(tidyverse)
# what to use (PCA, NMDS, PCoA)
  # lot's of 0 values use NMDS (Bray-Curtis distance)
  

#grouped data not in percentages----
group <- read_excel("C:/Users/Erwin kers/OneDrive/UIT/BIO-3524/practice data/Github/Multivariate-FA-stats/grouping.xlsx")
View(group)

groupPR <- prcomp(group[c(44:51)], center = T, scale = T)
fviz_pca_biplot(groupPR, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = group$Species, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Species") +
  ggtitle("2D PCA-plot from FA dataset") +
  theme(plot.title = element_text(hjust = 0.5))


# trying to get the same layout (PCA)
library(FactoMineR)
library(ade4)
library(ExPosition)

?PCA <- PCA(percent[c(44:51)]),scale.unit = TRUE, ncp = 5, graph = TRUE)


#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization to first four columns in iris dataset
normFA <- as.data.frame(lapply(percent[c(44:51)], min_max_norm))

Normok <- prcomp(normFA, center = T, scale = F)
fviz_pca_biplot(Normok, geom.ind = "point", pointshape = 21, 
                pointsize = 2, 
                fill.ind = group$Species, 
                col.ind = "black", 
                palette = "jco", 
                addEllipses = T,
                label = "var",
                col.var = "black",
                repel = T,
                legend.title = "Species") +
  ggtitle("2D PCA-plot from FA dataset") +
  theme(plot.title = element_text(hjust = 0.5))

######## Percentage----


percent <- read_excel("C:/Users/Erwin Kers/OneDrive/UIT/BIO-3524/practice data/percentage FA.xlsx")

## not the same result, did they remove outliers?

percentPR <- prcomp(percent[c(44:51)], center = T, scale = T)
fviz_pca_biplot(percentPR, geom.ind = "point",
                
                pointshape = 22, 
                pointsize = 2, 
                fill.ind = percent$Species, 
                col.ind = "black", 
                palette = c("yellow", "green","red", "blue", "white"), 
                addEllipses = F,
                label = "var",
                col.var = "black",
                repel = F,
                legend.title = "Species") +
  ggtitle("2D PCA-plot from FA dataset") +
  theme(plot.title = element_text(hjust = 0.5))

percentPR <- prcomp(percent[c(44:51)], center = T, scale = T)
fviz_pca_biplot(percentPR, geom.ind = "point", pointshape = 21, 
                pointsize = 2, 
                fill.ind = percent$Species, 
                col.ind = "black", 
                palette = "jco", 
                addEllipses = F,
                label = "var",
                col.var = "black",
                repel = T,
                legend.title = "Species") +
  ggtitle("2D PCA-plot from FA dataset") +
  theme(plot.title = element_text(hjust = 0.5))


results <- prcomp(percent[c(44:51)], scale = TRUE)


##### Correspondence analysis

PLOT.CA (CA(percent[c(44:51)]))
plot(CA(percent[c(44:51)]))


### LRA (need to remove 0 values to be able to use this)
library(easyCODA)




########## PERMANOVA

library(vegan)

adonis2(percent[c(5:43)] ~ Species, data = group, permutations = 999, method="bray")

adonis2(percent[c(44:49)] ~ Species, data = group, permutations = 999, method="bray")

#somehow everything i test is significant. Can't get the same results as the paper:
  #PERMANOVA, F(4,122) = 93.2, p = 0.001)

#### NMDS
library(ggplot2)

q <- metaMDS(percent[c(49:51)], distance="bray", k=2, trymax=35, autotransform=TRUE)
stressplot(q)

NMDS1 <- q$points[,1] 
NMDS2 <- q$points[,2]
NMDS.plot<-cbind(percent, NMDS1, NMDS2)

ggplot(NMDS.plot, aes(NMDS1, NMDS2, color=Species))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()


########## comparing 2 species (are they different?)



