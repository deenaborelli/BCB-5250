
#download the table and save as a CSV format,upload it into R.
#reading the data
data <- read.csv("C:/Users/deena/OneDrive/Desktop/Data.csv",row.names = 1)

#viewing the data
View(data)

#load two R package which will allow us to do PCA in R
library("factoextra")
library("FactoMineR")


#using the PCA() function
pca.data <- PCA(data[,-1], scale.unit = TRUE, graph = FALSE)

#using fviz_eig() function to make sure that most of the data will be presented in the PCA plot
fviz_eig(pca.data, addlabels = TRUE, ylim = c(0, 70))

##Q1
#the first two principal components account for 63.2% and 18.5% of the total 
#variance, respectively, capturing 81.7% of the variability in the dataset cumulatively.


#using fviz_pca_var() function to draw a variable correlation plot 
fviz_pca_var(pca.data, col.var = "cos2",
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE)

#plotting the cell types 
pca.data <- PCA(t(data[,-1]), scale.unit = TRUE, graph = FALSE)

#using the viz_pca_ind() function for the visualization 
fviz_pca_ind(pca.data, col.ind = "cos2", 
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
             repel = TRUE)

#To add labels to the PCA plot we can use ggpubr package
library(ggpubr) 

a <- fviz_pca_ind(pca.data, col.ind = "cos2", 
                  gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
                  repel = TRUE)

#using ggpar() function to add labels
ggpar(a,
      title = "Principal Component Analysis",
      xlab = "PC1", ylab = "PC2",
      legend.title = "Cos2", legend.position = "top",
      ggtheme = theme_minimal())

#plot the gene instead of the cell types, using PCA() function
pca.data <- PCA(data[,-1], scale.unit = TRUE,ncp = 2, graph = FALSE)

#To color the gene in the PCA plot we will be using the first column (Lineage),
#convert the column to a factor
data$lineage <- as.factor(data$lineage)


#coloring palette
library(RColorBrewer)

nb.cols <- 3
mycolors <- colorRampPalette(brewer.pal(3, "Set1"))(nb.cols)

#using fviz_pca_ind() function to create the PCA plot
a <- fviz_pca_ind(pca.data, col.ind = data$lineage,
                  palette = mycolors, addEllipses = TRUE)

#using the ggpar() function to add labels
ggpar(a,
      title = "Principal Component Analysis",
      xlab = "PC1", ylab = "PC2",
      legend.title = "Cell type", legend.position = "top",
      ggtheme = theme_minimal())


##Q2
#some genes are associated with a specific type of cell. These genes could act
#as marker to identify these cell types. In TE cells the KRT18, KRT8 and S100A16
#are highly expresses compare to the other genes. In EPI cells the DPPA5, IFITM1,
#MT1X and UPP1 are highly expressed compare to other genes. And in PE cells only 
#APOA1 is highly expressed compare to the other genes.









