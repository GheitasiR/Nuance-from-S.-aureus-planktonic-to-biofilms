#Principal component analysis(PCA)
set.seed(9)
TrainingIndex <- createDataPartition(df$Lable, p=1, list = FALSE)
TrainingSet <- df[TrainingIndex,]
dim(TrainingSet)
TestingSet <- df[-TrainingIndex,]
#TestingSet$Lable <- "unknown"
dim(TestingSet)
data_unsup <- as.data.frame(rbind(TrainingSet, TestingSet))
# Check the class of the input data
class(data_unsup[,1:12])
# Convert the data to numeric
data_unsup[,1:12] <- as.numeric(as.matrix(data_unsup[,1:12]))
# Check the class of the input data again
class(data_unsup[,1:12])
pca2 <- prcomp(as.matrix(data_unsup[,1:12]), center = TRUE, scale. = TRUE)
pca2 <- prcomp(data_unsup[,1:12], center = TRUE, scale. = TRUE)
class(pca2)
is.list(pca2)
png(filename = "file name.png", width = 12, height = 12, units = "cm", res = 300)
fviz_eig(pca2, addlabels = TRUE, xlab="PCA1", ylab="PCA2")
dev.off()
df_out2 <- as.data.frame(pca2$x)
head(df_out2)
df_out2$Class <- as.character(data_unsup[,13])
p1 <- ggplot(df_out2, aes(x=PC1, y=PC2, color=Class, label=rownames(df_out2))) +
  geom_point() + geom_text(aes(label=rownames(df_out2)), hjust=0, vjust=0) +
  theme_bw()
p2 <- ggplot(df_out2, aes(x=PC1, y=PC3, color=Class, label=rownames(df_out2))) +
  geom_point() +geom_text(aes(label=rownames(df_out2)), hjust=0, vjust=0) +
  theme_bw()
p3 <- ggplot(df_out2, aes(x=PC2, y=PC3, color=Class, label=rownames(df_out2))) +
  geom_point() +geom_text(aes(label=rownames(df_out2)), hjust=0, vjust=0) +
  theme_bw()
pFin <- grid.arrange(p1,p2,p3, ncol=2)
ggsave(pFin, filename = "file name2.png", device = "png", dpi = 600, width = 30, height = 30,
       units = "cm")
png(filename = "file name2.png", width = 12, height = 12, units = "cm", res = 300)
plot(pca$x[,1],pca$x[,2], xlab="PCA1 (X%)", ylab="PCA2 (Y%)")
dev.off()
```
#Heatmap
#pre processing data #displays structures of R objects = class
hh<-df
which(apply(hh[,1:12],2,var)==0)
hh[,1:12]<- hh[,1:12][,which(apply(hh[,1:12],2,var)!=0)]
sum(is.na(hh))
sapply(hh, class)
# Select only character columns
char_cols <- sapply(hh, is.character)
# Convert character columns to numeric
hh[, char_cols] <- apply(hh[, char_cols], 2, function(x) as.numeric(as.character(x)))
hh$Lable <- factor(hh$Lable)
str(hh)	
row.names(hh)<- samplename
#check if there are missing data? or the class of hh
sum(is.na(hh))
sapply(hh, class)
#plot Heat-map
data2<- hh[,-13]
row.names(hh) <- samplename
png(filename = "heatmap_sample.png", width = 100, height = 100, units = "cm", res = 300)
heatmap(as.matrix(t(data2)), main = "Heatmap_sample", cexRow = 2.75, cexCol = 3.5, scale="none")
dev.off()
png(filename = "heatmap_cytokine.png", width = 100, height = 100, units = "cm", res = 300)
heatmap(as.matrix(hh[,1:12]), main = "Heatmap_cytokine",
        scale = "none", cexRow = 2.75, cexCol = 3.5,
        RowSideColors = rainbow(10)[hh$Lable])
dev.off()
