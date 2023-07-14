#loading packages
library("readxl")
library(openxlsx)
library("lattice")
library(rlang)
library(ggplot2)
library("caret")
library("rpart")
library("rpart.plot")
```
#loading data
setwd("C:/desktop")
df<- read_excel(""C:/desktop/your file name.xlsx", sheet = "sheet name", col_names = TRUE)
log_cap<- log2(df)
write.xlsx(log_cap, "log_cap.xlsx")
df$x -> rows
row.names(df)<-df$x
samplename<-df$x
df <- df[,-1]
df$Lable -> Lable
df$Lable<-as.factor(df$Lable)
row.names(df) <- samplename
table(df$Lable)
str(df2)
```
