#Packages Descriptive Statistics
library(openxlsx)
library(ggplot2)

#Importing data
data1<-read.xlsx("D:/Data Stat Desk/X1/Book1.xlsx")
data2<-read.xlsx("D:/Data Stat Desk/X1/Book2.xlsx")
View(data1)
View(data2)

#Make bar chart
gridExtra::grid.arrange(
  ggplot(data1, aes(x = Kab, y = Var)) +
    geom_bar(aes(x=reorder(Kab,-Var),y=Var),stat = "identity") +
    coord_flip() + scale_y_continuous(name="X1") +
    scale_x_discrete(name="Kabupaten/Kota") +
    theme(axis.text.x = element_text(face="bold", color="#008000",
                                     size=8, angle=0),
          axis.text.y = element_text(face="bold", color="#008000",
                                     size=8, angle=0)), 
  ggplot(data2, aes(x = Kab, y = Var)) +
    geom_bar(aes(x=reorder(Kab,-Var),y=Var),stat = "identity") +
    coord_flip() + scale_y_continuous(name="X1") +
    scale_x_discrete(name="Kabupaten/Kota") +
    theme(axis.text.x = element_text(face="bold", color="#008000",
                                     size=8, angle=0),
          axis.text.y = element_text(face="bold", color="#008000",
                                     size=8, angle=0)), 
  nrow = 1
)
#Repeats until X14 where all variables are finished

#Packages Fuzzy C-Shell
library(factoextra)
library(cluster)
library(openxlsx)
library(e1071)

#Importing data
data<-read.xlsx("D:/Data.xlsx")
str(data)
head(data)
rownames(data)

#Setting ID
rownames(data)<-make.names(data$Kab,unique=TRUE)
data2<-data[,-1]
rownames(data2)

#Data standardization
datascale<-scale(data2)
View(datascale)
write.csv(datascale, "datafuzzy.csv")

#FCS
#2 Cluster
Set.seed(1000)
X<-as.matrix(datascale)
FCS<-cshell(X,2,verbose=TRUE,method=”cshell”,m=2)
print(FCS)
FCS[‘radius’]
ValidityIndex<-fclustIndex(FCS,X, index="all")
ValidityIndex

#3 Cluster
Set.seed(1000)
X<-as.matrix(datascale)
FCS<-cshell(X,3,verbose=TRUE,method=”cshell”,m=2)
print(FCS)
FCS[‘radius’]
ValidityIndex<-fclustIndex(FCS,X, index="all")
ValidityIndex

#4 Cluster
Set.seed(1000)
X<-as.matrix(datascale)
FCS<-cshell(X,4,verbose=TRUE,method=”cshell”,m=2)
print(FCS)
FCS[‘radius’]
ValidityIndex<-fclustIndex(FCS,X, index="all")
ValidityIndex

#5 Cluster
Set.seed(1000)
X<-as.matrix(datascale)
FCS<-cshell(X,5,verbose=TRUE,method=”cshell”,m=2)
print(FCS)
FCS[‘radius’]
ValidityIndex<-fclustIndex(FCS,X, index="all")
ValidityIndex

#6 Cluster
Set.seed(1000)
X<-as.matrix(datascale)
FCS<-cshell(X,6,verbose=TRUE,method=”cshell”,m=2)
print(FCS)
FCS[‘radius’]
ValidityIndex<-fclustIndex(FCS,X, index="all")
ValidityIndex

#Packages Mapping
library(rgdal)
library(spdep)
library(raster)
library(gdata)

data<-read.csv("D:/DataPeta.csv",sep=";")
str(data)
data.indoProv=readOGR(dsn="D:/Pulau Sulawesi KabKot",layer="pulau sulawesi")
plot(data.indoProv)
text(data.indoProv,"KABKOT",cex=0.4)
data.indoProv@data<-cbind(Cluster=data[,2],data.indoProv@data)

sp.label <- function(x, label) {list("sp.text", coordinates(x), label,cex=0.4)}
NUMB.sp.label <- function(x) {sp.label(x, as.vector(x@data$KABKOT))}
make.NUMB.sp.label <- function(x) {do.call("list", NUMB.sp.label(x))}

spplot(data.indoProv[,1],sp.layout = make.NUMB.sp.label(data.indoProv),
cex=2,col="white")

