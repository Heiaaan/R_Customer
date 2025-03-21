customer_data=read.csv("D:/Data_analysis/Data_Analyst_project/R-Programming-Customer-Segmentation-master/Mall_Customers.csv")
#编程基础：很多语言中反斜杠\是转义字符，所以正斜杠/在多数编程语言中会视为路径分隔符，且不会与转义字符冲突。
#加载数据
str(customer_data)
names(customer_data)
head(customer_data)
#查看数据结构
summary(customer_data$Age)
sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)
sd(customer_data$Spending.Score..1.100.)
#通过names(customer_data)查看到每列的名称进行数据摘要
#sd()计算标准差，标准差是数据点与均值的平均距离，


#Customer Gender Visualization

a=table(customer_data$Gender)
View(a)
#table() 函数：用于统计因子型或字符型变量的频数。
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

#性别条形图
pct=round(a/sum(a)*100)
#round()和SQL一样，此处为保留0位小数
View(pct)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
#paste() 函数：将多个字符串拼接成一个字符串。
#?
View(lbs)
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")


#Visualization of Age Distribution


summary(customer_data$Age)

hist(customer_data$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

boxplot(customer_data$Age,
        col="#ff0066",
        main="Boxplot for Descriptive Analysis of Age")

#Analysis of the Annual Income of the Customers

summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

plot(density(customer_data$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(customer_data$Annual.Income..k..),
        col="#ccff66")

boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")

hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#6600cc",
     labels=TRUE)

#K-means Algorithm
#K 均值聚类算法 
library(purrr)
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
#customer_data[,3:5]:选择 customer_data 数据框的第 3 到第 5 列作为聚类数据
#一般是针对数值列，此为age\income\score
#iter.max=100：最大迭代次数。
#k：簇的数量
#nstart=100：随机初始化的次数（避免局部最优）
#algorithm="Lloyd"：使用的算法（Lloyd 是 K 均值的一种经典算法）
#$tot.withinss：簇内平方和（ISS），表示所有簇中数据点与其质心的距离平方和。

#kmeans() 函数：执行 K 均值聚类。
k.values <- 1:10
#定义一个从 1 到 10 的向量，表示要测试的簇数量。

iss_values <- map_dbl(k.values, iss)
#map_dbl() 函数：
#来自 purrr 包，用于对向量中的每个元素应用函数，并返回一个双精度数值向量。
#这里对 k.values 中的每个 k，调用 iss(k) 计算 ISS。

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")
#绘制肘部法则图，用于选择最佳的簇数量。


#Average Silhouette Method

library(cluster) 
library(gridExtra)
library(grid)


k2<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))
#silhouette() 函数：
#k2$cluster：K 均值聚类的结果（每个数据点的簇标签）
#dist(customer_data[,3:5], "euclidean")：计算数据点之间的欧几里得距离。
#返回值：
#轮廓系数值，范围在 [-1, 1] 之间：
#接近 1：表示聚类结果好。
#接近 0：表示聚类结果一般。
#接近 -1：表示聚类结果差。

k3<-kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k4<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))

k5<-kmeans(customer_data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")))

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")))

k7<-kmeans(customer_data[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")))

k8<-kmeans(customer_data[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")))

k9<-kmeans(customer_data[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")))

k10<-kmeans(customer_data[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")))



library(NbClust)
library(factoextra)

#确定最佳k
fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")
#使用轮廓系数法（silhouette）确定最佳的簇数量，并绘制相关图表。
set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
#使用 Gap Statistic 方法评估不同簇数量下的聚类质量。
fviz_gap_stat(stat_gap)

#最后选择k=6
k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6


#Visualizing the Clustering Results using the First Two Principle Components
#使用前两个主成分可视化聚类结果
#PCA 的作用
#降维：将高维数据（如多个变量）降维到低维空间（如 2 个主成分），便于可视化和分析。
#去除冗余：通过主成分分析，可以去除变量之间的冗余信息，提取最重要的特征。
#解释数据：通过主成分的解释方差比例，可以了解每个主成分对数据的贡献。
pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
#对 customer_data 数据集的第 3 到第 5 列进行主成分分析。
summary(pcclust)
#Standard deviation：每个主成分的标准差。标准差越大，说明该主成分包含的信息越多
#Proportion of Variance：每个主成分解释的方差比例。
#Cumulative Proportion：累计解释的方差比例。

pcclust$rotation[,1:2]



set.seed(1)
#ggplot() 函数：用于创建图形对象。
#这里对象为x =Annual.Income..k.., y = Spending.Score..1.100.
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
#geom_point()增加散点图：
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")



ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")



kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))



































































