#December 14.2019
#Nadya Sokolova

# Clustering in R Practical Guide to cluster analysis in R (Alboukadel Kassambara)
# основные алгоритмы partitioning clustering (k-means, PAM, CLARA), 
#нужно задать количество кластеров заранее


install.packages("cluster")
install.packages("factoextra")
install.packages("clustertend")
install.packages("dendextend")
install.packages("NbClust")
install.packages("fpc")
install.packages("dbscan")


library(stats)
library(cluster)
library(factoextra)
library(clustertend)
library(dendextend)
library(NbClust)
library(fpc)
library(dbscan)

data("USArrests")
df <- scale(USArrests)

# kmeans(x, centers, iter.max = 10, nstart = 1)
# сколько кластеров выбрать?

fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# алгоритм каждый раз начинает с рандомно выбранного центроида, поэтому чтобы результаты воспроизводились

set.seed(123)

km.res <- kmeans(df, 4, nstart = 25) # nstart желательно между 25 и 50, чтобы найти стабильное решение
print(km.res)

aggregate(USArrests, by=list(cluster=km.res$cluster), mean) # увидеть среднее по кластерам до шкалировани€
dd <- cbind(USArrests, cluster = km.res$cluster) # добавить данные о кластерах в датасет
km.res$cluster # наблюдени€ и номер кластера
km.res$size # размер каждого кластера
km.res$centers # центроиды каждого кластера

fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

# k-medoids (PAM) что это и чем лучше предыдущего алгоритма
# pam(x, k, metric = "euclidean", stand = FALSE)
# как выбрать сколько должно быть групп?

# нужно выбрать решение, где average silhouette выше (чем выше, тем дальше кластеры)
fviz_nbclust(df, pam, method = "silhouette")+
  theme_classic()

pam.res <- pam(df, 2)
print(pam.res)
pam.res$medoids
head(pam.res$clustering)
fviz_cluster(pam.res,
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic())


# CLARA посчитаем на рандомно сгенерированном датасете 
set.seed(1234)
df <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
            cbind(rnorm(300,50,8), rnorm(300,50,8)))
colnames(df) <- c("x", "y")
rownames(df) <- paste0("S", 1:nrow(df))
head(df, nrow = 6)
# clara(x, k, metric = "euclidean", stand = FALSE,samples = 5, pamLike = FALSE)
fviz_nbclust(df, clara, method = "silhouette")+
  theme_classic()
clara.res <- clara(df, 2, samples = 50, pamLike = TRUE)
print(clara.res)
clara.res$medoids
head(clara.res$clustering, 10)
fviz_cluster(clara.res,
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic())

# hierarchical clustering (agglomerative - agnes, divisive - diana) не требует выделить число кластеров заранее
data("USArrests")
df <- scale(USArrests)
# пример на agglomerative (то есть сначала каждый объект образует кластер, схожие объекты объедин€ютс€ в одну ветку)
# нужно создать матрицу дистанций между объектами
res.dist <- dist(df, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]
res.hc <- hclust(d = res.dist, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5)
# как пон€ть насколько хорошо произошло деление между кластерами, можно посчитать коррел€цию между изначально данными дистанци€ми между объектами и полученными в результате применени€ алгоритма
# чем ближе к 1, тем лучше получившеес€ решение
res.coph <- cophenetic(res.hc)
cor(res.dist, res.coph)
res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, cophenetic(res.hc2))
# как поделить на отдельные кластеры? например, хотим поделить на 4 кластера
grp <- cutree(res.hc, k = 4)
head(grp, n = 4)
table(grp) # сколько наблюдений в каждом кластере из четырех

fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

fviz_cluster(list(data = df, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# есть функции, которые объединяют несколько функций дл€ рассчета agnes и diana
res.agnes <- agnes(x = USArrests, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward") # Linkage method
res.diana <- diana(x = USArrests, # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean") # metric for distance matrix

fviz_dend(res.agnes, cex = 0.6, k = 4)

df <- scale(USArrests)
set.seed(123)
ss <- sample(1:50, 10) # отбираем рандомно 10 наблюдений из 50, чтобы проще было визуализировать
df <- df[ss,]
res.dist <- dist(df, method = "euclidean")
hc1 <- hclust(res.dist, method = "average")
hc2 <- hclust(res.dist, method = "ward.D2")
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
dend_list <- dendlist(dend1, dend2)
tanglegram(dend1, dend2)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))

# entanglement - мера соответстви€ (визуальное сравнение); 
# чем ниже коэффициент, тем лучше соответствие (измер€етс€ от 0 до 1)
# также можно сравнивать, посмотрев на корреляцию (измеряется от -1 до 1. 
# при значении 1 делаем вывод, что дендрограммы несимметричны)
cor.dendlist(dend_list, method = "cophenetic")

# немного визуализации дендрограмм
data(USArrests)
dd <- dist(scale(USArrests), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

fviz_dend(hc, cex = 0.5)
fviz_dend(hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect_fill = TRUE)
fviz_dend(hc, cex = 0.5, horiz = TRUE) # если хотим горизонтальную дендрограмму
fviz_dend(hc, k = 4, cex = 0.4, horiz = TRUE, k_colors = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)
fviz_dend(hc, cex = 0.5, k = 4,
          k_colors = "jco", type = "circular")
fviz_dend(hc, k = 4, k_colors = "jco",
          type = "phylogenic", repel = TRUE)


# как понять насколько хорошим получилось кластерное решение
# »змеряя кластерную тенденцию. ¬ќзможно наши данные вообще не кластеризуютс€
# iris data set
head(iris, 3)
df <- iris[, -5]
# —оздадим рандомный датасет дл€ сравнени€
random_df <- apply(df, 2, function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)
# Standardize the data sets
df <- iris.scaled <- scale(df)
random_df <- scale(random_df)
# ¬изуализируем, чтобы увидеть кластеры (если они есть)
fviz_pca_ind(prcomp(df), title = "PCA - Iris data",
             habillage = iris$Species, palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")
fviz_pca_ind(prcomp(random_df), title = "PCA - Random data",
             geom = "point", ggtheme = theme_classic())
set.seed(123)
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())
km.res2 <- kmeans(random_df, 3)
fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())
fviz_dend(hclust(dist(random_df)), k = 3, k_colors = "jco",
          as.ggplot = TRUE, show_labels = FALSE)
# как убедиться, что данные кластеризуются
# Hopkins statistic (сравнивает дистанции между объектами в нашем датасете и в рандомно сгенерированном)
# если значение статистики близко к 0, значит можем сделать вывод, что есть разница и данные кластеризуемы. ≈сли близко к 0.5, значит рандомный датасет и наш похож
set.seed(123)
hopkins(df, n = nrow(df)-1) # считаем дл€ базы данных по ирисам
set.seed(123)
hopkins(random_df, n = nrow(random_df)-1) # считаем для рандомной базы данных

# как найти оптимальное количество кластеров
# elbow method - ищем решение с минимальной суммой квадратов внутри кластера (использовали при подсчете k-means)
# average silhouette method (использовали дл€ подсчета k medoids)
# gap statistic (измер€ет насколько отличаетс€ деление на кластеры от рандомного)
# функции, которые объедин€ют несколько этих показателей

df <- scale(USArrests)
# fviz_nbclust(x, FUNcluster, method = c("silhouette", "wss", "gap_stat"))
# рассчитаем показатели дл€ k-means на одном датасете USArrests
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

# ”сложненна€ кластеризаци€
# Fuzzy clustering
# fanny(x, k, metric = "euclidean", stand = FALSE)
df <- scale(USArrests)
res.fanny <- fanny(df, 2)
head(res.fanny$membership)
head(res.fanny$clustering)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")

# DBSCAN как решение дл€ данных, где не наход€тс€ хорошо идентифицируемые кластеры и есть выбросы 
# (+ не надо заранее определ€ть количество кластеров
data("multishapes")
df <- multishapes[, 1:2]
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df, geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5) # eps определ€ет радиус, MinPts опредл€ет количество точек в пределах радиуса
# чем больше датасет, тем больше должно быть значение MinPts, минимальное значение 3
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
print(db)
# как выбрать eps?
# нужно посчитать среднюю дистанцию между точкой и ближайшими сосед€ми
dbscan::kNNdistplot(df, k = 5)
abline(h = 0.15, lty = 2)
