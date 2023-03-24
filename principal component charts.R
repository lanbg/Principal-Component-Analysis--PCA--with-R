# Script para montar gr?ficos de an?lise de componentes principais

# Instalacao e carregamento dos pacotes
lista.de.pacotes <- c("FactoMineR", "factoextra", "corrplot")
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in% installed.packages()[,"Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes)
lapply(lista.de.pacotes, require, character.only = TRUE)

# Carregando os dados
data(decathlon2)

decathlon2

decathlon2.active <- decathlon2[1:23, 1:10]

# ROdar o PCA
res.pca <- PCA(decathlon2.active, graph = FALSE)

eig.val <- get_eigenvalue(res.pca)

eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50), title="")

var <- get_pca_var(res.pca)
var

# Coordinates / autovetores (DIM é o CPA1 , CPA2 ...)
head(var$coord)

# Cos2: quality on the factore map/ 
#qual a porcentagem de variancia daquela variavel naquele CPA (é bom para eliminar variáveis)
head(var$cos2)

# Contributions to the principal components/ 
#esse é mais fácil para verificar se pode eliminar a variável(se explica pouco não vai te servir para muito coisa)
head(var$contrib)

fviz_pca_var(res.pca, col.var = "black")

corrplot(var$cos2, is.corr=FALSE) #esse mostra a correlação da variavel com os PCA

# Total cos2 of variables on Dim.1 and Dim.2/ mostrando a importância somente dos CPA 1 e 2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
#só CAP 1 e 3 mas o padrão é 1 e 2
fviz_cos2(res.pca, choice = "var", axes = c(1,3))
#contribuição nos CPA 1, 2 e 3
fviz_cos2(res.pca, choice = "var", axes = 1:3)

# Color by cos2 values: quality on the factor map
#repel tenta no máximo possível não deixar os nomes uma em cima do outro
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

# Change the transparency by cos2 values
fviz_pca_var(res.pca, alpha.var = "cos2")

corrplot(var$contrib, is.corr=FALSE) 

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Change the transparency by contrib values
fviz_pca_var(res.pca, alpha.var = "contrib")

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)

# Color variables by groups
fviz_pca_var(res.pca, col.var = grp,
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)

# Description of dimension 1
res.desc$Dim.1

# Description of dimension 2
res.desc$Dim.2

ind <- get_pca_ind(res.pca)
ind

# Coordinates of individuals
head(ind$coord)

# Quality of individuals
head(ind$cos2)

# Contributions of individuals
head(ind$contrib)

fviz_pca_ind(res.pca)

fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(res.pca, pointsize = "cos2",
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_cos2(res.pca, choice = "ind")

# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)
