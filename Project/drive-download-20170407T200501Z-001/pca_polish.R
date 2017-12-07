# clean_polish_dt = reduce_impute_polish(polish_dt)

pca = prcomp(clean_polish_dt[,-c(58,57,56,47,46,27,26,21,14)])

X = as.matrix(clean_polish_dt[,-c(58,57,56,47,46,27,26,21,14)])
M = pca$rotation[,1:5]

pca_polish_data = as.data.frame((X %*% M))
pca_polish_data = log((pca_polish_data^2))
pca_polish_data = cbind(pca_polish_data,clean_polish_dt[,c(58,57,56,47,46,27,26,21,14)])
pca_polish_data$year = as.factor(pca_polish_data$year)

# library(GGally)
# 
# gg = ggpairs(pca_polish_data,columns = c(1:5,12,13) , mapping = aes(color = class, shape = class, alpha = 0.35))
# gg
