#
# ## to make consistence of biased variance and std
Biased_Var = function(x){
  # x : a vector
  MEAN = mean(x)
  temp = 0
  for (i in x){
    temp = temp + (i - MEAN)^2
  }
  return(temp/length(x))
}
# Biased_Var(c(1,2,3))

## PCA


# # PCA

pca = function(df){
  pr <- prcomp(df, scale = TRUE)
  print(pr)
  library(ggfortify)
  p <- autoplot(pr, data = df, alpha = I(1/5),
                colour = 'reg_tsp',
                loadings = TRUE, loadings.colour = 'orange',
                main = 'PCA two components')
  print(p)
  
  plot(pr, main = 'sceen plot')
}


