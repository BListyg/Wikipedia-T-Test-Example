library(MASS)

#####Unpaired

wikipedia_example <- function(n=20,x=x){
  x <- x
  
  wiki_data <- mvrnorm(n = n, 
                       mu = c(0,0), 
                       Sigma = matrix(c(1,x,x,1), byrow = T, ncol = 2)
  ) %>% data.frame
  
  return(t.test(wiki_data$X1, wiki_data$X2,'two.sided')$p.value)
}

wikipedia_example_sim <- function(x){table(replicate(n =10000,wikipedia_example(x = x))<0.05)[2]/10000}

unpaired_results <- apply(matrix(seq(-1,1,0.1)), 1, wikipedia_example_sim)

unpaired_results[is.na(unpaired_results)] <- 0

plot(unpaired_results, type='l')

#####Paired

wikipedia_example <- function(n=20,x=x){
  x <- x
  
  wiki_data <- mvrnorm(n = n, 
                       mu = c(0,0), 
                       Sigma = matrix(c(1,x,x,1), byrow = T, ncol = 2)
  ) %>% data.frame
  
  return(t.test(wiki_data$X1, wiki_data$X2,'two.sided',paired=T)$p.value)
}

wikipedia_example_sim <- function(x){table(replicate(n =10000,wikipedia_example(x = x))<0.05)[2]/10000}

paired_results <- apply(matrix(seq(-1,1,0.1)), 1, wikipedia_example_sim)

paired_results[is.na(unpaired_results)] <- 0

lines(paired_results, type='l')
