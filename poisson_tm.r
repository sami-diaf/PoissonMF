
# Poisson Factorization
# Topic models with Poisson distribution (alternative to Poisson Hierarchical Models)

rm(list=ls()) # cleaning memory

library(dplyr)
library(quanteda)
library(tidyverse)
library(ggplot2)
library(austin)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(tidytext)
library(poismf)
library(Matrix)

options(stringsAsFactors = F)

# example in quanteda

data_corpus_sotu <- readRDS(url("https://quanteda.org/data/data_corpus_sotu.rds"))

toks_sotu <- tokens(data_corpus_sotu,
                     remove_numbers = TRUE,
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_separators = TRUE,
                     remove_url = TRUE,
                     split_hyphens=TRUE
) %>% # tokens_wordstem() %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  #tokens_remove(pattern = stopwords("de", source = "marimo"))
  tokens_select(min_nchar = 2) #%>% tokens_wordstem()

# trimming the dfm
vdfm <- dfm(toks_sotu) %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 1)

mat <- quanteda::convert(vdfm, to="matrix") %>% Matrix::Matrix()

## good quality, but slow
model <- poismf(k=5, mat, method="tncg")

str(model)

model$A

model$B

model$A %>% t() %>% 
  as.data.frame(row.names = mat@Dimnames$docs) %>% 
  arrange(-V1) %>% 
  View()

model$A %>% t() %>% 
  as.data.frame(row.names = mat@Dimnames$docs) %>%
  ggplot(aes(x=V1, y=V2, label = rownames(.))) + 
  geom_point() +
  geom_text(position =position_jitter(width=0.1,height=0.1))

model$B %>% t() %>% 
  as.data.frame(row.names = mat@Dimnames$features) %>% 
  arrange(-V1) %>% 
  View()

a_mat <- model$A %>% t() %>% 
  as.data.frame(row.names = mat@Dimnames$docs)

a_mat %>% as.data.frame() %>% 
  arrange(-V1) %>% 
  View() 

set.seed(987456321)

library(cluster)
library(fpc)

clus <- kmeans(a_mat, centers=5)

plotcluster(a_mat, clus$cluster)  

clusplot(a_mat, clus$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

clus$cluster[clus$cluster==1]

# PCA

mat_pca <- prcomp(a_mat, center = TRUE,scale. = TRUE)

summary(mat_pca)

library(ggbiplot)

ggbiplot(mat_pca, labels=rownames(a_mat))

