library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)
class(tissue_gene_expression$x)

d <- dist(tissue_gene_expression$x)
as.matrix(d)[c(1,2,39,40,73,74), c(1,2,39,40,73,74)]

image(as.matrix(d))