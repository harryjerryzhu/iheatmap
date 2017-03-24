top_n_vars_genes<-function(data,n=1000){
  data<-data
  data<-t(scale(t(data),scale=F))
  vars<-apply(data,1,var)
  sorted_vars<-sort(vars,decreasing=T)
  genes<-names(sorted_vars)
  top_n_genes<-genes[1:n]
  return (top_n_genes)
}
