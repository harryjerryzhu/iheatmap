# Create a iheatmap class
# an iheatmap class should contain expression data and profile data

IHeatMap<-setClass(
  "IHeatMap",
  contains = "data.frame",

  slots=c(
    expression="data.frame",
    profile="data.frame",
    ihp="list"
    ),

  validity=function(object){
    if(is.null(object@expression))
      return ("expression can't be null!")
    else {
      if (is.null(object@profile))
        return (TRUE)
      else {
        if (all(colnames(object@expression)==colnames(object@profile)))
          return (TRUE)
        else
          return ("expression and profile should share same column names")
  }}}
)

setGeneric(name="create_iheatmap",
           def=function(theObject,k_hcluster_color=1,k_vcluster_color=1,xaxis_label_size=5,yaxis_label_size=5,colorbartitle="z-value",
                        colorbarposition=NULL,nchar_anno=NULL){
             standardGeneric("create_iheatmap")
           })
setMethod(f="create_iheatmap",
          signature="IHeatMap",
          definition=function(theObject,k_hcluster_color=1,k_vcluster_color=1,xaxis_label_size=5,yaxis_label_size=5,colorbartitle="z-value",
                              colorbarposition=NULL,nchar_anno=NULL){
            require(dplyr)
            require(plotly)
            require(reshape2)
            require(dendextend)
            theObject@ihp=i_heatmap(theObject@expression,theObject@profile,k_hcluster_color=k_hcluster_color,k_vcluster_color=k_vcluster_color,xaxis_label_size=xaxis_label_size,yaxis_label_size=yaxis_label_size,colorbartitle=colorbartitle,
                                    colorbarposition=colorbarposition,nchar_anno=nchar_anno)
            return(theObject)
          })

setGeneric(name="get_iheatmap",
           def=function(theObject){
             standardGeneric("get_iheatmap")
           })
setMethod(f="get_iheatmap",
          signature="IHeatMap",
          definition=function(theObject){
            return(theObject@ihp["iheatmap"])
          })

setGeneric(name="get_iheatmap_data",
           def=function(theObject){
             standardGeneric("get_iheatmap_data")
           })
setMethod(f="get_iheatmap_data",
          signature="IHeatMap",
          definition=function(theObject){
            return(theObject@ihp["iheatmapdata"])
          })
