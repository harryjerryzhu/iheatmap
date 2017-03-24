require(dplyr)
require(plotly)
require(reshape2)
require(dendextend)

g<-dplyr::group_by(iris,Species)
new_iris<-as.data.frame(mutate(g,id=paste(Species,seq_len(n()),sep="_")))
rownames(new_iris)<-new_iris$id
new_iris<-select(new_iris,-c(5,6))
new_iris<-as.data.frame(t(scale(t(new_iris))))
data<-sample_n(new_iris,50)

profile<-t(data.frame(Sex=c("Male","Female","Male","Female"),Age=c("Young","Young","Old","Old")))
colnames(profile)<-colnames(data)
profile<-as.data.frame(profile)


# hide_xaxis-- hide x axises
hide_xaxis = function(p){
  l=layout(p,
           # hide x axis
           xaxis=list(
             showgrid = FALSE,
             showline = FALSE,
             showticklabels = FALSE,
             ticks = "",
             title = ""
           )
          )
  return(l)
}

# put y in right side
right_yaxis<-function(p){
  l=layout(p,
           #put y axis right
           yaxis=list(
             side="right",

             showgrid = FALSE,
             showline = FALSE,
             showticklabels = TRUE,
             ticks = "",
             dtick=1,
             title = ""
           )
           )
  return(l)
}

# produce  hclust dendrogram
# with row-level as default
# for column-level, pass direction="vertical"
# mark the dendrogram with k_color colors
# return the dendrogram
# and dendrogram order

hc_dend<-function(data,direction="horizontal",k_colors=1){
  horiz=T
  if (direction=="horizontal")
    data=data
  if(direction=="vertical"){
    data<-t(data)
    horiz=F
  }
  dend<- data %>% dist(method="euclidean") %>% hclust(method="ward.D2") %>% as.dendrogram
  ggdend <- dend %>% set("labels","") %>% set("branches_lwd",0.3) %>% set("branches_k_color",k=k_colors)%>% as.ggdend
  ggdend<-ggplot(ggdend,horiz=horiz)+theme(legend.position="none",plot.margin = unit(c(0,0,0,0), "cm"))
  ggdend<-ggplotly(ggdend) %>% layout(yaxis=list(side="right")) %>% layout(plot_bgcolor='rgb(255, 2255, 255)') %>%
    layout(paper_bgcolor='rgb(255, 255, 255)')
  return (list(dend=dend,ggdend=ggdend))
}

ann_heatmap<-function(v_dend,profile,nchar_anno){
  if(is.null(nchar_anno)) nchar_anno=3
  v_order<-order.dendrogram(v_dend$dend)
  profile<-profile[,v_order]
  profile$id=rownames(profile)
  m_profile<-melt(profile,id="id")
  m_profile$id<-factor(m_profile$id,levels=rev(rownames(profile)))
  m_profile$variable<-factor(m_profile$variable,levels=colnames(profile))
  p=ggplot(m_profile,aes(x=variable,y=id))+
    geom_tile(aes(fill=value),color="grey",height=0.9)+
    geom_text(aes(label=substr(value,1,nchar_anno)))+
    scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0),position="right")+
    labs(x="",y="")+
    theme_void()+
    theme(legend.position="none",
          axis.ticks=element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=10),
          axis.line.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"))
  pann<-ggplotly(p) %>% layout(yaxis=list(side="right"),margin=list(l = 0, r = 100, b = 50, t = 50, pad = 0)) %>% layout(plot_bgcolor='rgb(255, 255, 255)') %>%
    layout(paper_bgcolor='rgb(255, 255, 255)')

  return(pann)
}


# produce heatmap for data
heatmap<-function(data,h_dend=NULL,v_dend=NULL,xaxis_label_size,yaxis_label_size,colorbartitle,colorbarposition){
  if (!is.null(h_dend)){
    h_order<-rev(order.dendrogram(h_dend$dend))
    data<-data[h_order,]
  }

  if (!is.null(v_dend)){
    v_order<-order.dendrogram(v_dend$dend)
    data<-data[,v_order]
  }

  if(is.null(colorbarposition)) colorbarposition<-max(sapply(rownames(data),nchar))/60+1
  bottom_space=max(sapply(colnames(data),nchar))*9
  colors<-colorRamp(c("green","black","red"))
  h_data<-data
  h_data$id<-rownames(h_data)
  h_data<-melt(h_data,id="id")
  h_data$variable<-factor(h_data$variable,levels=colnames(data))
  h_data$id<-factor(h_data$id,levels=rev(rownames(data)))

  pheatmap<-plot_ly(x=h_data$variable,y=h_data$id,z=h_data$value,colors=colors) %>%
    add_heatmap(colorbar=list(x=colorbarposition,len=0.5,title=colorbartitle)) %>%
    layout(xaxis=list(showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      ticks = "",
                      dtick=1,
                      tickangle=90,
                      title = "",
                      tickfont=list(
                        family='Old Standard TT, serif',
                        size=xaxis_label_size,
                        color='black'
                      )),
           yaxis=list(tickfont=list(
             family='Old Standard TT, serif',
             size=yaxis_label_size,
             color='black'
           )),
           margin=list(b=bottom_space)
           ) %>%
    right_yaxis()
  return(list(data=data,heatmap=pheatmap))
}

i_heatmap<-function(data,profile=NULL,k_hcluster_color=1,k_vcluster_color=1,xaxis_label_size=5,yaxis_label_size=5,colorbartitle="z-value",
                   colorbarposition=NULL,nchar_anno=NULL){
  options(warn=-1)

  h_dend<-hc_dend(data,k_color=k_hcluster_color)
  v_dend<-hc_dend(data,k_color=k_vcluster_color,direction="vertical")
  hp<-heatmap(data,h_dend,v_dend,xaxis_label_size,yaxis_label_size,colorbartitle,colorbarposition)

  # align the heatmap position
  h=(0.955-0.045)/nrow(data)/2
  v=(1.045+0.045)/ncol(data)/2

   p1<-plotly_empty()
   p2<-v_dend$ggdend %>% layout(xaxis=list(domain=c(v-0.045,1.045-v)))

   top_subplot<-subplot(p1,p2,nrows=1,widths=c(0.2,0.8),margin=0)
  # v_dendrogram<-v_dend$ggdend
  # ann<-ann_heatmap(v_dend,profile)
  # hmap<-hp$heatmap


  if(!is.null(profile)){
    p3<-plotly_empty()
    p4<-ann_heatmap(v_dend,profile,nchar_anno)
    mid_subplot<-subplot(p3,p4,nrows=1,widths=c(0.2,0.8),margin=0)
  }
  else{
    mid_subplot<-NULL
  }

  p5<-h_dend$ggdend
  p6<-hp$heatmap  %>% layout(yaxis=list(domain=c(0.045-h,0.955+h)))
  # p7<-lheatmap(hp)
  # legend_subplot<-subplot(p7$lhp,p7$lt,nrows=1,widths=c(0.9,0.1),margin=0,shareY = T) %>% layout (yaxis=list(domain=c(0.4,0.6)),xaxis=list(domain=c(0.7,0.9)))
  #
  bot_subplot<-subplot(p5,p6,nrows=1,widths=c(0.2,0.8),margin=0)

  if(!is.null(mid_subplot)){
    main_plot<-subplot(top_subplot,mid_subplot,bot_subplot,nrows=3,heights=c(0.1,0.1,0.8),margin=0)
  }
  else{
    main_plot<-subplot(top_subplot,bot_subplot,nrows=2,heights=c(0.1,0.9),margin=0)
  }

  return(list(iheatmapdata=hp$data,iheatmap=main_plot))
}


hello=i_heatmap(data,profile,k_hcluster_color=3,k_vcluster_color=2,colorbartitle="hello")
