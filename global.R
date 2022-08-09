
#run periodically
#devtools::install_github("rstudio/shiny")

library(data.table)
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)


#load data
A<-read.csv("data/additive_breeding_values.csv",stringsAsFactors = F)
G<-read.csv("data/total_genetic_values.csv",stringsAsFactors = F)
P<-read.csv("data/phenotypic_LSmeans.csv",stringsAsFactors = F)

#load dictionary
keys<-read.csv("data/naming_key.csv",stringsAsFactors = F)

#Data prep

data.prep<-function(x){
  choice<-c("values","Name","Germplasm.type",keys[complete.cases(keys),"ID"])
  x1<-select_(x,.dots = choice)
  x2<-data.frame(values = x1$values,Individual = x1$Name,
                 Germplasm = x1$Germplasm.type,apply(x1[,-c(1,2,3)],2,signif, digits = 4))
  colnames(x2)[4:ncol(x2)]<-sapply(colnames(x2)[4:ncol(x2)], function(y) keys[keys$ID %in% y,2])
  #rownames(x2)<-x2$Individual 
  x2$Germplasm<-gsub("progeny","offspring",x2$Germplasm)
  return(x2)
}

A1<-data.prep(A)
G1<-data.prep(G)
P1<-data.prep(P)

#max_plots<-ncol(A1) - 2
myColors<-c("#E69F00","#B3B3B3", "#1874CD")
# functions

scatter_func<-function(x){
  
  #create list of category/traits
  p<-list()
  for(i in seq(4,ncol(x),1)) {
    p[[i]]<-as.data.frame(x[,c(2,3,i)])
  }
  
  #first 3 entries are null, the following step removes that.
  p[[1]]<-NULL
  p[[1]]<-NULL
  p[[1]]<-NULL
  
  #Make the plots
  
  myColors<-c("#E69F00","#999999", "dodgerblue3")
  
  #run function through list
  s.plots<-lapply(p, function(j) {
    ggplot(data=j, aes(x="", y=j[,3],colour = Germplasm,text = Individual)) +
      #ylab(names(j)[3]) +
      geom_jitter(width=0.3,alpha = 0.7,size = 2.5) +
      scale_colour_manual(name = "Germplasm",values = myColors) +
      theme_bw() +
      xlab("") +
      ylab("") +
      ggtitle(names(j[3])) +
      theme(axis.title.x=element_blank(),
            axis.text.y=element_text(size=12,colour="black"),
            axis.ticks.x=element_blank(),
            legend.position="none")
  })
  #names(s.plots)<-paste0("plot",1:length(s.plots))
  return(s.plots)
}
  

bi_scatter_func<-function(x,y){
  
  
}



#test scripts for the functions
# test.a<-A1[,1:8]
# pp<-scatter_func(test.a)
# pp1<-pp[[1]]
# library(gridExtra)
# marrangeGrob(pp,ncol=3,nrow = 1, top=NA)

# 
# ggplotly(pp1, tooltip = "Individual")
# lapply(pp,function(x){
#   ggplotly(x, tooltip = "Individual")
# })

# still having trouble: 
# colnames(test.a)[3:4]<-c("Days","GDD")
# plot_ly(test.a,x ~ Days, y = ~ GDD)

#removed extra objects
rm(A,G)

