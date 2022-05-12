library(ggplot2)
library(reshape2)
library(dplyr)
library(stringr)
library(tidyr)


#InputFileData <-"cuu_bd2.csv"
#Dir <- "/home/valeria/Documentos/Resultados_Llamada_variantes/cuu_bd2.csv"
#setwd(Dir)

#Cargar archivo
Data <-read.csv("/home/valeria/Documentos/Resultados_Llamada_variantes/cuu_bd.csv")
#Solo leer de la columna 2 a la 5, la 1 es de cromosomas
df <-data.frame(Data[2:5])
#ELiminar esos caracteres
df <- df[!(df$General_function=="#N/D"|df$General_function=="Not characterized"),]


df$Representative_group <-paste(df$General_function, ",", df$Representative_group)
#Eliminar columna
df$General_function <- NULL
#Agrupar datos
ans <- df %>% 
  group_by(Representative_group)%>% 
  summarise(Numero_SNPs=sum(Numero_SNPs), Numero_genes=sum(Numero_genes)) 



ans_2 <- str_split_fixed(ans$Representative_group,',',2)
ans_2 <- as.data.frame(ans_2)
ans_2$General_function <- ans_2$V1
ans_2$Representative_group <- ans_2$V2

#Eliminar columnas
ans_2$V1 <- NULL
ans_2$V2 <- NULL

#Eliminar columnas de otra tabla
ans$Representative_group<- NULL
#Unir tablas
ans_3 <- bind_cols(ans_2, ans) 

#Ordenar datos
df3 <-data.frame(ans_3)
sort.df <- df3[order(df3$Numero_genes) , ] 
sort.df <-cbind("No"=1:nrow(sort.df),sort.df)
sort.df <- sort.df[order(sort.df$Numero_genes) , ] 
#sort.df <- sort.df[order(sort.df) ,sort.df$No ]


df.long<-melt(sort.df, id.vars=c("General_function","Representative_group","No"))



df3 <-cbind("No"=1:nrow(df3),df3)
df.long<-melt(df3, id.vars=c("General_function","Representative_group","No"))
df.long <- df.long[order(df.long$No) , ] 

#Hacer gráfico
p<-ggplot(df.long,aes(No, value,fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  theme(
    plot.title =element_blank(),
    axis.text.x=element_text(angle=90,size=18,hjust=0.95,vjust=0.5),
    strip.text.x = element_text(size=18)
  )+
  #facet_grid(~General_funtion, scales="free" ,space="free")+
  
  facet_wrap(~General_function, scales="free_x", ncol=1)+
  
  scale_x_continuous(breaks =seq(from = min(df3$No), 
                                 to=max(df3$No), by =1),
                     labels = df3$Representative_group, expand = c(0, 0.5) )
#Exportar archivo
ggsave(filename="final_CUU.pdf",p,dpi=50,width = 400, height = 500, units = "cm", limitsize = FALSE)
