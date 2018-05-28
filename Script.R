library(tidyverse)
library(ggrepel)


datos<-read.csv('Datos/Milenials.csv', header = T, sep = ';')

## Resumenes 

    # sexo

      sexR<-datos%>%count(Sexo)%>%
        mutate(prop = prop.table(n)*100)
      
      
      sexR$fraction<- sexR$n / sum(sexR$n)
      sexR = sexR[order(sexR$fraction), ]
      sexR$ymax = cumsum(sexR$fraction)
      sexR$ymin = c(0, head(sexR$ymax, n=-1))
      
      
      ggplot(sexR, aes(fill=Sexo, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme_minimal() + theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
                           legend.text = element_text(colour = "black", size = 15), 
                           panel.grid = element_blank(),
                           axis.text = element_blank(),
                           axis.title = element_blank(),
                           axis.ticks = element_blank())+
         geom_label_repel(aes(label = paste(round(prop,2),"%"),
                              x = 4, y = (ymin + ymax)/2),inherit.aes = F, 
                          show.legend = F, size = 5)+
        annotate("text", x = 0, y = 0, size = 12, label = "Sexo")
      
      
      
      
      
    #Residencia
      
      resR<-datos%>%count(Residencia)%>%
        mutate(prop = prop.table(n)*100)
    
    #Estrato
      
      estR<-datos%>%count(Estrato)%>%
        mutate(prop = prop.table(n)*100)
      
      
      ggplot(estR, aes(as.factor(Estrato), prop)) +
        geom_bar(stat = "identity")  +
        geom_text(aes(label = paste0(round(prop,2), "%"), y = prop),
                  vjust = 1.4, size = 5, color = "white") + labs (x='Estrato', y='Porcentaje') +
        theme_minimal() + theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
                           legend.text = element_text(colour = "black", size = 15), 
                                                     axis.ticks = element_blank())
    
      
      
      
      # Frecuencia Compra
      frecR<-datos%>%count(Frecuencia_compra)%>%
        mutate(prop = prop.table(n)*100)

      
      frecR$Frecuencia_compra <- factor(frecR$Frecuencia_compra, levels = frecR$Frecuencia_compra[order(-frecR$prop)])
      ggplot(frecR, aes(Frecuencia_compra, prop)) +
        geom_bar(stat = "identity")  +
        geom_text(aes(label = paste0(round(prop,2), "%"), y = prop),
                  vjust = 1.1, size = 5, color = "white") + labs (x='Frecuencia de compra', y='Porcentaje') +
        theme_minimal() + theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
                                legend.text = element_text(colour = "black", size = 15), 
                                axis.ticks = element_blank())
      
      
      
      
      # Sitio compra
      SitioC<-datos%>%count(Sitio_compra)%>%
        mutate(prop = prop.table(n)*100)

    # COmbine_nueva
      CombineR<-datos%>%count(Combine_nueva)%>%
        mutate(prop = prop.table(n)*100)

      CombineR$fraction<- CombineR$n / sum(CombineR$n)
      CombineR = CombineR[order(CombineR$fraction), ]
      CombineR$ymax = cumsum(CombineR$fraction)
      CombineR$ymin = c(0, head(CombineR$ymax, n=-1))
      
      
      ggplot(CombineR, aes(fill=Combine_nueva, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme_minimal() + theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
                                legend.text = element_text(colour = "black", size = 15), 
                                panel.grid = element_blank(),
                                axis.text = element_blank(),
                                axis.title = element_blank(),
                                axis.ticks = element_blank())+
        geom_label_repel(aes(label = paste(round(prop,2),"%"),
                             x = 4, y = (ymin + ymax)/2),inherit.aes = F, 
                         show.legend = F, size = 5)+
        annotate("text", x = 0, y = 0, size = 8, label = "Ropa nueva")
      
      
      
      
      # Info_etiqueta
      
      infoLabR<-datos%>%count(Info_etiqueta)%>%
        mutate(prop = prop.table(n)*100)
      
      
      infoLabR$fraction<- infoLabR$n / sum(infoLabR$n)
      infoLabR = infoLabR[order(infoLabR$fraction), ]
      infoLabR$ymax = cumsum(infoLabR$fraction)
      infoLabR$ymin = c(0, head(infoLabR$ymax, n=-1))
      
      
      ggplot(infoLabR, aes(fill=Info_etiqueta, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme_minimal() + theme(legend.text = element_text(colour = "black", size = 15), 
                                panel.grid = element_blank(),
                                axis.text = element_blank(),
                                axis.title = element_blank(),
                                axis.ticks = element_blank(),
                                legend.title=element_blank(),
                                legend.position="bottom")+
        geom_label_repel(aes(label = paste(round(prop,2),"%"),
                             x = 4, y = (ymin + ymax)/2),inherit.aes = F, 
                         show.legend = F, size = 5)+
        annotate("text", x = 0, y = 0, size = 6, label = "¿Lee la etiqueta del producto\n al comprar?")
    # Impacto_textil
      
      impactoR<-datos%>%count(Impacto_textil)%>%
        mutate(prop = prop.table(n)*100)
      
    #Calf_impacto
      CalImp<-datos%>%count(Cal_impacto)%>%
        mutate(prop = prop.table(n)*100)
      
      CalImp$fraction<- CalImp$n / sum(CalImp$n)
      CalImp = CalImp[order(CalImp$fraction), ]
      CalImp$ymax = cumsum(CalImp$fraction)
      CalImp$ymin = c(0, head(CalImp$ymax, n=-1))
      
      
      CalImp$Cal_impacto <- factor(CalImp$Cal_impacto, levels = CalImp$Cal_impacto[order(-CalImp$prop)])
      
      ggplot(CalImp, aes(fill=CalImp$Cal_impacto, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme_minimal() + theme(legend.text = element_text(colour = "black", size = 15), 
                                panel.grid = element_blank(),
                                axis.text = element_blank(),
                                axis.title = element_blank(),
                                axis.ticks = element_blank(),
                                legend.title=element_blank(),
                                legend.position="bottom")+
        geom_label_repel(aes(label = paste(round(prop,2),"%"),
                             x = 4, y = (ymin + ymax)/2),inherit.aes = F, 
                         show.legend = F, size = 5)+
        annotate("text", x = 0, y = 0, size = 6, label = "Impactos de la industria textil")
    
      
      
      
      # Dispoción_impacto
      dispI<-datos%>%count(Disposicion_ropa)%>%
        mutate(prop = prop.table(n)*100)
      
      
      dispI$Disposicion_ropa <- factor(dispI$Disposicion_ropa, levels = dispI$Disposicion_ropa[order(-dispI$prop)])
      ggplot(dispI, aes(Disposicion_ropa , prop)) +
        geom_bar(stat = "identity")  +
        geom_text(aes(label = paste0(round(prop,2), "%"), y = prop),
                  vjust = 1.1, size = 5, color = "white") + labs (x='Frecuencia de compra', y='Porcentaje') +
        theme_minimal() + theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
                                legend.text = element_text(colour = "black", size = 15), 
                                axis.ticks = element_blank())
      
      
      
    #Uso_ropasegunda
      uso_Seg<-datos%>%count(Uso_ropasegunda)%>%
        mutate(prop = prop.table(n)*100)
      
      
      uso_Seg$fraction<- uso_Seg$n / sum(uso_Seg$n)
      uso_Seg = uso_Seg[order(uso_Seg$fraction), ]
      uso_Seg$ymax = cumsum(uso_Seg$fraction)
      uso_Seg$ymin = c(0, head(uso_Seg$ymax, n=-1))
      
      
  
      
      ggplot(uso_Seg, aes(fill=uso_Seg$Uso_ropasegunda, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme_minimal() + theme(legend.text = element_text(colour = "black", size = 15), 
                                panel.grid = element_blank(),
                                axis.text = element_blank(),
                                axis.title = element_blank(),
                                axis.ticks = element_blank(),
                                legend.title=element_blank(),
                                legend.position="bottom")+
        geom_label_repel(aes(label = paste(round(prop,2),"%"),
                             x = 4, y = (ymin + ymax)/2),inherit.aes = F, 
                         show.legend = F, size = 5)+
        annotate("text", x = 0, y = 0, size = 6, label = "Uso de ropa de segunda mano")
      
      
    # Origen compra 
      oriC<-datos%>%count(Origen_compra)%>%
        mutate(prop = prop.table(n)*100)
      
      
      oriC$fraction<- oriC$n / sum(oriC$n)
      oriC = oriC[order(oriC$fraction), ]
      oriC$ymax = cumsum(oriC$fraction)
      oriC$ymin = c(0, head(oriC$ymax, n=-1))
      
      
      
      
      ggplot(oriC, aes(fill=oriC$Origen_compra, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme_minimal() + theme(legend.text = element_text(colour = "black", size = 15), 
                                panel.grid = element_blank(),
                                axis.text = element_blank(),
                                axis.title = element_blank(),
                                axis.ticks = element_blank(),
                                legend.title=element_blank(),
                                legend.position="bottom")+
        geom_label_repel(aes(label = paste(round(prop,2),"%"),
                             x = 4, y = (ymin + ymax)/2),inherit.aes = F, 
                         show.legend = F, size = 5)+
        annotate("text", x = 0, y = 0, size = 6, label = "Origen de las prendas")
      
      
      chisq.test(table(datos$Sexo,datos$Info_etiqueta)
                  ,simulate.p.value = TRUE)
      
      
###################### Inferencia ########################################
      
      ### Frecuencia de compra 
      
      datos$Frecuencia_compra<-factor(datos$Frecuencia_compra, 
                                      levels = levels(datos$Frecuencia_compra), labels = c('3 a 4', '2', 
                                                                                           '0', '1', '12', '52'))
      
      
      datos2<-datos %>%
        mutate(Frecuencia_compra= recode(Frecuencia_compra,
                                          "c('1', '2')='1 a 2'"))
      
      ## Debido a la baja frecuencia se juntarán algunas categorías como acontinuación: 
      
      
      # Sexo 
      
      
   FC<-datos2 %>%  count(Sexo, Frecuencia_compra)%>%            
        mutate(prop = prop.table(n)) 
      
      
      fc2<-table(datos2$Sexo,datos2$Frecuencia_compra)
      chisq.test(fc2, simulate.p.value = T)
      
      assoc(fc2)
      assocstats(fc2)
      
      FC$Frecuencia_compra <- factor(FC$Frecuencia_compra , levels = FC$Frecuencia_compra[order(-FC$Frecuencia_compra )])
      ggplot(FC, aes(x=FC$Frecuencia_compra,y=prop*100)) +
        geom_bar(stat="identity", position="stack", aes(fill=Sexo)) +
        coord_flip() +
        theme_minimal() +
        theme(legend.position="bottom") + scale_fill_brewer(palette = "Dark2")
      
      
      ## Combine nueva  
      
      
      FC_cn<-datos2 %>%  count(Combine_nueva, Frecuencia_compra)%>%            
        mutate(prop = prop.table(n)) 
      
      fc3<-table(datos2$Combine_nueva,datos2$Frecuencia_compra)
      chisq.test(fc3)
      assocstats(fc3)
      ggplot(FC_cn, aes(x=FC_cn$Frecuencia_compra,y=prop*100)) +
        geom_bar(stat="identity", position="stack", aes(fill=Combine_nueva)) +
        coord_flip() +
        theme_minimal() +
        theme(legend.position="bottom") + scale_fill_brewer(palette = "Dark2")
      
      
      
      chisq.test(xtabs( ~datos$Estrato+datos$Combine_nueva))
      
      
      
    
      
      gg_prop <- ggplot(data = data.frame()
                  , aes(x = Sexo, y = prop, fill = Info_etiqueta)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3)



    sex_frec<-datos  %>%  count(Sexo, Info_etiqueta)%>%            # group_by() & summarise(n = n()) are implicit
      mutate(prop = prop.table(n)) 

    gg_prop %+%   # use %+% to add...
      sex_frec
    
    sexChi<-sex_frec %>% select(-prop) %>%spread(datos$Frecuencia_compra, n)
    chisq.test(sexChi)
    
    
    sex<-datos %>% count(Sexo, Info_etiqueta, Cal_impacto)  %>%            # group_by() & summarise(n = n()) are implicit
      mutate(prop = prop.table(n)) 
    
    
  
    titanic_wide <- data.frame(sex)
    
    ggplot(data = titanic_wide,
           aes(axis1 = Sexo, axis2 = Cal_impacto, axis3=Info_etiqueta, 
               weight = n)) +
            geom_alluvium(aes(fill = Cal_impacto)) +
      geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
      theme_minimal()
    
    
    ggplot(sex_frec, aes(x=sex_frec$Info_etiqueta,y=prop*100)) +
      geom_bar(stat="identity", position="stack", aes(fill=Sexo)) +
      coord_flip() +
      theme_minimal() +
      theme(legend.position="bottom")

  chisq.test(table(datos$Sexo, datos$Frecuencia_compra))    
  
    