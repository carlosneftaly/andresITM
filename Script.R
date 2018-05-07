library(tidyverse)



datos<-read.csv('Datos/Milenials.csv', header = T, sep = ';')

## Resumenes 

    # sexo

  gg_prop <- ggplot(data = data.frame()
                  , aes(x = Sexo, y = prop, fill = Frecuencia_compra)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3)



    sex_frec<-datos  %>%  count(Sexo, Frecuencia_compra)%>%            # group_by() & summarise(n = n()) are implicit
      mutate(prop = prop.table(n)) 

    gg_prop %+%   # use %+% to add...
      sex_frec
    
    sexChi<-sex_frec %>% select(-prop) %>%spread(Frecuencia_compra, n)
    chisq.test(sexChi)
    
    
    sex<-datos %>% count(Sexo, Estrato, Info_etiqueta, Frecuencia_compra)  %>%            # group_by() & summarise(n = n()) are implicit
      mutate(prop = prop.table(n)) 
    
    
  
    titanic_wide <- data.frame(sex)
    
    ggplot(data = titanic_wide,
           aes(axis1 = Estrato, axis2 = Sexo, axis3=Frecuencia_compra, 
               weight = n)) +
      scale_x_discrete(limits = c("Sexo", "Estrato"), expand = c(.1, .05)) +
      geom_alluvium(aes(fill = Info_etiqueta)) +
      geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
      theme_minimal()
    
    
    ggplot(sex_frec, aes(x=Frecuencia_compra,y=prop*100)) +
      geom_bar(stat="identity", position="stack", aes(fill=Sexo)) +
      coord_flip() +
      scale_fill_brewer(palette="YlGnBu") +
      theme_minimal() +
      theme(legend.position="bottom")

  chisq.test(table(datos$Sexo, datos$Frecuencia_compra))    
    