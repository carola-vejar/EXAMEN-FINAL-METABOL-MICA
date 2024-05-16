#Creadora: Carola Véjar Quiroga
library(pacman)

p_load("readr", #para lamar las bases de datos
       "ggplot2", #para graficar
       "ggrepel", #para etiquetar datos en una gráfica
       "dplyr", #facilita el manejo de datos
       "matrixTests") #realiza prueba estadística
datos <- read_csv(file="https://raw.githubusercontent.com/ManuelLaraMVZ/Transcript-mica/main/examen2")
datos

#extracción de genes controles (referencia)
controles <- datos %>% 
  filter(Condicion=="Control")
head(controles)

#sacar promedios de los controles 
promedio_controles <-  controles %>% 
  summarise(Mean_C1 = mean(Cx1),
            Mean_C2 = mean(Cx2),
            Mean_C3 = mean(Cx3),
            Mean_T1 = mean(T1),
            Mean_T2 = mean(T2),
            Mean_T3 = mean(T3)) %>% 
  mutate(Gen="Promedio_controles") %>% 
  select(7,1,2,3,4,5,6) #reordenar las columnas
promedio_controles

###################################################
#extraer los genes de la tabla "datos"

genes <- datos %>% 
  filter(Condicion=="Target") %>% 
  select(-2)
head(genes)
##########
#Sacar el 2^-DCT

DDCT <- genes %>% 
  mutate(DCT_C1=2^-(Cx1-promedio_controles$Mean_C1), #Lo que va despues del "$" es el nombre de la columna"
         DCT_C2=2^-(Cx2-promedio_controles$Mean_C2),
         DCT_C3=2^-(Cx3-promedio_controles$Mean_C3),
         DCT_T1=2^-(T1-promedio_controles$Mean_T1),
         DCT_T2=2^-(T2-promedio_controles$Mean_T2),
         DCT_T3=2^-(T3-promedio_controles$Mean_T3)) %>% 
  select(-2,-3,-4,-5,-6,-7) ###quitar las primeras columnas y sólo dejar los DCT

DDCT

promedio_genes<-DCT %>% 
  mutate(Mean_DCT_Cx=(DCT_C1+DCT_C2+DCT_C3)/3,
         Mean_DCT_Tx=(DCT_T1+DCT_T2+DCT_T3)/3) %>% 
  select(1,8,9)


promedio_genes #Resultado final del valor de 2^-DDCT
##################################
