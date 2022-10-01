
library(sp)
library(raster)
library(rgdal)
library(sf)

# Zona de Interes
Zona = st_read("SHP/Puerto.geojson")
Zona_py <- st_transform(Zona ,
                        crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Via_Mal <- st_read ("SHP/Red_vial.geojson") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
CP <- st_read ("SHP/CP.geojson") # Caragmos un shp de puerto maldonado
CetroPo  <- st_transform(CP ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
CetroPo_xy <- cbind(CetroPo, st_coordinates(st_centroid(CetroPo$geometry)))

NDCI <- raster("Raster/NDCI_Puerto.tif")
NDCI_py= projectRaster(NDCI, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(NDCI_py)

NDCI_py.pa        <-  rasterToPoints(NDCI_py)
NDCI_py.pa_a      <-  data.frame(NDCI_py.pa)
colnames(NDCI_py.pa_a) <- c("x","y", "Clorofila")

colores<- c('#d8e2dc', '#8ecae6', '#023e8a', '#03045e', '#184e77', '#40916c', '#80b918',
            '#55a630','#aacc00','#d4d700','#eeef20','#ffff3f','#ff9e00','#ff9100','#ff6d00','#e36414'
            ,'#9a031e')

NDCI_py.pa_a %>% subset(Clorofila <= 0.4 & Clorofila> -0.11052) ->NDCI_Puerto

library(ggplot2)
library(ggspatial)


library(scales)
library(tidyr)
library(dplyr)
library(ggthemes)
show_col(viridis_pal(option = "inferno")(9))
bivariate_color_scale <- tibble(
  "3 - 3" = "#184e77", # 3
  "2 - 3" = "#8ecae6", # 2
  "1 - 3" = "#d8e2dc", # 1
  "3 - 2" = "#023e8a", # 6
  "2 - 2" = "#03045e", # 5
  "1 - 2" = "#40916c", # 4
  "3 - 1" = "#d4d700", # 7
  "2 - 1" = "#aacc00", # 8
  "1 - 1" = "#55a630"  # 9
) %>%
  gather("group", "fill")

bivariate_color_scale %<>%
  separate(group, into = c("gini", "mean"), sep = " - ") %>%
  mutate(gini = as.integer(gini),
         mean = as.integer(mean))

legend <-ggplot() +
  geom_tile(data = bivariate_color_scale, mapping = aes(
    x = gini, y = mean, fill = fill)) +
  scale_fill_identity() +
  labs(x = "Mayor NDCI ⟶️",
       y = "Menor NDCI ⟶️") +
  theme_map() +
  theme( axis.title = element_text(size = 6),
         axis.title.x=element_text(color="white"),
         axis.title.y=element_text(color="white")) +
  coord_fixed()

legend 

legend.grob <- ggplotGrob(legend)


  
Mapa =ggplot()+
  geom_raster(data = NDCI_Puerto , aes(x,y,fill =Clorofila)) + 
  scale_fill_gradientn(colours = colores, name="NDCI",
                       breaks = c(0,0.1,0.2,0.3,0.4))+
  geom_sf(data = Zona_py, fill=NA, color="black", size=0.5)+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf_label(data = CetroPo_xy , aes(label = NOMBCP ), 
                family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
                size = 2, face = "bold",color = 'black',
                point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c(-69.26, -69.13), ylim = c(-12.68  , -12.48)) +
  theme_classic()+
  theme(legend.position = c(0.9,0.1),
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, family="serif", face = "italic"),
        
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=11, family="serif"),
        legend.title = element_text(size=11, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')+
  geom_vline(xintercept = c(-69.26,-69.24,-69.22,-69.20,-69.18,-69.16,-69.14,-69.12), color = "gray50",linetype = "dashed", size = 0.5)+
  geom_hline(yintercept = c(-12.65, -12.60,-12.55,-12.50), color = "gray50",linetype = "dashed", size = 0.5)+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -69.26, y = -12.68, hjust = 0, vjust = 1, 
           label = "18-09-2022",size = 6, family="serif", color = 
             "white", face = "bold")+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  annotation_custom(grob= legend.grob, xmin = -69.26, xmax = -69.24, ymin =-12.55, ymax=-12.48)+
  labs(color = '',  x = 'Longitud', y = 'Latitud',
       title = "Mapa de Índice de clorofila de diferencia normalizada",
       subtitle = "la ciudad de Puerto Maldonado",
       caption = "Datos en https://earthexplorer.usgs.gov \n@Ing. Gorky Florez")
  

ggsave(plot=Mapa ,"Mapa de clorofila_Puerto Maldonado.png",units = "cm",width = 21, #alto
       height = 29, #ancho
       dpi=1200)




























































