#Packages
#https://jcheshire.com/resources/different-maps-same-data/
library(tmap)
library(rgdal)
library(sp)
library(spatialEco)

# load in file

input<- readOGR(dsn="http://www2.geog.ucl.ac.uk/~ucfa012/COVID_Oct30_Rate.GeoJSON", layer="COVID_Oct30_Rate")

# Remove the Nas (this is Scotland and Wales and a function from the SpatialEco package)
input<- sp.na.omit(input, col.name="LA_Cln")

#Government choice - extra colours for low values           
opt1<-tm_shape(input) + 
  tm_fill("Rate_num", palette = "Reds", legend.hist = T, breaks=c(0,25,50,100,150,200,736.2), title="COVID-19 Cases\nPer 100k (Weekly)") +
  tm_borders(col="black", lwd=0.1)


#Suggestion 2 - same colour breaks up until 200+ plus cases          
opt2<-tm_shape(input) + 
  tm_fill("Rate_num", palette = "Reds", legend.hist = T, breaks=c(0,50,100,150,200,736.2),title="COVID-19 Cases\nPer 100k (Weekly)") +
  tm_borders(col="black", lwd=0.1)

#Suggestion 3 - consistent breaks up to 500+ cases          
opt3<-tm_shape(input) + 
  tm_fill("Rate_num", palette = "Reds", legend.hist = T, breaks=c(0,100,200,300, 400, 500,736.2),title="COVID-19 Cases\nPer 100k (Weekly)") +
  tm_borders(col="black", lwd=0.1)

#Suggestion 4 - natural breaks in the distribution using the Jenks algorithm         
opt4<-tm_shape(input) + 
  tm_fill("Rate_num", palette = "Reds", legend.hist = T, style="jenks",title="COVID-19 Cases\nPer 100k (Weekly)") +
  tm_borders(col="black", lwd=0.1)

#Suggestion 5 - equal numbers of local authorities in each colour band         
opt5<-tm_shape(input) + 
  tm_fill("Rate_num", palette = "Reds", legend.hist = T, style="quantile",title="COVID-19 Cases\nPer 100k (Weekly)") +
  tm_borders(col="black", lwd=0.1)

#Suggestion 6 - extra colours for higher values          
opt6<-tm_shape(input) + 
  tm_fill("Rate_num", palette = "Reds", legend.hist = T, breaks=c(100,200,300,400,500,550,600,650,700,736.2),title="COVID-19 Cases\nPer 100k (Weekly)") +
  tm_borders(col="black", lwd=0.1)

#uncomment below if you want to save as PDF. I have only selected 4 of the above options.
#pdf("map.pdf",width=20, height=5)
tmap_arrange(opt1,opt2,opt6,opt4, ncol=2)
#dev.off()

#Voila!