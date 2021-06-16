########################################
# include all the needed packages here #
options(stringsAsFactors = F)

packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

## Packages for geospatial data handling
packages(raster)
packages(rgeos)
packages(rgdal)
packages(Formula)
packages(gdalUtils)
packages(randomForest)

## Packages for data table handling
packages(xtable)
packages(DT)
packages(dismo)
packages(stringr)
packages(plyr)
packages(data.table)

## Packages for graphics and interactive maps
packages(ggplot2)
packages(leaflet)
packages(leaflet.extras)
packages(RColorBrewer)

#### HOME DIRECTORY
homedir <- paste0(normalizePath("~"),"/")

#### POINTS DE LA BDD 2016-2018
df      <- read.csv(paste0(homedir,"rdc_extract_st/bdd_sae_conv_2016_2018_completed_v20210615.csv"))

#### SPATIALISATION
spdf    <- SpatialPointsDataFrame(df[,c("location_x","location_y")],
                                  df,
                                  proj4string = CRS("+init=epsg:4326"))
#plot(spdf)

#### LISTE DES BATCHES TELECHARGÉS
list_batches <- dir(paste0(homedir,"downloads/"),pattern= glob2rx("cod_*"))
print(list_batches)

#### BOUCLE PAR BATCH
for(batch in list_batches[1]){
  print(batch)
  batch_path <- paste0(homedir,"downloads/",batch,"/")
  
  #### LISTE DES TUILES DANS LE BATCH
  list_tiles <- list.dirs(batch_path,recursive = F,full.names = F)
  
  #### BOUCLE PAR TUILE
  for(tile_index in list_tiles){
    
    #### CHEMIN DE LA TUILE
    tile             <- paste0(batch_path,tile_index,"/")
    print(tile)
    
    #### NOM DE SORTIE
    tile_output_name <- paste0(homedir,"rdc_extract_st/",batch,"_tile",tile_index,"_ts.csv")
    
    if(!file.exists(tile_output_name)){
      #### IMAGES ET DATES DE LA TUILE
      vrt   <- brick(paste0(tile,"stack.vrt"))
      dates <- readLines(paste0(tile,"dates.csv"))
      
      #### EMPRISE DE LA TUILE
      e<-extent(vrt)
      
      poly <- Polygons(list(Polygon(cbind(
        c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
        c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
      )),1)
      
      lp <- list(poly)
      
      rprint <-SpatialPolygonsDataFrame(
        SpatialPolygons(lp,1:length(lp)), 
        data.frame(matrix(data=1,nrow = 1,ncol = 1)), 
        match.ID = F
      )
      
      crs(rprint) <- crs(spdf)
      
      #### SELECTION DES POINTS SUR L'EMPRISE
      pp <- spdf[rprint,] 
      #plot(pp)
      #plot(rprint,add=T)
      
      #### INITIALISATION DU FICHIER DE SORTIE (DATES EN PREMIERE COLONNE)
      ts <- data.frame(matrix(data=dates,ncol=1,nrow=length(dates)))
      names(ts) <- "dates"
      
      #### BOUCLE D'EXTRACTION DES IMAGES PAR POINT
      for(pts_index in 1:nrow(pp)){
        print(pts_index)
        pts          <- pp[pts_index,]
        plot_id      <- paste0("pid",pts@data$PLOTID)
        tmp          <- raster::extract(vrt,pts)
        ts[,plot_id] <- t(tmp)
        #plot(ts[,paste0("pid",pts@data$PLOTID)])
      }
      
      write.csv(ts,tile_output_name,row.names = F)
    }
  }
}
