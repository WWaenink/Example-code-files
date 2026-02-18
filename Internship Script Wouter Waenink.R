#This is my main script form my internship where I investigated the effect of artificial light at night on stone and pine marten. In the end I settled on a resource selection function to see if they had a bias for locations with more or less light, taking into account whether there is cover around.
#unfortunately I cannot provide the data that is meant to accompany this script since it's owned by a small consultancy agency and I am not allowed to share it.
#this script might seem a little weird in places - that's because it's a cleaned up version of a script where I was exploring the data at first. As such it may load more packages than it actually needs, and though I think I have simplified it as much as possible it may still do some things that are a little unecessary in places where it was meant to be linked to a block of code that was deleted.


#functions MASS: dplyr functions not working because of MASS dominance, add "dplyr::[function]
#install pacman
if (!require("pacman")){
  install.packages("pacman")
}

#load required packages
p_load(tidyverse, terra, lubridate, NISTunits, lme4, nlme, tidymodels, randomForest, vcd, MASS, fitdistrplus, logspline, goft, devtools, move, moveVis, ctmm, devtools, manipulate, adehabitatHR, ctmm, sf, mvtnorm, amt, DescTools, effects)
devtools::install_github("ctmm-initiative/ctmm")
#set working directory
setwd("d:/datasets/20221100 marter data Jaap Mulder/Data Wouter Waenink/R/Light and tracker data")
# d:/datasets/20221100 marter data Jaap Mulder/Data Wouter Waenink/R/Light and tracker data

##loading data
#get location paths of data
locations = paste("Data", list.files(path = "Data", pattern="*.csv"), sep = "/")
#get names for files
names <- locations %>% 
  str_replace(".*/", "") %>% 
  str_replace(".csv", "") %>% 
  str_replace("Raw_", "")
#load files into list and name list
dataList = lapply(locations, read_csv)
names(dataList) <- names

#filter out missed fixes
for(i in 1:length(dataList)){
  print(i)
  dataList[[i]] <- dataList[[i]] %>% 
    filter(!is.na(OL),!is.na(NB), OL != 0, NB != 0, !is.na(datum), !is.na("sat-tijd"), !is.na(x), !is.na(y)) %>% 
    dplyr::select(individu,nr,x,y,"sat-tijd", datum)
}

#convert dates into lubridate dates (needs to be done differently for different datasets because the date formats are different)
for (i in c(1:15,22,25:32)){
  print(i)
  dataList[[i]] <- dataList[[i]] %>% 
    mutate(datum = coalesce(dmy(datum), as.Date("1970-01-01")))
}  
for (i in c(16:21,23,24)){
  print(i)
  dataList[[i]] <- dataList[[i]] %>% 
    mutate(datum = coalesce(mdy(datum), as.Date("1970-01-01")))
}  

#convert date and time into lubridate date and time
for(i in 1:length(dataList)){
  message(paste("working on dataset", i, "out of", length(dataList), sep = " "))
  tempDateTime <- pull(dataList[[i]], datum)
  tempTime <- pull(dataList[[i]], "sat-tijd")  
  #message(tempDateTime, tempTime)
  hour(tempDateTime) <- hour(tempTime)
  minute(tempDateTime) <- minute(tempTime)
  second(tempDateTime) <- second(tempTime)
  dataList[[i]][,"datetime"] <- tempDateTime
}

#filter out anything introduced before 2000(unreadable or missing dates are just before converted to <1970)
for(i in 1:length(dataList)){
  message(paste("working on dataset", i, "out of", length(dataList), sep = " "))
  dataList[[i]] <- dataList[[i]] %>% 
    filter(datetime > dmy_hms("01-01-2000 00:00:00"))
}
#rename sim in the Sallandse Heuvelrug to simsal, as another sim already exists in the Bilt, also split up vlek before and after territory switch
for(i in 1:length(dataList)){
  dataList[[i]] <- dataList[[i]] %>% 
    mutate(individu = ifelse(individu == "Sim" & datum > dmy("01-01-2020"),"Simsal", individu), individu = ifelse(individu == "Vlek" & datetime > dmy_hms("18-01-2022 00:00:00"), "vlekNieuw", individu))
}

#this has to be redone because dataset 9 has invalid inputs for ∆t so the loop can't run fully while also keeping the precalculated delta t
#this is a correct delta t calculation by Wouter
deltat <- dataList
deltatcombined <- tibble("x" = numeric())
for(i in c(1:length(deltat))){
  print(paste("Working on dataset", i, "out of", length(dataList)))
  deltat[[i]] <- deltat[[i]] %>% 
    dplyr::select(datetime, individu, nr, x, y) %>% 
    filter(!is.na(datetime), !is.na(x), !is.na(y)) %>% 
    mutate(deltatrecalc = as.numeric(datetime - as.numeric(lag(datetime)))/60) %>% 
    filter(deltatrecalc < 1440, deltatrecalc > 0) 
  deltatcombined <- full_join(deltatcombined, deltat[[i]])
}

#get everything checked and time intervals set as close to 10 minutes as possible - to sync all intervals
for(i in 1:length(deltat)){
  message(paste("\n","Working on dataset", i, "out of", length(deltat)), appendLF = T)
  sum = 0
  oldsum = 0
  for(j in 1:(nrow(deltat[[i]])-1)){
    message('\r',paste(j,"/",nrow(deltat[[i]])-1, sep = ""),appendLF = F)
    sum = pull(deltat[[i]][j,"deltatrecalc"]) + sum
    nextsum = pull(deltat[[i]][j+1,"deltatrecalc"]) + sum
    #message('\n', paste(sum, oldsum))
    check <- which(abs(c(sum,nextsum)-10)==min(abs(c(sum,nextsum)-10)))
    if(sum > 20){
      deltat[[i]][j,"keep"] <- T
      sum = 0
    }else if(length(check) > 1){
      deltat[[i]][j,"keep"] <- T
      sum = 0
    }else if(check == 1){
      deltat[[i]][j,"keep"] <- T
      sum = 0
    }else{
      deltat[[i]][j,"keep"] <- F
    }
  }
}
#once again recalculate delta t with new timesteps (it's practically the same code as two blocks above) - delta t used for speed calculation
deltatcombined <- tibble()
for(i in c(1:length(deltat))){
  print(paste("Working on dataset", i, "out of", length(dataList)))
  deltat[[i]] <- deltat[[i]] %>% 
    dplyr::select(datetime, individu, nr, x, y, keep) %>% 
    filter(!is.na(datetime), !is.na(x), !is.na(y), keep == T) %>% 
    mutate(deltatrecalc = as.numeric(datetime - as.numeric(lag(datetime)))/60) %>% 
    filter(deltatrecalc < 1440, deltatrecalc > 0) %>% 
    dplyr::select(-keep)
  deltatcombined <- bind_rows(deltatcombined, deltat[[i]])
}

#calculating a few parameters (distance, angle & speed)
#not used but exported below
for (i in c(1:length(deltat))){
  print(paste("Working on dataset", i, "out of", length(deltat)))
  deltat[[i]] <- deltat[[i]] %>% 
    mutate(deltatrecalc = as.numeric(datetime - as.numeric(lag(datetime)))/60, distance = sqrt((x-lag(x))^2+(y-lag(y))^2), angle = NISTradianTOdeg(atan2(lag(x)-x,lag(y)-y))%%360, speed = distance/deltatrecalc) %>% 
    filter(deltatrecalc < 240, deltatrecalc > 0) 
}


#visualise distances
#eventually not used
deltatcopy <- deltat
distancetibble <- tibble()
for(i in c(1:length(deltatcopy))){
  print(paste("Working on dataset", i, "out of", length(deltat)))
  deltatcopy[[i]] <- deltatcopy[[i]] %>% 
    dplyr::select(nr, datetime, individu, distance, deltatrecalc, angle, speed, x, y) %>% 
    filter(!is.na(datetime), !is.na(distance), !is.na(individu))
  distancetibble <- bind_rows(distancetibble, deltatcopy[[i]])
}

#turning tibble into movestack - x and y from above - movestack is used by move package for GPS tracking data
#routine below removes double locations within one individual 
projection <- crs("+init=EPSG:28992")
martendata <- distancetibble %>%
  group_by(individu) %>% 
  dplyr::arrange(individu, datetime) %>% 
  ungroup() %>% 
  mutate(tempID = paste(nr, datetime, individu)) %>% 
  group_by(tempID) %>% 
  filter(!n()>1) %>% 
  ungroup() %>% 
  select(-tempID)
movingMartens <- move(x = martendata$x, y = martendata$y, time = martendata$datetime ,data = as.data.frame(martendata), proj = projection, animal = martendata$individu)


###working on cover and light raster###
vegMap <- raster("GIS Files/Raster/ahn3_10m_mean_normalized_height.tif")
vegMap <- crop(vegMap, extent(movingMartens)*1.1)
vegMap[is.na(vegMap[])] <- 0
rivm <- raster("GIS files/Raster/rivm_20220101_gm_lichtemissie2020.tif")

###rsf###
#creating telemetry object as ctmm requires it - conitnuous time move modelling
martenTelemetry <- as.telemetry(movingMartens)

#generating AKDE homeranges (cores can be adjusted to -1 to use all but 1 core, but if it is run on the HPC this might hog all the cores, so make sure to change  it back to something reasonable when running it on there)
#not used for random points to compare, but used to find the nest location
#warning: this takes forerver, see next step!
AKDEhomeranges <- list()
for(i in 1:length(martenTelemetry)){
  message("\r", paste("### working on dataset", i , "out of", length(martenTelemetry),"###"))
  marten_guess <- ctmm.guess(martenTelemetry[[i]], CTMM = ctmm(isotropic = T), interactive = F)
  marten_select <- ctmm.select(martenTelemetry[[i]], marten_guess, verbose = T, cores = 4)
  AKDEhomeranges[[i]] <- akde(martenTelemetry[[i]], marten_select)
}

# alternative: load pre-made home range data directly (3GB of data!):
load("manualrsfs.RData")

#calculating distance from nest based on AKDE homeranges
distancefromcenter <- list()
for(i in 1:length(AKDEhomeranges)){
  temparray <- which(AKDEhomeranges[[i]]$PDF == max(AKDEhomeranges[[i]]$PDF), arr.ind = T)
  modex <- AKDEhomeranges[[i]]$r$x[temparray[1]]
  modey <- AKDEhomeranges[[i]]$r$y[temparray[2]]
  mode <- SpatialPoints(matrix(c(modex,modey), nrow= 1),CRS("+init=EPSG:28992"))
  temprast <- crop(vegMap, extent(movingMartens[[i]])*6)
  values(temprast) <- 0
  temprast <- distanceFromPoints(temprast, mode)
  distancefromcenter[[i]] <- temprast
}


#creating function for classifying cover
#arbitrary - 30 cm is the height of a mustelid ()
coverclassification <- function(x){
  return(ifelse(x > 0.3, 1,0))
}

#classify cover
#coverlist is list with maps with and without cover, using the function just defined
coverlist = list()
for(i in 1:length(martenTelemetry)){
  coverlist[[i]] <- calc(crop(vegMap, extent(movingMartens[[i]])*1.5), fun = coverclassification)
}


###rsf with amt### amt = animal movement tracking/tools
#creating tracks; arange puts invividuals in correct order
deltatcombined <- deltatcombined %>% 
  arrange(individu)

martendatalist <- list()
for(i in 1:length(unique(deltatcombined$individu))){
  martendatalist[[i]] <- deltatcombined %>% 
    filter(individu == unique(deltatcombined$individu)[i])
}
names(martendatalist) <- unique(deltatcombined$individu)

#removing a couple of points on the wrong side of vlek's habitat switch - Vlek is the reallocated marten (the one tha swam the IJssel)
martendatalist[[31]] <- martendatalist[[31]] %>% 
  filter(datetime < dmy_hms("17-01-2022 00:00:00"))

#generating random points (MCP homerange calculation is included in random_points() function)
pointlist <- list()
for(i in 1:length(martendatalist)){
 temp <- track(martendatalist[[i]],x = martendatalist[[i]]$x, y = martendatalist[[i]]$y, t = martendatalist[[i]]$datetime, crs = "epsg:28992")
 pointlist[[i]] <- random_points(temp)
}

#add ID to pointlist
for(i in c(1:length(pointlist))){
  print(paste("Working on dataset", i, "out of", length(martendatalist)))
  pointlist[[i]][,"ID"] <- martendatalist[[i]][1,"individu"]
  pointlist[[i]][,"centerdistance"] <- raster::extract(distancefromcenter[[i]], SpatialPoints(coords = cbind(pointlist[[i]]$x_, pointlist[[i]]$y_), proj4string = CRS(projection)))
}

#combine pointlist into points
points <- bind_rows(pointlist)

#plot all the points to see if nothing weird happened
plot(points)

#prepare data for RSF
#light and vegetation data in to rsfdata 
#if the next command casuse a stack usage error, reload light map (next line)
rivm <- raster("GIS files/Raster/rivm_20220101_gm_lichtemissie2020.tif")
rsfdata <- points |> extract_covariates(rivm) |> extract_covariates(vegMap)
rsfdataclassified <- points |> extract_covariates(rivm) |> extract_covariates(coverMap)

#summarise data, rename layer to cover to make it clearer
summary(rsfdata)
summary(rsfdataclassified)

rsfdata <- rsfdata %>% 
  mutate(ID = as.factor(ID))
rsfdataclassified <- rsfdataclassified %>% 
  mutate(ID = as.factor(ID), cover = as.factor(layer)) %>% 
  select(-layer)

#fitting base rsf => null model
rsfbase <- glmer(case_ ~ 1 + (1|ID), data = rsfdata, family = binomial)

#fitting rsfs with vegetation height
rsf <- glmer(case_ ~ scale(rivmlicht)*scale(ahn3_10m_mean_normalized_height) + scale(centerdistance) + (1|ID), data = rsfdata, family = binomial)
rsfsimple <- glmer(case_ ~ scale(rivmlicht)+scale(ahn3_10m_mean_normalized_height) + scale(centerdistance) + (1|ID), data = rsfdata, family = binomial)
rsfnodist <- glmer(case_ ~ scale(rivmlicht)*scale(ahn3_10m_mean_normalized_height) + (1|ID), data = rsfdata, family = binomial)

#fitting rsfs with cover
rsfclassified <- glmer(case_ ~ scale(rivmlicht)*cover + scale(centerdistance) + (1|ID), data = rsfdataclassified, family = binomial)
rsfclassifiedsimple <- glmer(case_ ~ scale(rivmlicht) + cover + scale(centerdistance) + (1|ID), data = rsfdataclassified, family = binomial)
rsfclassifiednodist <- glmer(case_ ~ scale(rivmlicht) * cover + (1|ID), data = rsfdataclassified, family = binomial)

summary(rsfclassified) # highly significant - if compared with random points, significance disappears

#add model comparsions 

#generating some random data to make sure that the model doesn't just always give significant results
rsfdatabullshit <- rsfdata %>% 
  mutate(ID = as.factor(ID), rivmlicht = runif(nrow(rsfdata),0,100), ahn3_10m_mean_normalized_height = runif(nrow(rsfdata),0,100))

#fitting rsf with random data
rsfbs <- glmer(case_ ~ scale(rivmlicht)*scale(ahn3_10m_mean_normalized_height) + (1|ID), data = rsfdatabullshit, family = binomial)

#scale variables to get mean and SD of scaled variables
scale(rsfdata$rivmlicht)
scale(rsfdata$ahn3_10m_mean_normalized_height)



#creating shapefiles from AKDE homerange polygons, will end up being in the wrong location so not usable
homerangepolygons <- list()
for(i in 1:length(AKDEhomeranges)){
  shapefile_polygons <- as.sf(AKDEhomeranges[[i]], level.UD=0.95, level=0.95)
  middle_polygon <- shapefile_polygons[2,]
  z <- crs(movingMartens)
  homerangepolygons[[i]] <- st_transform(middle_polygon, crs = z) 
}

#creating plot of results
plot(Effect(focal.predictors = c("cover","rivmlicht"), mod = rsfclassified))
#checking base values
head(test$`scale(rivmlicht):layer`$model.matrix)

