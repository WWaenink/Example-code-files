#this is the main script form my thesis, where I looked at spatial and temporal avoidance between American mink and Eurasian otter, red fox and pine marten on holmöarna in västerbotten (Sweden)
#unfortunately I am unable to share the accompanying data, since it is the property of Svenska Jägareförbundet.

####set up workspace and packages####
if (!require("pacman")){
  install.packages("pacman")
}
p_load(tidyverse, camtraptor, unmarked, car, hms, activity, overlap)
setwd("C:/R Projects/MSc Thesis")

####reading in camera deployment data####
#create function to replace currently active cameras with date mbox file was downloaded (30-10-2023)
replacer <- function(x){
  outputvector <- vector(length = length(x))
  for(i in 1:length(x)){
    if(is.na(x[i])){
      outputvector[i] = NA
    }else if(x[i] == "x"){
      outputvector[i] = "31-10-2023" 
    }else{
      outputvector[i] = x[i]
    }  
  }
  return(outputvector)
}

#create dataframe with start and end date
camsncoords <- read_csv("Output/Cams 'n Coords.csv") %>% 
  #select(name, startDate, endDate, altStart, altEnd, Trap) %>%
  arrange(name) %>% 
  #replace x with correct end date
  mutate(endDate = replacer(endDate)) %>%
  mutate(altEnd = replacer(altEnd)) %>% 
  #put date into correct format
  mutate(startDate = dmy(startDate, tz = "CET"), endDate = dmy(endDate, tz = "CET"), altStart = dmy(altStart, tz = "CET"), altEnd = dmy(altEnd, tz = "CET")) %>% 
  filter(!(name %in% c("1-310","1-311","1-312", "ao31","ao32","ho16","ho24","gg15","skitgrundsundet")))

####create empty detection dataframe for occupancy model####
#create function to determine week/year number of a date (keep in mind the 31st of december gets week number 53 so it gets lumped in with the first week of the next year, but that makes just as much sense as lumping it in with the last week of last year)
yearweek <- function(minDate, curDate){
  #get days in period
  days <- slot(as.period(interval(minDate, curDate), unit = "day"), "day")
  #get week number since start
  weeks = ceiling(days/7)
  weeks = ifelse(weeks == 0, 1, weeks)
  return(weeks)
}

#create empty detection dataframe of correct dimensions
maxweeks <- yearweek(minDate = min(camsncoords$startDate), curDate = max(c(camsncoords$endDate, camsncoords$altEnd), na.rm = T))
detectionsEmpty <- matrix(data = NA, nrow = nrow(camsncoords), ncol = maxweeks)
namesInOrder <- camsncoords$name
colnames(detectionsEmpty) <- 1:maxweeks
rownames(detectionsEmpty) <- camsncoords$name
detectionsEmpty <- as_tibble(detectionsEmpty)


for(i in 1:nrow(detectionsEmpty)){
  ##run through matrix and add 0 where end and start date are
  ##intended dates but dates are wrong
  detectionsEmpty[i, yearweek(minDate = min(camsncoords$startDate), curDate = camsncoords$startDate[i])] <- 0
  detectionsEmpty[i, yearweek(minDate = min(camsncoords$startDate), curDate = camsncoords$endDate[i])] <- 0
  if(!is.na(camsncoords$altStart[i])){
    detectionsEmpty[i, yearweek(minDate = min(camsncoords$startDate), curDate = camsncoords$altStart[i])] <- 0
    detectionsEmpty[i, yearweek(minDate = min(camsncoords$startDate), curDate = camsncoords$altEnd[i])] <- 0
  }
  #fill space inbetween with 0s
  #this checks for up to two periods, as some sites have had two separate camera deployments.
  boundaries <- which(detectionsEmpty[i,] == 0)
  if(length(boundaries) == 2){
    indices <- boundaries[1]:boundaries[2]
    for(j in indices){
      detectionsEmpty[i,j] <- 0
    }
  }else if(length(boundaries) == 4){
    indices1 <- boundaries[1]:boundaries[2]
    indices2 <- boundaries[3]:boundaries[4]
    for(j in indices1){
      detectionsEmpty[i,j] <- 0
    }
    for(j in indices2){
      detectionsEmpty[i,j] <- 0
    }
  }else{
    message("Wrong number of period boundaries when trying to fill detection matrix")
  }
}

####read in camera observation data####
#read in camera data package exported from agouti
camdata <- read_camtrap_dp("Raw Data/holmondata/datapackage.json")
#isolate observations
obsRaw <- camdata$data$observations %>% 
  select(scientificName, timestamp, deploymentID, observationType) %>% 
  distinct()
deployKey <- camdata$data$deployments %>% 
  select(deploymentID, locationName)
#create start and end data object
startEnd <- camsncoords %>% 
  select(name, startDate, endDate) %>% 
  rename(locationName = name)
obs <- obsRaw %>% 
  left_join(deployKey, by = "deploymentID") %>% 
  left_join(startEnd, by = "locationName")
#setting variable to make sure that the adjusted offset later does not run twice without reloading obs
offsetRerunPreventer = T
#solve issues in dataset
#try to get 1228 picutres out of fendero since they weren't meant to be in there
filecheck <- list.files("C:/Holmon photos/Fendero")
filecheck <- filecheck[str_detect(filecheck, "1228")]
timevector <- .POSIXct(character(length(filecheck)))
for(i in 1:length(filecheck)){
  timevector[i] <- file.info(paste("C:/Holmon photos/Fendero/", filecheck[i], sep = ""))$mtime
}
obs <- obs %>% 
  filter(!(timestamp %in% timevector & locationName == "Fendero"))
#removing 1-235 pictures as well
filecheck <- list.files("C:/Holmon photos/1-235")
filecheck <- filecheck[str_detect(filecheck, "1-235")]
timevector <- .POSIXct(character(length(filecheck)))
for(i in 1:length(filecheck)){
  timevector[i] <- file.info(paste("C:/Holmon photos/1-235/", filecheck[i], sep = ""))$mtime
}
obs <- obs %>% 
  filter(!(timestamp %in% timevector & locationName == "1-235"))
#removing 1-240 pictures
filecheck <- list.files("C:/Holmon photos/1-240")
filecheck <- filecheck[str_detect(filecheck, "1-240")]
timevector <- .POSIXct(character(length(filecheck)))
for(i in 1:length(filecheck)){
  timevector[i] <- file.info(paste("C:/Holmon photos/1-240/", filecheck[i], sep = ""))$mtime
}
obs <- obs %>% 
  filter(!(timestamp %in% timevector & locationName == "1-240"))
#changing pictures from before may 22 to Malgrund 1 (this means all pictures from 1-240, Peter confirmed this is correct)
obs <- obs %>% 
  mutate(locationName = ifelse((timestamp < dmy("20-05-2022") & locationName == "hg1"), "malgrund1 fdHG1", locationName))
#filtering wrong pictures for HG2
obs <- obs %>% 
  filter(!(timestamp < dmy("19-05-2022") & locationName == "hg2"))
#filtering 1250B
obs <- obs %>% 
  filter(!(timestamp < dmy("01-11-2021") & locationName == "1250B"))
#filtering 1-250
obs <- obs %>% 
  filter(!(timestamp < dmy("17-09-2021") & locationName == "1-250"))
#filtering out HG3 and gaddback fiske pictures not meant to be included (also filtering out remaining ones I have no info on)
obs <- obs %>% 
  filter(!((timestamp < startDate | timestamp > endDate))) #& (locationName == "hg3" | locationName == "gaddback fiske")))
#filtering cameras too far away
obs <- obs %>% 
  filter(!(locationName %in% c("1-310","1-311","1-312", "ao31","ao32","ho16","ho24","gg15","skitgrundsundet")))

#somewhere something (probably timezone related) introduced a 2 hour offset in all the timestamps where the actual timestamp is 2 hours beyond the real timestamp. Fixing here. OffsetRerunPreventer prevents it from doing this again should it accidentally be ran again.
if(offsetRerunPreventer){
  obs$timestamp <- obs$timestamp+hours(2)
  offsetRerunPreventer = F
}

#split observations into dataframes with observations for every species of interest separately
minkObs <- obs %>% 
  filter(scientificName == "Neovison vison")
martObs <- obs %>% 
  filter(scientificName == "Martes martes")
ottObs <- obs %>% 
  filter(scientificName == "Lutra lutra")
foxObs <- obs %>% 
  filter(scientificName == "Vulpes vulpes")

####creating detection matrices for occupancy model####
#create observation matrices for each species
minkMatr <- detectionsEmpty
for(i in 1:nrow(minkObs)){
  row = which(namesInOrder == minkObs$locationName[i])
  col = yearweek(min(camsncoords$startDate), minkObs$timestamp[i])
  minkMatr[row,col] <- 1
}
martMatr <- detectionsEmpty
for(i in 1:nrow(martObs)){
  row = which(namesInOrder == martObs$locationName[i])
  col = yearweek(min(camsncoords$startDate), martObs$timestamp[i])
  martMatr[row,col] <- 1
}
ottMatr <- detectionsEmpty
for(i in 1:nrow(ottObs)){
  row = which(namesInOrder == ottObs$locationName[i])
  col = yearweek(min(camsncoords$startDate), ottObs$timestamp[i])
  ottMatr[row,col] <- 1
}
foxMatr <- detectionsEmpty
for(i in 1:nrow(foxObs)){
  row = which(namesInOrder == foxObs$locationName[i])
  col = yearweek(min(camsncoords$startDate), foxObs$timestamp[i])
  foxMatr[row,col] <- 1
}

#this part is not very tidy but it's a lot faster than redoing everything above
#originally all sites were together, but since site-year combinations are treated as separate sites they should be included as separate rows with 52 columns for each week of the year
{
minkMatr2019 <- as.data.frame(cbind(matrix(nrow = nrow(minkMatr), ncol = 22), minkMatr[,1:30], deparse.level = 0))
colnames(minkMatr2019) <- 1:52
minkMatr2020 <- minkMatr[31:82]
colnames(minkMatr2020) <- 1:52
minkMatr2021 <- minkMatr[83:134]
colnames(minkMatr2021) <- 1:52
minkMatr2022 <- minkMatr[135:186]
colnames(minkMatr2022) <- 1:52
minkMatr2023 <- cbind(minkMatr[187:231], as.data.frame(matrix(nrow = nrow(minkMatr), ncol = 7)))
colnames(minkMatr2023) <- 1:52
minkMatrMulti <- rbind(minkMatr2019,minkMatr2020,minkMatr2021,minkMatr2022,minkMatr2023)
colnames(minkMatrMulti) <- 1:52
rm(minkMatr2019)
rm(minkMatr2020)
rm(minkMatr2021)
rm(minkMatr2022)
rm(minkMatr2023)

martMatr2019 <- as.data.frame(cbind(matrix(nrow = nrow(martMatr), ncol = 22), martMatr[,1:30], deparse.level = 0))
colnames(martMatr2019) <- 1:52
martMatr2020 <- martMatr[31:82]
colnames(martMatr2020) <- 1:52
martMatr2021 <- martMatr[83:134]
colnames(martMatr2021) <- 1:52
martMatr2022 <- martMatr[135:186]
colnames(martMatr2022) <- 1:52
martMatr2023 <- cbind(martMatr[187:231], as.data.frame(matrix(nrow = nrow(martMatr), ncol = 7)))
colnames(martMatr2023) <- 1:52
martMatrMulti <- rbind(martMatr2019,martMatr2020,martMatr2021,martMatr2022,martMatr2023)
colnames(martMatrMulti) <- 1:52
rm(martMatr2019)
rm(martMatr2020)
rm(martMatr2021)
rm(martMatr2022)
rm(martMatr2023)

ottMatr2019 <- as.data.frame(cbind(matrix(nrow = nrow(ottMatr), ncol = 22), ottMatr[,1:30], deparse.level = 0))
colnames(ottMatr2019) <- 1:52
ottMatr2020 <- ottMatr[31:82]
colnames(ottMatr2020) <- 1:52
ottMatr2021 <- ottMatr[83:134]
colnames(ottMatr2021) <- 1:52
ottMatr2022 <- ottMatr[135:186]
colnames(ottMatr2022) <- 1:52
ottMatr2023 <- cbind(ottMatr[187:231], as.data.frame(matrix(nrow = nrow(ottMatr), ncol = 7)))
colnames(ottMatr2023) <- 1:52
ottMatrMulti <- rbind(ottMatr2019,ottMatr2020,ottMatr2021,ottMatr2022,ottMatr2023)
colnames(ottMatrMulti) <- 1:52
rm(ottMatr2019)
rm(ottMatr2020)
rm(ottMatr2021)
rm(ottMatr2022)
rm(ottMatr2023)

foxMatr2019 <- as.data.frame(cbind(matrix(nrow = nrow(foxMatr), ncol = 22), foxMatr[,1:30], deparse.level = 0))
colnames(foxMatr2019) <- 1:52
foxMatr2020 <- foxMatr[31:82]
colnames(foxMatr2020) <- 1:52
foxMatr2021 <- foxMatr[83:134]
colnames(foxMatr2021) <- 1:52
foxMatr2022 <- foxMatr[135:186]
colnames(foxMatr2022) <- 1:52
foxMatr2023 <- cbind(foxMatr[187:231], as.data.frame(matrix(nrow = nrow(foxMatr), ncol = 7)))
colnames(foxMatr2023) <- 1:52
foxMatrMulti <- rbind(foxMatr2019,foxMatr2020,foxMatr2021,foxMatr2022,foxMatr2023)
colnames(foxMatrMulti) <- 1:52
rm(foxMatr2019)
rm(foxMatr2020)
rm(foxMatr2021)
rm(foxMatr2022)
rm(foxMatr2023)
}

####setting up data for occupancy model####
#site level covariates 
#trap or not
cov_trap <- c(camsncoords$Trap,camsncoords$Trap,camsncoords$Trap,camsncoords$Trap,camsncoords$Trap)

#create names per year
nameYearCombos <- crossing(namesInOrder,2019:2023) %>% 
  rename(locationName = `namesInOrder`, year = `2019:2023`) %>% 
  arrange(locationName)
#number of observations per location
locCount <- obs %>% 
  mutate(year = year(timestamp)) %>% 
  group_by(locationName, year) %>% 
  summarise(n = n()) %>% 
  right_join(nameYearCombos, by = c("locationName","year")) %>% 
  mutate(n = ifelse(is.na(n),0,n)) %>% 
  mutate(siteyearID = paste(locationName, year, sep = "_")) %>% 
  filter(!is.na(year)) %>% 
  arrange(locationName,year)
#number of unidentifiables per location
nUnidentifiable <- obs %>% 
  filter(observationType == "unknown") %>% 
  mutate(year = year(timestamp)) %>% 
  group_by(locationName, year) %>% 
  summarise(nUnidentifiable = n()) %>% 
  right_join(nameYearCombos, by = c("locationName","year")) %>% 
  mutate(nUnidentifiable = ifelse(is.na(nUnidentifiable),0,nUnidentifiable)) %>% 
  mutate(siteyearID = paste(locationName, year, sep = "_"))%>% 
  filter(!is.na(year)) %>% 
  arrange(locationName,year)
#relative proportion of unkowns per location
relPropUnidentifiable <- merge(locCount, nUnidentifiable, by = "siteyearID", all = T) %>%
  select(-locationName.y, -year.y) %>% 
  rename(locationName = locationName.x, year = year.x) %>% 
  mutate(nUnidentifiable = ifelse(is.na(nUnidentifiable), 0, nUnidentifiable)) %>% 
  mutate(relProp = nUnidentifiable/n) %>% 
  arrange(siteyearID) %>%
  mutate(relProp = ifelse(is.na(relProp), 1, relProp)) %>% 
  arrange(year, locationName)

#must return true ONLY!!! NOT TRUE,FALSE otherwise the location names for the relative proportion of unidentifiable pictures are out of order.
{message("!!!!ENSURE THIS ONLY SAYS TRUE, NOT FALSE OR BOTH TRUE AND FALSE!!!!")
unique(unique(relPropUnidentifiable$locationName) == namesInOrder)}
#relPropUnidentifiable$relProp is now ready for use as covariate
cov_relProp <- relPropUnidentifiable$relProp
#subtract 2018 from all years to get 2019 as 1 and 2023 as 5. The model cannot handle the years being that high.
cov_time <- relPropUnidentifiable$year - 2018

#filter out cameras with only NAs
selectionVector <- vector()
for(i in 1:nrow(minkMatrMulti)){
  selectionVector[i] <- TRUE %in% !is.na(minkMatrMulti[i,])
}

#cutting input vectors and matrices
{
minkMatrMultiClean <- minkMatrMulti[selectionVector,]
martMatrMultiClean <- martMatrMulti[selectionVector,]
ottMatrMultiClean <- ottMatrMulti[selectionVector,]
foxMatrMultiClean <- foxMatrMulti[selectionVector,]
cov_relProp <- cov_relProp[selectionVector]
cov_time <- cov_time[selectionVector]
cov_trap <- cov_trap[selectionVector]
}

#fitting model
#putting data in right format
specList <- list(mink = as.matrix(minkMatrMultiClean),
                 otter  = as.matrix(ottMatrMultiClean),
                 pine_marten = as.matrix(martMatrMultiClean),
                 red_fox = as.matrix(foxMatrMultiClean))

#creating site covariates
sitecovs <- as.data.frame(cbind(cov_relProp, cov_trap, cov_time)) %>% 
  rename(relPropUnidentifiable = `cov_relProp` ,trap = `cov_trap`, time = cov_time) %>% 
  mutate(relPropUnidentifiable = logit(relPropUnidentifiable,F), trap = as.logical(trap))


#creating unmarkedFrameOccuMulti object (required format for the data)
OMDat <- unmarkedFrameOccuMulti(specList, sitecovs, maxOrder = 2)

####fitting occupancy models####
OMFitBase <- occuMulti(detformulas = c('~1','~1','~1','~1'),
          stateformulas = c('~1','~1','~1','~1'), 
          data = OMDat,
          maxOrder = 1)

OMFitMultiBase <- occuMulti(detformulas = c('~ 1','~ 1','~ 1', '~ 1'),
                   stateformulas = c('~1','~1','~1','~1','~1','~1','~1','~1','~1','~1'), 
                   data = OMDat,
                   maxOrder = 2)

OMFitDetectTrap <- occuMulti(detformulas = c('~ 1 + trap','~ 1 + trap','~ 1 + trap', '~ 1 + trap'),
                         stateformulas = c('~1','~1','~1','~1','~1','~1','~1','~1','~1','~1'), 
                         data = OMDat,
                         maxOrder = 2)

OMFitDetectRelProp <- occuMulti(detformulas = c('~ 1 + relPropUnidentifiable','~ 1 + relPropUnidentifiable','~ 1 + relPropUnidentifiable', '~ 1 + relPropUnidentifiable'),
                             stateformulas = c('~1','~1','~1','~1','~1','~1','~1','~1','~1','~1'), 
                             data = OMDat,
                             maxOrder = 2)

OMFitDetect <- occuMulti(detformulas = c('~ trap + relPropUnidentifiable','~ trap + relPropUnidentifiable','~ trap + relPropUnidentifiable', '~ trap + relPropUnidentifiable'),
                                           stateformulas = c('~1','~1','~1','~1','~1','~1','~1','~1','~1','~1'), 
                                           data = OMDat,
                                           maxOrder = 2)

OMFitMinkTime <- occuMulti(detformulas = c('~ trap + relPropUnidentifiable','~ trap + relPropUnidentifiable','~ trap + relPropUnidentifiable', '~ trap + relPropUnidentifiable'),
                   stateformulas = c('~1 + time','~1','~1','~1','~1','~1','~1','~1','~1','~1'), 
                   data = OMDat,
                   maxOrder = 2)

OMFit <- occuMulti(detformulas = c('~ trap + relPropUnidentifiable','~ trap + relPropUnidentifiable','~ trap + relPropUnidentifiable', '~ trap + relPropUnidentifiable'),
                       stateformulas = c('~1 + time','~1 + time','~1 + time','~1 + time','~1','~1','~1','~1','~1','~1'), 
                       data = OMDat,
                       maxOrder = 2)

#listing AICs per model
OMFitBase@AIC
OMFitMultiBase@AIC
OMFitDetectTrap@AIC
OMFitDetectRelProp@AIC
OMFitDetect@AIC
OMFitMinkTime@AIC
OMFit@AIC

####plotting results####
#creating data for scenario without trap, over time, with and average proportion of unidentifiable pictures
predData <- data.frame(time = seq(min(sitecovs$time),max(sitecovs$time), length.out = 100),
                       trap = rep(FALSE, 100),
                       relPropUnidentifiable = rep(mean(sitecovs$relPropUnidentifiable), 100))

#creating plots (code adapted from Rota webinar https://www.youtube.com/watch?v=tj_OCO77_sc)
#otter
minkOttPres <- predict(OMFit, type = 'state', species = 'mink', cond = 'otter', newdata = predData)
minkOttAbs <- predict(OMFit, type = 'state', species = 'mink', cond = '-otter', newdata = predData)

#create dataframe for plotting
gg_df_cond <- data.frame(
  time = rep(predData$time, 2),
  occupancy = c(minkOttPres$Predicted,
                minkOttAbs$Predicted),
  low = c(minkOttPres$lower,
          minkOttAbs$lower),
  high = c(minkOttPres$upper,
           minkOttAbs$upper),
  conditional = rep(c('Otter present', 'Otter absent'),
                    each = 100)
)
#plot conditional mink occupancy for presence and absence of otter
minkOttPlot <- ggplot(gg_df_cond, aes(x = time, y = occupancy, group = conditional)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = conditional), alpha = 0.8) +
  geom_line(aes(linetype = conditional), linewidth = 1) +
  ylab('Conditional mink\noccupancy probability') +
  xlab('time (years)') +
  scale_fill_manual(name = 'Eurasian otter State', values = c("#003666", "#006638")) +
  scale_linetype_manual(name = 'Otter State', values = c("solid", "dashed")) +
  theme(text = element_text(size = 15), legend.position = c(0.75, 0.85)) +
  scale_x_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))+
  theme_bw()


#fox
minkFoxPres <- predict(OMFit, type = 'state', species = 'mink', cond = 'red_fox', newdata = predData)
minkFoxAbs <- predict(OMFit, type = 'state', species = 'mink', cond = '-red_fox', newdata = predData)

#create dataframe for plotting
gg_df_cond <- data.frame(
  time = rep(predData$time, 2),
  occupancy = c(minkFoxPres$Predicted,
                minkFoxAbs$Predicted),
  low = c(minkFoxPres$lower,
          minkFoxAbs$lower),
  high = c(minkFoxPres$upper,
           minkFoxAbs$upper),
  conditional = rep(c('Red fox present', 'Red fox absent'),
                    each = 100)
)
#plot conditional mink occupancy for presence and absence of fox
minkFoxPlot <- ggplot(gg_df_cond, aes(x = time, y = occupancy, group = conditional)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = conditional), alpha = 0.8) +
  geom_line(aes(linetype = conditional), linewidth = 1) +
  ylab('Conditional mink\noccupancy probability') +
  xlab('time (years)') +
  scale_fill_manual(name = 'Red Fox State', values = c("#003666", "#006638")) +
  scale_linetype_manual(name = 'Red Fox State', values = c("solid", "dashed")) +
  theme(text = element_text(size = 15), legend.position = c(0.75, 0.85)) +
  scale_x_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))+
  theme_bw()


#marten
minkMartPres <- predict(OMFit, type = 'state', species = 'mink', cond = 'pine_marten', newdata = predData)
minkMartAbs <- predict(OMFit, type = 'state', species = 'mink', cond = '-pine_marten', newdata = predData)

#create dataframe for plotting
gg_df_cond <- data.frame(
  time = rep(predData$time, 2),
  occupancy = c(minkMartPres$Predicted,
                minkMartAbs$Predicted),
  low = c(minkMartPres$lower,
          minkMartAbs$lower),
  high = c(minkMartPres$upper,
           minkMartAbs$upper),
  conditional = rep(c('Pine marten present', 'Pine marten absent'),
                    each = 100)
)

#plot conditional mink occupancy for presence and absence of marten
minkMartPlot <- ggplot(gg_df_cond, aes(x = time, y = occupancy, group = conditional)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = conditional), alpha = 0.8) +
  geom_line(aes(linetype = conditional), linewidth = 1) +
  ylab('Conditional mink\noccupancy probability') +
  xlab('time (years)') +
  scale_fill_manual(name = 'Pine Marten State', values = c("#003666", "#006638")) +
  scale_linetype_manual(name = 'Pine Marten State', values = c("solid", "dashed")) +
  theme(text = element_text(size = 15), legend.position = c(0.75, 0.85)) +
  scale_x_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))+
  theme_bw()


#plot amount of site-year combinations per year
ggplot()+
  geom_histogram(aes(x = cov_time),bins = 5,colour = "black", fill = "grey")+
  xlab("Year")+
  theme_classic()+
  theme(axis.text.x = element_text(size = rel(2)), axis.text.y = element_text(size = rel(2)))+
  theme(axis.title.x = element_text(size = rel(2)), axis.title.y = element_text(size = rel(2)))+
  scale_x_continuous(breaks = 1:5, labels = 2019:2023)

####activity pattern analysis####   
#exploring co-occurence in sites !!!! THIS BLOCK WAS WRITTEN BEFORE THE INTRODUCTION OF SITE-YEAR COMBINATIONS - it handles sites only, not site-year combinations!!!
obsTest <- pivot_wider(obs, names_from = scientificName, values_from = scientificName, values_fill = NA) %>% 
  select(locationName,'Martes martes','Lutra lutra','Vulpes vulpes','Neovison vison') %>%
  mutate(`Martes martes` = if_else(is.na(`Martes martes`), 0, 1)) %>% 
  mutate(`Vulpes vulpes` = if_else(is.na(`Vulpes vulpes`), 0, 1)) %>% 
  mutate(`Lutra lutra` = if_else(is.na(`Lutra lutra`), 0, 1)) %>% 
  mutate(`Neovison vison` = if_else(is.na(`Neovison vison`), 0, 1)) %>% 
  group_by(locationName) %>% 
  summarise(`Martes martes` = max(`Martes martes`),`Lutra lutra` = max(`Lutra lutra`),`Vulpes vulpes` = max(`Vulpes vulpes`),`Neovison vison` = max(`Neovison vison`)) %>% 
  ungroup() %>% 
  mutate(combCol = paste(`Martes martes`,`Lutra lutra`,`Vulpes vulpes`,`Neovison vison`)) %>%
  filter(`Neovison vison` == 1) %>% 
  group_by(combCol) %>% 
  summarise(n())
#co-occurrence combinations and amount of sites with these co-occurrence combinations
message('Marten ', 'Otter ', 'Fox ', 'Mink')
obsTest

#activity pattern testing
#create states of presence/absence per site-year combination
siteStates <- pivot_wider(obs, names_from = scientificName, values_from = scientificName, values_fill = NA) %>% 
  select(locationName,'Martes martes','Lutra lutra','Vulpes vulpes','Neovison vison', timestamp) %>%
  mutate(year = year(timestamp)) %>% 
  mutate(`Martes martes` = if_else(is.na(`Martes martes`), 0, 1)) %>% 
  mutate(`Vulpes vulpes` = if_else(is.na(`Vulpes vulpes`), 0, 1)) %>% 
  mutate(`Lutra lutra` = if_else(is.na(`Lutra lutra`), 0, 1)) %>% 
  mutate(`Neovison vison` = if_else(is.na(`Neovison vison`), 0, 1)) %>% 
  group_by(locationName, year) %>% 
  summarise(`Martes martes` = max(`Martes martes`),`Lutra lutra` = max(`Lutra lutra`),`Vulpes vulpes` = max(`Vulpes vulpes`),`Neovison vison` = max(`Neovison vison`)) %>% 
  ungroup() %>% 
  mutate(siteID = paste(locationName, year, sep = "_")) %>% 
  select(siteID,'Martes martes','Lutra lutra','Vulpes vulpes','Neovison vison')

#get site year combinations where only mink were present
siteStates %>% filter(`Neovison vison` == 1 & `Lutra lutra` == 0 & `Vulpes vulpes` == 0 & `Martes martes` == 0)
#getting sites with only mink and no other species
sitesMinkOnly <- siteStates %>% 
  filter(`Neovison vison` == 1 & `Lutra lutra` == 0 & `Vulpes vulpes` == 0 & `Martes martes` == 0) %>% 
  select(siteID)
#get dataframe with species combinations and site ID
obsSiteID <- pivot_wider(obs, names_from = scientificName, values_from = scientificName, values_fill = NA) %>% 
  select(locationName,'Martes martes','Lutra lutra','Vulpes vulpes','Neovison vison', timestamp) %>%
  mutate(year = year(timestamp)) %>% 
  mutate(`Martes martes` = if_else(is.na(`Martes martes`), 0, 1)) %>% 
  mutate(`Vulpes vulpes` = if_else(is.na(`Vulpes vulpes`), 0, 1)) %>% 
  mutate(`Lutra lutra` = if_else(is.na(`Lutra lutra`), 0, 1)) %>% 
  mutate(`Neovison vison` = if_else(is.na(`Neovison vison`), 0, 1)) %>% 
  filter(`Neovison vison` == 1 & `Lutra lutra` == 0 & `Vulpes vulpes` == 0 & `Martes martes` == 0) %>% 
  mutate(siteID = paste(locationName, year, sep = "_"))

#get number of observations of mink without other animals
obsSiteID %>% 
  filter(siteID %in% sitesMinkOnly$siteID)

#sites with only mink and other species
sitesMinkOtt <- siteStates %>% 
  filter(`Neovison vison` == 1 & `Lutra lutra` == 1 & `Vulpes vulpes` == 0 & `Martes martes` == 0) %>% 
  select(siteID)
sitesMinkFox <- siteStates %>% 
  filter(`Neovison vison` == 1 & `Lutra lutra` == 0 & `Vulpes vulpes` == 1 & `Martes martes` == 0) %>% 
  select(siteID)
sitesMinkMart <- siteStates %>% 
  filter(`Neovison vison` == 1 & `Lutra lutra` == 0 & `Vulpes vulpes` == 0 & `Martes martes` == 1) %>% 
  select(siteID)
#get number of observations of other species only with mink
obsSiteID %>% 
  filter(siteID %in% sitesMinkOtt$siteID)
obsSiteID %>% 
  filter(siteID %in% sitesMinkFox$siteID)
obsSiteID %>% 
  filter(siteID %in% sitesMinkMart$siteID)



#create dataframe with sites where mink were present/absent
minkNoFox <- siteStates %>% 
  filter(`Neovison vison` == 1, `Vulpes vulpes` == 0) %>% 
  mutate(state = "minkNoFox")
minkFox <- siteStates %>% 
  filter(`Neovison vison` == 1, `Vulpes vulpes` == 1) %>% 
  mutate(state = "minkFox")
minkNoOtt <- siteStates %>% 
  filter(`Neovison vison` == 1, `Lutra lutra` == 0) %>% 
  mutate(state = "minkNoOtt")
minkOtt <- siteStates %>% 
  filter(`Neovison vison` == 1, `Lutra lutra` == 1) %>% 
  mutate(state = "minkOtt")
minkNoMart <- siteStates %>% 
  filter(`Neovison vison` == 1, `Martes martes` == 0) %>% 
  mutate(state = "minkNoMart")
minkMart <- siteStates %>% 
  filter(`Neovison vison` == 1, `Martes martes` == 1) %>% 
  mutate(state = "minkMart")

#getting coords for suntimes
coords <- camsncoords %>% 
  select(name, lat, lng) %>% 
  rename(locationName = name)

#getting data for fitting activity patterns
actObs <- obs %>% 
  select(locationName,timestamp, scientificName) %>% 
  filter(scientificName == "Neovison vison") %>% 
  mutate(year = year(timestamp)) %>% 
  mutate(siteID = paste(locationName, year, sep = "_")) %>% 
  left_join(coords, by = "locationName") %>% 
  mutate(timeRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$solar) %>%  #testRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$clock) ### This function somehow does not function when testing with a single date and coordinates. This code can test whether clock times are actually different from suntimes (they are)
  #mutate(timeRad = (as.numeric(as_hms(timestamp))/86400)*2*pi) %>% 
  select(siteID, timeRad, lat, lng)
  
#getting data for fitting activity patterns for each state
minkNoFoxObs <- actObs %>% 
  filter(siteID %in% minkNoFox$siteID)
minkFoxObs <- actObs %>% 
  filter(siteID %in% minkFox$siteID)
minkNoOttObs <- actObs %>% 
  filter(siteID %in% minkNoOtt$siteID)
minkOttObs <- actObs %>% 
  filter(siteID %in% minkOtt$siteID)
minkNoMartObs <- actObs %>% 
  filter(siteID %in% minkNoMart$siteID)
minkMartObs <- actObs %>% 
  filter(siteID %in% minkMart$siteID)
message(paste("with/without \n","fox",nrow(minkFoxObs),"/",nrow(minkNoFoxObs),"\n","Otter",nrow(minkOttObs),"/",nrow(minkNoOttObs),"\n")," Marten ",nrow(minkMartObs),"/",nrow(minkNoMartObs))
message("!!! OTTER IS PROBLEMATIC, NOT ENOUGH OBSERVATIONS WITH FOR PROPER COMPARISON")

#fit activity pattern and compare with and without other predator
minkNoFoxFit <- fitact(minkNoFoxObs$timeRad, sample = "data")
minkFoxFit <- fitact(minkFoxObs$timeRad, sample = "data")
minkFoxComp <- compareCkern(minkNoFoxFit, minkFoxFit)

minkNoOttFit <- fitact(minkNoOttObs$timeRad, sample = "data")
minkOttFit <- fitact(minkOttObs$timeRad, sample = "data")
minkOttComp <- compareCkern(minkNoOttFit, minkOttFit)

minkNoMartFit <- fitact(minkNoMartObs$timeRad, sample = "data")
minkMartFit <- fitact(minkMartObs$timeRad, sample = "data")
minkMartComp <- compareCkern(minkNoMartFit, minkMartFit)

#actComps <- rbind(minkFoxComp,minkOttComp,minkMartComp)
#write_csv(as.data.frame(round(actComps, 3)), "Output/actComps.csv")

overlapPlot(minkNoMartObs$timeRad,minkMartObs$timeRad, xcenter = "midnight", rug = T, linecol = c("red","blue"), linewidth = c(2,2), main = "", ylab = "American mink activity density")
legend("topright", c("Pine marten absent", "Pine marten present"),lty=c(1,2), col=c("red","blue"))

overlapPlot(minkNoOttObs$timeRad,minkOttObs$timeRad, xcenter = "midnight", rug = T, linecol = c("red","blue"), linewidth = c(2,2), main = "", ylab = "American mink activity density")
legend("topright", c("Eurasian otter absent", "Eurasian otter present"),lty=c(1,2), col=c("red","blue"))

overlapPlot(minkNoFoxObs$timeRad,minkFoxObs$timeRad, xcenter = "midnight", rug = T, linecol = c("red","blue"), linewidth = c(2,2), main = "", ylab = "American mink activity density")
legend("topright", c("Red fox absent", "Red fox present"),lty=c(1,2), col=c("red","blue"))


#code for plotting, centered on night
plot(minkNoFoxFit, data = "rug", centre = "night", main = "Red fox absent", ylab = "American mink activity frequency")
plot(minkFoxFit, data = "rug", centre = "night", main = "Red fox present", ylab = "American mink activity frequency")
plot(minkNoOttFit, data = "rug", centre = "night", main = "Eurasian otter absent", ylab = "American mink activity frequency")
plot(minkOttFit, data = "rug", centre = "night", main = "Eurasian otter present", ylab = "American mink activity frequency")
plot(minkNoMartFit, data = "rug", centre = "night", main = "Pine marten absent", ylab = "American mink activity frequency")
plot(minkMartFit, data = "rug", centre = "night", main = "Pine marten present", ylab = "American mink activity frequency")

obsFox <- obs %>% 
  filter(scientificName == "Vulpes vulpes") %>% 
  select(timestamp, locationName) %>% 
  left_join(coords, by = "locationName") %>% 
  mutate(timeRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$solar)  #testRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$clock) ### This function somehow does not function when testing with a single date and coordinates. This code can test whether clock times are actually different from suntimes (they are)
foxFit <- fitact(obsFox$timeRad, sample = "data")

obsOtt <- obs %>% 
  filter(scientificName == "Lutra lutra") %>%
  select(timestamp, locationName) %>% 
  left_join(coords, by = "locationName") %>% 
  mutate(timeRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$solar)  #testRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$clock) ### This function somehow does not function when testing with a single date and coordinates. This code can test whether clock times are actually different from suntimes (they are)
ottFit <- fitact(obsOtt$timeRad, sample = "data")

obsMart <- obs %>% 
  filter(scientificName == "Martes martes") %>%
  select(timestamp, locationName) %>% 
  left_join(coords, by = "locationName") %>% 
  mutate(timeRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$solar)  #testRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$clock) ### This function somehow does not function when testing with a single date and coordinates. This code can test whether clock times are actually different from suntimes (they are)
martFit <- fitact(obsMart$timeRad, sample = "data")

obsMink <- obs %>% 
  filter(scientificName == "Neovison vison") %>%
  select(timestamp, locationName) %>% 
  left_join(coords, by = "locationName") %>% 
  mutate(timeRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$solar)  #testRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$clock) ### This function somehow does not function when testing with a single date and coordinates. This code can test whether clock times are actually different from suntimes (they are)
minkFit <- fitact(obsMink$timeRad, sample = "data")

#plot activity patterns separately
plot(martFit, data = "rug", centre = "night", ylab = "Pine marten activity frequency")
plot(foxFit, data = "rug", centre = "night", ylab = "Red fox activity frequency")
plot(ottFit, data = "rug", centre = "night", ylab = "Eurasian otter activity frequency")
plot(minkFit, data = "rug", centre = "night", ylab = "American mink activity frequency")


####Activity patterns of other predators in presence and absence of mink####
ottNoMink <- siteStates %>% 
  filter(`Neovison vison` == 0, `Lutra lutra` == 1) %>% 
  mutate(state = "minkNoFox")
ottMink <- siteStates %>% 
  filter(`Neovison vison` == 1, `Lutra lutra` == 1) %>% 
  mutate(state = "minkFox")
foxNoMink <- siteStates %>% 
  filter(`Neovison vison` == 0, `Vulpes vulpes` == 1) %>% 
  mutate(state = "minkNoOtt")
foxMink <- siteStates %>% 
  filter(`Neovison vison` == 1, `Vulpes vulpes` == 1) %>% 
  mutate(state = "minkOtt")
martNoMink <- siteStates %>% 
  filter(`Neovison vison` == 0, `Martes martes` == 1) %>% 
  mutate(state = "minkNoMart")
martMink <- siteStates %>% 
  filter(`Neovison vison` == 1, `Martes martes` == 1) %>% 
  mutate(state = "minkMart")

#getting observations
actObsPreds <- obs %>% 
  select(locationName,timestamp, scientificName) %>% 
  mutate(year = year(timestamp)) %>% 
  mutate(siteID = paste(locationName, year, sep = "_")) %>% 
  left_join(coords, by = "locationName") %>% 
  mutate(timeRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$solar) %>%  #testRad = solartime(timestamp, lat = lat, lon = lng, tz = 1)$clock) ### This function somehow does not function when testing with a single date and coordinates. This code can test whether clock times are actually different from suntimes (they are)
  select(siteID, timeRad, scientificName)


#get observations for predators in locations where mink were present or absent
ottNoMinkObs <- actObsPreds %>% 
  filter(scientificName == "Lutra lutra") %>% 
  filter(siteID %in% ottNoMink$siteID)
ottMinkObs <- actObsPreds %>% 
  filter(scientificName == "Lutra lutra") %>% 
  filter(siteID %in% ottMink$siteID)
foxNoMinkObs <- actObsPreds %>% 
  filter(scientificName == "Vulpes vulpes") %>% 
  filter(siteID %in% foxNoMink$siteID)
foxMinkObs <- actObsPreds %>% 
  filter(scientificName == "Vulpes vulpes") %>% 
  filter(siteID %in% foxMink$siteID)
martNoMinkObs <- actObsPreds %>% 
  filter(scientificName == "Martes martes") %>% 
  filter(siteID %in% martNoMink$siteID)
martMinkObs <- actObsPreds %>% 
  filter(scientificName == "Martes martes") %>% 
  filter(siteID %in% martMink$siteID)
message(paste("without/with \n","Otter",nrow(ottNoMinkObs),"/",nrow(ottMinkObs),"\n","Fox",nrow(foxNoMinkObs),"/",nrow(foxMinkObs),"\n")," Marten ",nrow(martNoMinkObs),"/",nrow(martMinkObs))
message("Otter IS PROBLEMATIC, NOT ENOUGH OBSERVATIONS FOR COMPARISON")

#fit activity pattern and compare with and without mink
ottNoMinkFit <- fitact(ottNoMinkObs$timeRad, sample = "data")
ottMinkFit <- fitact(ottMinkObs$timeRad, sample = "data")
ottMinkComp <- compareCkern(ottNoMinkFit, ottMinkFit)

foxNoMinkFit <- fitact(foxNoMinkObs$timeRad, sample = "data")
foxMinkFit <- fitact(foxMinkObs$timeRad, sample = "data")
foxMinkComp <- compareCkern(foxNoMinkFit, foxMinkFit)

martNoMinkFit <- fitact(martNoMinkObs$timeRad, sample = "data")
martMinkFit <- fitact(martMinkObs$timeRad, sample = "data")
martMinkComp <- compareCkern(martNoMinkFit, martMinkFit)

#plotting results

overlapPlot(ottNoMinkObs$timeRad,ottMinkObs$timeRad, xcenter = "midnight", rug = T, linecol = c("red","blue"), linewidth = c(2,2), main = "", ylab = "Eurasian otter activity density")
legend("topright", c("American mink absent", "American mink present"),lty=c(1,2), col=c("red","blue"))

overlapPlot(foxNoMinkObs$timeRad,foxMinkObs$timeRad, xcenter = "midnight", rug = T, linecol = c("red","blue"), linewidth = c(2,2), main = "", ylab = "Red fox activity density")
legend("topright", c("American mink absent", "American mink present"),lty=c(1,2), col=c("red","blue"))

overlapPlot(martNoMinkObs$timeRad,martMinkObs$timeRad, xcenter = "midnight", rug = T, linecol = c("red","blue"), linewidth = c(2,2), main = "", ylab = "Pine marten activity density")
legend("topright", c("American mink absent", "American mink present"),lty=c(1,2), col=c("red","blue"))

plot(ottNoMinkFit, data = "rug", centre = "night", main = "American mink absent", ylab = "Eurasian otter activity frequency")
plot(ottMinkFit, data = "rug", centre = "night", main = "American mink present", ylab = "Eurasian otter activity frequency")
plot(foxNoMinkFit, data = "rug", centre = "night", main = "American mink absent", ylab = "Red fox activity frequency")
plot(foxMinkFit, data = "rug", centre = "night", main = "American mink present", ylab = "Red fox activity frequency")
plot(martNoMinkFit, data = "rug", centre = "night", main = "American mink absent", ylab = "Pine marten activity frequency")
plot(martMinkFit, data = "rug", centre = "night", main = "American mink present", ylab = "Pine marten activity frequency")


####Appendix - chance calculator from log odds####
# read in data
#manually add marginal mink occupancy to 2nd order natural paramaters to get conditional mink occupancy
#this could have been done more cleanly higher up, but I did it in a separate script later. I only merged it now.
{odd <- read_csv("Raw Data/odds.csv")
odd[5:7,2] <- odd[5:7,2]+as.numeric(odd[1,2])
}

#define function for turning log odds into percentages not used but would be better if I update this for anything in the future, added as an afterthought
perc <- function(x){
  100*(exp(x)/(1+exp(x)))
}

#calculate percentages
chance <- odd %>% 
  rename(chanceCILow = chanceCI) %>% 
  mutate(chance = 100*(exp(odds)/(1+exp(odds))),
         chanceCIHigh = 100*(exp(odds + 1.96*oddsCI)/(1+exp(odds+1.96*oddsCI))),
         chanceCILow = 100*(exp(odds - 1.96*oddsCI)/(1+exp(odds-1.96*oddsCI)))) %>% 
  mutate(chance = round(chance, 2),
         chanceCIHigh = round(chanceCIHigh, 2),
         chanceCILow = round(chanceCILow, 2)) %>% 
  select(Var,chanceCILow,chance,chanceCIHigh)

write_csv(chance,"Output/chanceTable.csv")

####Appendix - photo organiser script####
#this is the script I used to reorganise the photos into different folders with proper names
setwd("C:/Users/FuckOffWindows/Downloads/Holmon photos")

library(pacman)
p_load(terra)

#get filenames
filenames <- list.files()

#get datetime
dateTime <- .POSIXct(vector(length = length(filenames)), tz = "CET")
for(i in 1:length(dateTime)){
  tempString <- str_sub(filenames[i], end = 19)
  dateTime[i] <- ymd_hms(tempString, tz = "CET")
}

###get picture site name###
#getting rid of date and time from downloading picture
filenamesNoDatTime <- filenames
for(i in 1:length(filenamesNoDatTime)){
  filenamesNoDatTime[i] <- str_sub(filenames[i], start = 26)
}

#getting rid of some unwanted words
filenamesNoDatTime <- str_remove_all(filenamesNoDatTime, "Fwd_ ")

#change filenames to folder names
folderNames <- filenamesNoDatTime %>% 
  str_replace('.+, (.+)','\\1') %>% 
  str_sub(end = -5)

###change names based on dataframe###
#load dataframe
camNames <- read_csv("C:/R Projects/MSc Thesis/Output/CameraNameTest.csv")

#renames
namesToChange <- camNames %>% 
  filter(str_detect(Specialities, "Rename to")) %>% 
  select(value, Specialities) %>% 
  mutate(Specialities = str_replace(Specialities, "Rename to ", ""))

for(i in 1:length(folderNames)){
  if(folderNames[i] %in% namesToChange$value){
    tempIndex <- which(folderNames[i] == namesToChange$value)
    folderNames[i] <- namesToChange$Specialities[tempIndex]
  }
}

#merges
namesToMerge <- camNames %>% 
  filter(str_detect(Specialities, "Merge with")) %>% 
  select(value, Specialities) %>% 
  mutate(Specialities = str_replace(Specialities, "Merge with ", ""))

for(i in 1:length(folderNames)){
  if(folderNames[i] %in% namesToMerge$value){
    tempIndex <- which(folderNames[i] == namesToMerge$value)
    folderNames[i] <- namesToMerge$Specialities[tempIndex]
  }
}

#mh bjurholm is slightly different, it was used in another project, then moved later on, so only pictures after may 24 2022 are valid
for(i in 1:length(filenames)){
  if((dateTime[i] < dmy("24-05-2022")) & folderNames[i] == "mh bjurholm2"){
    filenames[i] <- NA
    folderNames[i] <- NA
    dateTime[i] <- NA
    filenamesNoDatTime[i] <- NA
    
  }
}

filenames <- filenames[!is.na(filenames)]
folderNames <- folderNames[!is.na(folderNames)]
dateTime <- dateTime[!is.na(dateTime)]
filenamesNoDatTime <- filenamesNoDatTime[!is.na(filenamesNoDatTime)]

filterTibble <- camNames %>% 
  filter(valid) %>% 
  select(value)

filterVector <- folderNames %in% filterTibble$value

filenames <- filenames[filterVector]
folderNames <- folderNames[filterVector]
dateTime <- dateTime[filterVector]
filenamesNoDatTime <- filenamesNoDatTime[filterVector]


###only run when writing files###

#for(i in 1:length(unique(folderNames))){
#dir.create(paste("C:/Holmon photos/", unique(folderNames)[i], sep = ''), showWarnings = F)
#}

#copy the files
#for(i in 1:length(folderNames)){
#print(i)
#file.copy(filenames[i], paste("C:/Holmon photos/", folderNames[i], "/", filenamesNoDatTime[i], sep = ''))
#Sys.setFileTime(paste("C:/Holmon photos/", folderNames[i], "/", filenamesNoDatTime[i], sep = ''), dateTime[i])
#}

#cameraNames <- filenamesNoDatTime
#for(i in 1:length(filenamesNoDatTime)){
#temp[i] <- str_replace(filenamesNoDatTime[i],".*, ",'')
#cameraNames[i] <- str_sub(temp[i], end = -5)
#}



#generating long lat data for agouti
#1- is filtered out separately because it contains pictures that have to be sorted manually
latLong <- camNames %>% 
  filter(valid, value != "1-", !is.na(x), !is.na(y)) %>% 
  select(x,y)

camsncoords <- camNames %>% 
  filter(valid, value != "1-", !is.na(x), !is.na(y)) %>% 
  select(value)


points <- vect(latLong, geom=c("x","y"), crs = crs("epsg:3021"))
pointsSweRef99 <- project(points, crs("epsg:3006"))

pointsLatLong <- project(points, crs("epsg:4326"))
coordsLatLong <- as_tibble(crds(pointsLatLong))

camsncoords <- bind_cols(camsncoords,coordsLatLong)


#only run if rewriting coordinates for agouti
#write_csv(camsncoords, "C:/R Projects/MSc thesis/Output/Cams 'n Coords.csv")

####Appendix - raster creation script####
#I lost the script used to filter the cameras afterwards, but it's a simple matter of sampling the data from the saltdistance raster at the points of the cameras and then filtering them to only include cameras with values of =< 100 
#loading in extra packages
p_load(sf, raster)
#loading in Västerbotten land cover raster
landCover <- raster("Raw Data/landCoverSweden.tif")
plot(landCover)

#cropping landcover raster to holmön
holmExtent <- extent(c(782045,799381),c(7061870,7091955))
landCoverHolm <- crop(landCover, holmExtent)
plot(landCoverHolm)

#removing unnecessary landcover raster
rm(landCover)


#defining functions for creating salt and freshwater rasters
saltFun <- function(x){
  if(x == 62){
    return(1)
  }else{
    return(0)
  }
}
freshFun <- function(x){
  if(x == 61){
    return(1)
  }else{
    return(0)
  }
}

#creating salt and freshwater rasters
saltRast <- calc(landCoverHolm,saltFun)
freshRast <- calc(landCoverHolm,freshFun)

plot(saltRast)
plot(freshRast)

#creating function that defines center cell as NA if all neighbours have a value of 1 (so you only get shore cells, this should make calculations signficantly faster)
filterfun <- function(x, ...){
  y <- x[!is.na(x)]
  a <- ifelse(sum(y) < 9, x[5], NA)
  return(a)
}
#applying above function to rasters
saltRastSimple <- focal(saltRast, matrix(c(1,1,1,1,1,1,1,1,1), nrow = 3, ncol = 3),fun = filterfun, pad = T, padValue = 1)

#defining function that only selects values of 1 (so water)
waterfun <- function(x){
  x == 1
}

#create points
freshpoints <- rasterToPoints(freshRast, fun = waterfun, spatial = T)
freshDistance <- distanceFromPoints(freshRast, freshpoints)

#creating salt points
saltpoints <- rasterToPoints(saltRastSimple,fun = waterfun, spatial = T)
saltDistance <- distanceFromPoints(saltRastSimple, saltpoints)
plot(saltDistance)

#creating combined water raster
watRast <- saltRast + freshRast

#creating inverting function
invFun <- function(x){
  abs(x-1)
}

#inverting watRast
watRastInv <- calc(watRast, invFun)

#removing water data from the rasters
saltDistFinal <- saltDistance * watRastInv
freshDistFinal <- freshDistance * watRastInv

#replacing water with NA
replaceFun <- function(x){
  return(ifelse(x == 0, NA, x))
}
saltDistFinal <- calc(saltDistFinal, replaceFun)
freshDistFinal <- calc(freshDistFinal, replaceFun)


raster::writeRaster(saltDistFinal, filename = "Output/saltdistance.tif", overwrite = T)
raster::writeRaster(freshDistFinal, filename = "Output/freshdistance.tif", overwrite = T)
####Appendix - descriptive statistics####
#number of observations per species
nrow(minkObs)
nrow(martObs)
nrow(ottObs)
nrow(foxObs)
#combining dataframes
targObs <- rbind(minkObs,martObs,ottObs,foxObs)

#create dataframe with percent of cameras that took pictures per year
distFram <- targObs %>%
  mutate(timestamp = year(timestamp)) %>% 
  group_by(scientificName, timestamp) %>% 
  summarise(count = n())

#pivot to a wider dataframe where every year has its own column
pivot_wider(distFram, names_from = timestamp, values_from = count) %>% 
  select('scientificName', '2019', '2020', '2021', '2022', '2023') %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  mutate(Total = sum(`2019`, `2020`, `2021`, `2022`, `2023`))

#getting amount of sites animals were spotted at
siteDistFram <- targObs %>% 
  mutate(timestamp = year(timestamp)) %>% 
  group_by(scientificName, timestamp) %>%
  summarise(sites = length(unique(locationName)))  
  

#function to determine deployment years
deplYears <- function(start,end,start2,end2){
  if(is.na(start2)){
    return(seq(start,end))
  }else{
    return(c(seq(start,end), seq(start2,end2)))
  }
}

#getting years all cameras were active in
camsDeplYears <- camsncoords %>% 
  mutate(startDate = year(startDate), endDate = year(endDate), altStart = year(altStart), altEnd = year(altEnd)) %>% 
  rowwise() %>% 
  mutate(activeYears = list(deplYears(startDate,endDate,altStart,altEnd))) %>% 
  select(name,activeYears) %>% 
  mutate(`2019` = 2019 %in% unlist(activeYears), `2020` = 2020 %in% unlist(activeYears), `2021` = 2021 %in% unlist(activeYears), `2022` = 2022 %in% unlist(activeYears), `2023` = 2023 %in% unlist(activeYears)) %>% 
  ungroup() %>%
  summarise(`2019`=sum(`2019`),`2020`=sum(`2020`),`2021`=sum(`2021`),`2022`=sum(`2022`),`2023`=sum(`2023`))

  
#calculating percentage of sites animals were spotted at
obsSitePerYear <- pivot_wider(siteDistFram, names_from = timestamp, values_from = sites) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  select(scientificName, `2019`,`2020`,`2021`,`2022`,`2023`)

relSiteObserved <- obsSitePerYear %>% 
  mutate(`2019` = (`2019`/camsDeplYears$`2019`)*100, `2020` = (`2020`/camsDeplYears$`2020`)*100, `2021` = (`2021`/camsDeplYears$`2021`)*100, `2022` = (`2022`/camsDeplYears$`2022`)*100, `2023` = (`2023`/camsDeplYears$`2023`)*100)
