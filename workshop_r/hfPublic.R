#Demo of High-Frequency Data Capabilities

#STEP 1: load in packages, functions, defaults, raw data

setwd('~/DataSciencePublic/workshop_r')

op <- options(digits.secs=3)

source('loadPackagesHF.R')
source('hfFunctionsPublic.R')

#load in raw data
#download from here: https://machinemetrics-public.s3-us-west-2.amazonaws.com/ds/hf_files.zip
#and put them in the workshop_r folder

hf <- read.csv('hf.csv', stringsAsFactors = FALSE)
execParts <- read.csv('execParts.csv', stringsAsFactors = FALSE)
modals <- read.csv('modals.csv', stringsAsFactors = FALSE)

#STEP 2: do some basic data cleaning

hf$timestamp <- ymd_hms(hf$timestamp)
execParts$timestamp <- ymd_hms(execParts$timestamp)
modals$timestamp <- ymd_hms(modals$timestamp)

#join modals, partcounts, and execution status

lfAll <- getCleanedModals(lfPAll = execParts, lfModalsAll = modals, path = 1)

#join HF with metadata

lfF <- joinModalHF(lfArt = lfAll, wAll = hf, block = FALSE)

#STEP 3: some simple feature engineering

#path 1
  
summed <- lfF %>% filter(execution == 1) %>% group_by(`T`, counter) %>% dplyr::summarise(sum_load = sum(load, na.rm = TRUE),  sd_load = sd(load, na.rm = TRUE),
                                                                                    mean_load = mean(load, na.rm = TRUE), 
                                                                                    med_load = median(load, na.rm = TRUE),
                                                                                    mean_speed = mean(`SPINDLE.1.motor.speed`, na.rm = TRUE), sd_speed = sd(`SPINDLE.1.motor.speed`, na.rm = TRUE), 
                                                                                    med_speed = median(`SPINDLE.1.motor.speed`, na.rm = TRUE),
                                                                                    ts = min(timestamp), count_load = n())


#STEP 4: identify bad part and plot

badtime <- as.POSIXct('2020-05-18 14:11:37', 'UTC')

bad <- which(abs(summed$ts-badtime) == min(abs(summed$ts - badtime)))

bad <- summed$counter[bad]

summed$T <- as.character(summed$T)

#plot all of the tools on the same graph

ggplot(data=summed,
       aes(x=counter, y=(mean_load), colour=`T`)) +
  geom_line() + geom_point() + geom_vline(xintercept = bad, colour = 'red')

htmlwidgets::saveWidget(ggplotly(), 'workshop.html')

#STEP 5: automated method for finding anomalies

getThresholds(summed, tuning = 75)
