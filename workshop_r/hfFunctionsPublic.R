# This function cleans and joins modals (metadata)
# Input: raw modal and partcount files
# Output: joined metadata file
# Arguments:
# lfPAll: raw partcount file 
# lfModalsAll: raw modals file
# path: which machine path to analyze

getCleanedModals <- function(lfPAll = execParts, lfModalsAll = modals, path = 1){
  
  lfArt <- full_join(lfPAll, lfModalsAll)
  
  lfArt <- lfArt %>% arrange(timestamp)
  
  if(path == 2){
    
    #use this if looking at path2
    lfArt <- lfArt %>% filter((key == 'partcount' & path == 1) | (path == 2))
    
  }
  
  if(path == 1){
    
    #use this if looking at path1
    lfArt <- lfArt %>% filter(path == 1)
    
  }
  
  #continue
  
  lfArt <- longToWide(lfArt, ltz = 'UTC')
  
  if(isTRUE(lfArt$M == 0)){lfArt$execution == 0}
  
  lfArt$execution <- as.numeric(lfArt$execution)
  
  lfArt$execution <- ifelse((lfArt$execution == 0), 0, 1)
  
  return(lfArt)
  
}

# This function joins the metadata and high-freq data together into one "wide" dataframe
# Input: cleaned metadata dataframe and raw high-frequency data
# Output: joined dataframe for final analysis
# Arguments:
# lfArt: cleaned metadata dataframe
# wAll: raw high-frequency data
# block: do G-code blocks exist in metadata?

joinModalHF <- function(lfArt = lfArt, wAll = wAll, block = FALSE){
  
  lfF <- full_join(wAll, lfArt) %>% filter(timestamp < max(wAll$timestamp, na.rm = T), 
                                                        timestamp > min(wAll$timestamp, na.rm = T)) %>% 
                                                        arrange(timestamp) 
  
  #if block (G-Code) exists, use this
  
  if(block == TRUE){
    
    #only consider when BLOCK is there
    
    NonNAindex <- which(!is.na(lfF$block))
    firstNonNA <- min(NonNAindex)
    
    lfF <- lfF %>% filter(row_number() >= firstNonNA)
    
    lfF$block <- na.locf(lfF$block)
    
    lfF <- lfF %>% filter(!is.na(block))
    
  }
  
  NonNAindex <- which(!is.na(lfF$execution))
  firstNonNA <- min(NonNAindex)
  
  lfF <- lfF %>% filter(row_number() >= firstNonNA)
  
  lfF$execution <- na.locf(lfF$execution)
  
  if('T' != colnames(lfF)){
  
    NonNAindex <- which(!is.na(lfF$T))
    firstNonNA <- min(NonNAindex)
    
    lfF <- lfF %>% filter(row_number() >= firstNonNA)
    lfF$T <- na.locf(lfF$T)
  
  }
  
  if('partcount' %in% colnames(lfF)){
    
    NonNAindex <- which(!is.na(lfF$partcount))
    firstNonNA <- min(NonNAindex)
    
    lfF <- lfF %>% filter(row_number() >= firstNonNA)
    
    lfF$partcount <- as.numeric(lfF$partcount)
    
    lfF$partcount[!is.na(lfF$partcount)] <- 1
    
    lfF <- lfF %>% mutate(counter = partcount)
    
    lfF$counter[is.na(lfF$counter)] <- 0
    
    lfF$counter <- cumsum(lfF$counter) + 1
    
    lfF <- lfF %>% filter(counter != 1, row_number() > 1)
    
  }
  
  lfF$block <- lfF$N <- lfF$O <- lfF$partcount <- lfF$M <- NULL
  
  lfF <- lfF %>%
    dplyr::rename(load = `SPINDLE.1.load`) %>%
    filter(!is.na(load))
  
  if('counter' %in% colnames(lfF)){
    
    lfF <- lfF %>%
      group_by(counter) %>% mutate(number = 1) %>%
      mutate(ticker = cumsum(number)) %>% filter(!is.na(load)) 
    
  }
  
  lfF$ts_idx <- as.numeric(lfF$timestamp)
  lfF$idx <- 1:nrow(lfF)
  
  if('SPINDLE.2.load' %in% colnames(lfF)){
    
    lfF <- lfF %>% dplyr::rename(load2 = `SPINDLE.2.load`)
    
  }
  
  lfF$T <- as.numeric(lfF$T)
  
  return(lfF)
  
}

# This function generates thresholds for each metric considered, and creates a plotly with thresholds
# Input: cleaned summary dataframe with metrics by tool and part, parameters for threshold setting
# Output: plotly displaying thresholds
# Arguments:
# summed: cleaned summary dataframe with metrics by tool and part
# tuning: how many parts should be considered during the tuning period?
# badPart: what part does the tool breakage occur on?
# scale: centers and scales the data with R's default scale function
# name: what do we want to name the plotly output file?

getThresholds <- function(summed, tuning = 50, badPart = max(summed$counter), scale = FALSE, name = 'allTools.html'){
  
  if(scale == TRUE){
    
    summed[,3:ncol(summed)] <- scale(summed[,3:ncol(summed)])
    
  }
  
  thresholds <- summed %>% filter(counter < (min(counter) + tuning)) %>% 
    group_by(`T`) %>% summarise(sd_sd = sd(sd_load), mean_sd = mean(sd_load),
                                sd = sd(mean_load), mean = mean(mean_load), min = min(mean_load), max = max(mean_load),
                                min_sd = min(sd_load), max_sd = max(sd_load))
  
  summed <- summed %>% left_join(., thresholds)
  
  summed <- summed %>%
    mutate(min_threshold = min(mean - 3*sd, min), max_threshold = max(mean + 3*sd, max))
  
  a <- ggplot(data=summed) + aes(x=counter, y=(mean_load), colour=`T`, label = 'mean') +
    geom_line() + geom_point(aes(text=paste0("<br>timestamp:", ts, "\nload:", mean_load, "\ncounter:", counter, "\ntool:", T))) + 
    geom_vline(xintercept = badPart, colour = 'red')
  
  b <- a + geom_ribbon(aes(x=counter, ymin=min_threshold, ymax=max_threshold), alpha = 0.3)
  
  htmlwidgets::saveWidget(ggplotly(b, tooltip = 'text'), name)

}

# This function converts dataframes from long to wide based on timestamp
# Input: raw dataframe 
# Output: 'wide style' dataframe with timestamp as key
# Arguments:
# df: raw dataframe 
# timeidMin/timeidMax: time range to select
# ltz: timezone to cast times into

longToWide <- function(df, timeidMin = min(df$timestamp), timeidMax = max(df$timestamp), ltz = "America/New_York") {
  
  timeidMin <- ymd_hms(timeidMin, tz = ltz)
  timeidMax <- ymd_hms(timeidMax, tz = ltz)
  
  dfLeft <- df %>% filter(timestamp < timeidMax, timestamp > timeidMin) %>%
    dplyr::select(key, value, timestamp)
  
  dfLeft <- dfLeft[!duplicated(paste(dfLeft$key, dfLeft$timestamp)), ]
  dfLeftSpread <- tidyr::spread(data = dfLeft, key = timestamp, value = value)
  dfLeftSpread <- as.data.frame(t(dfLeftSpread))
  dfLeftSpread <- varhandle::unfactor(dfLeftSpread)
  
  colnames(dfLeftSpread) <- dfLeftSpread[1,]
  dfLeftSpread$timestamp <- rownames(dfLeftSpread)
  dfLeftSpread <- dfLeftSpread[-1,]
  dfLeftSpread$timestamp <- ymd_hms(dfLeftSpread$timestamp, tz = ltz)
  
  return(dfLeftSpread)
  
}
