# Install necessary packages

wants <- c('tm', 'scales', 'aws.s3', 'TTR', 'data.table', 'RColorBrewer', 'phonTools', 'spectral',
           'TSA', 'gganimate', 'roll', 'tidyverse', 'plotly', 'lubridate', 'data.table',
           'expss', 'ggplot2', 'reshape2', 'varhandle', 'zoo')

has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
sapply(wants, require, character.only = TRUE)
rm("wants","has")
