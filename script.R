setwd('C:/Users/Taemin/Desktop/Intro2R/Assessment 3')
data <- read.csv("yarra-traffic-counts.csv", header = TRUE)

install.packages("dplyr")
library(dplyr)

install.packages('lubridate')
library(lubridate)
install.packages('devtools')
require(devtools)
install_github("Displayr/flipTime")
install.packages('flipTime')
library('flipTime')
library('stringr')


#Mutate data_capture into two variables, 'date_captured_month' and 'date_captured_year'
data_dates_mutated <- mutate(data,
            date_captured_month = as.integer(substr(AsDate(癤풼ate_captured), start = 6, stop = 7)),
            date_captured_year  = as.integer(substr(AsDate(癤풼ate_captured), start = 1, stop = 4))
                             )

#trim any white spaces
data[sapply(data, str_trim)]

#Group data
data_grouped <- data_dates_mutated %>%
  group_by(date_captured_month) %>%
  summarise(avg_volume = mean(volume_per_day), avg_speed = mean(X85th_percentile_speed), 
            suburb, year = date_captured_year)





test1 <- data_dates_mutated %>%
  filter(date_captured_year == 2011) %>%
  filter(suburb == "Abbotsford")


#visualise data
install.packages("ggplot2")
library("ggplot2")
traffic = ggplot(data_grouped, aes(x = date_captured_month, y = avg_volume)) 
traffic = traffic + geom_point(aes(color = suburb))
traffic
  

traffic_volume = ggplot(data_grouped, mapping = aes(x = date_captured_month, y = avg_volume))
traffic_volume = traffic_volume + geom_bar(mapping = aes(fill=suburb), stat = "identity", position ="dodge")
traffic_volume = traffic_volume + scale_x_discrete(limit = factor(c(1,2,3,4,5,6,7,8,9,10,11,12)))
traffic_volume 


traffic_speed = ggplot(data_grouped, mapping = aes(x = date_captured_month, y = avg_speed))
traffic_speed = traffic_speed + geom_bar(mapping = aes(fill=suburb), stat = "identity", position ="dodge")
traffic_speed = traffic_speed + scale_x_discrete(limit = factor(c(1,2,3,4,5,6,7,8,9,10,11,12)))
traffic_speed 






Abbotsford_speed = ggplot(bind_rows(filter(data_grouped, year ==2009 & suburb == 'Abbotsford'),
                                 filter(data_grouped, year ==2010 & suburb == 'Abbotsford'),
                                 filter(data_grouped, year ==2011 & suburb == 'Abbotsford'),
                                 filter(data_grouped, year ==2012 & suburb == 'Abbotsford'),
                                 filter(data_grouped, year ==2013 & suburb == 'Abbotsford'),
                                 filter(data_grouped, year ==2014 & suburb == 'Abbotsford'),
                                 filter(data_grouped, year ==2015 & suburb == 'Abbotsford'),
                                 filter(data_grouped, year ==2016 & suburb == 'Abbotsford'),
                                 filter(data_grouped, year ==2017 & suburb == 'Abbotsford'),
                                 filter(data_grouped, year ==2018 & suburb == 'Abbotsford'),
                                 filter(data_grouped, year ==2019 & suburb == 'Abbotsford'),
                                  ), value,
                       mapping = aes(x = date_captured_month, y = avg_speed, 
                                     colour = factor(year))) + labs(x="Month", y="speed (km/h)", 
                                                          colour="Year") + 
                                                ggtitle("AVERAGE SPEED IN ABBOTSFORD")
                                              
Abbotsford_speed = Abbotsford_speed + geom_line(linetype = "dashed") + geom_point() + theme_classic()
Abbotsford_speed = Abbotsford_speed + scale_x_discrete(limit = factor(c(1,2,3,4,5,6,7,8,9,10,11,12)))
Abbotsford_speed

Alphington_speed = ggplot(bind_rows(filter(data_grouped, year ==2009 & suburb == 'Alphington'),
                                    filter(data_grouped, year ==2010 & suburb == 'Alphington'),
                                    filter(data_grouped, year ==2011 & suburb == 'Alphington'),
                                    filter(data_grouped, year ==2012 & suburb == 'Alphington'),
                                    filter(data_grouped, year ==2013 & suburb == 'Alphington'),
                                    filter(data_grouped, year ==2014 & suburb == 'Alphington'),
                                    filter(data_grouped, year ==2015 & suburb == 'Alphington'),
                                    filter(data_grouped, year ==2016 & suburb == 'Alphington'),
                                    filter(data_grouped, year ==2017 & suburb == 'Alphington'),
                                    filter(data_grouped, year ==2018 & suburb == 'Alphington'),
                                    filter(data_grouped, year ==2019 & suburb == 'Alphington'),
                                ), value,
mapping = aes(x = date_captured_month, y = avg_speed, 
              colour = factor(year))) + labs(x="Month", y="speed (km/h)", 
                                             colour="Year") + 
  ggtitle("AVERAGE SPEED IN Alphington")

Alphington_speed = Alphington_speed + geom_line(linetype = "dashed") + geom_point() + theme_classic()
Alphington_speed = Alphington_speed + scale_x_discrete(limit = factor(c(1,2,3,4,5,6,7,8,9,10,11,12)))
Alphington_speed


Burnley_speed = ggplot(bind_rows(filter(data_grouped, year ==2009 & suburb == 'Burnley'),
                                    filter(data_grouped, year ==2010 & suburb == 'Burnley'),
                                    filter(data_grouped, year ==2011 & suburb == 'Burnley'),
                                    filter(data_grouped, year ==2012 & suburb == 'Burnley'),
                                    filter(data_grouped, year ==2013 & suburb == 'Burnley'),
                                    filter(data_grouped, year ==2014 & suburb == 'Burnley'),
                                    filter(data_grouped, year ==2015 & suburb == 'Burnley'),
                                    filter(data_grouped, year ==2016 & suburb == 'Burnley'),
                                    filter(data_grouped, year ==2017 & suburb == 'Burnley'),
                                    filter(data_grouped, year ==2018 & suburb == 'Burnley'),
                                    filter(data_grouped, year ==2019 & suburb == 'Burnley'),
                                  ), value,
mapping = aes(x = date_captured_month, y = avg_speed, 
              colour = factor(year))) + labs(x="Month", y="speed (km/h)", 
                                             colour="Year") + 
  ggtitle("AVERAGE SPEED IN Burnley")
Burnley_speed = Burnley_speed + geom_line(linetype = "dashed") + geom_point() + theme_classic()
Burnley_speed = Burnley_speed + scale_x_discrete(limit = factor(c(1,2,3,4,5,6,7,8,9,10,11,12)))
Burnley_speed



Carlton_North_speed = ggplot(bind_rows(filter(data_grouped, year ==2009 & suburb == 'Carlton North'),
                                 filter(data_grouped, year ==2010 & suburb == 'Carlton North'),
                                 filter(data_grouped, year ==2011 & suburb == 'Carlton North'),
                                 filter(data_grouped, year ==2012 & suburb == 'Carlton North'),
                                 filter(data_grouped, year ==2013 & suburb == 'Carlton North'),
                                 filter(data_grouped, year ==2014 & suburb == 'Carlton North'),
                                 filter(data_grouped, year ==2015 & suburb == 'Carlton North'),
                                 filter(data_grouped, year ==2016 & suburb == 'Carlton North'),
                                 filter(data_grouped, year ==2017 & suburb == 'Carlton North'),
                                 filter(data_grouped, year ==2018 & suburb == 'Carlton North'),
                                 filter(data_grouped, year ==2019 & suburb == 'Carlton North'),
          ), value,
mapping = aes(x = date_captured_month, y = avg_speed, 
              colour = factor(year))) + labs(x="Month", y="speed (km/h)", 
                                             colour="Year") + 
  ggtitle("AVERAGE SPEED IN Carlton North")
Carlton_North_speed = Carlton_North_speed + geom_line(linetype = "dashed") + geom_point() + theme_classic()
Carlton_North_speed = Carlton_North_speed + scale_x_discrete(limit = factor(c(1,2,3,4,5,6,7,8,9,10,11,12)))
Carlton_North_speed



Clifton_Hill_speed = ggplot(bind_rows(filter(data_grouped, year ==2009 & suburb == 'Clifton Hill'),
                                       filter(data_grouped, year ==2010 & suburb == 'Clifton Hill'),
                                       filter(data_grouped, year ==2011 & suburb == 'Clifton Hill'),
                                       filter(data_grouped, year ==2012 & suburb == 'Clifton Hill'),
                                       filter(data_grouped, year ==2013 & suburb == 'Clifton Hill'),
                                       filter(data_grouped, year ==2014 & suburb == 'Clifton Hill'),
                                       filter(data_grouped, year ==2015 & suburb == 'Clifton Hill'),
                                       filter(data_grouped, year ==2016 & suburb == 'Clifton Hill'),
                                       filter(data_grouped, year ==2017 & suburb == 'Clifton Hill'),
                                       filter(data_grouped, year ==2018 & suburb == 'Clifton Hill'),
                                       filter(data_grouped, year ==2019 & suburb == 'Clifton Hill'),
), value,
mapping = aes(x = date_captured_month, y = avg_speed, 
              colour = factor(year))) + labs(x="Month", y="speed (km/h)", 
                                             colour="Year") + 
  ggtitle("AVERAGE SPEED IN Clifton Hill")
Clifton_Hill_speed = Clifton_Hill_speed + geom_line(linetype = "dashed") + geom_point() + theme_classic()
Clifton_Hill_speed = Clifton_Hill_speed + scale_x_discrete(limit = factor(c(1,2,3,4,5,6,7,8,9,10,11,12)))
Clifton_Hill_speed

Collingwood_speed = ggplot(bind_rows(filter(data_grouped, year ==2009 & suburb == 'Collingwood'),
                                      filter(data_grouped, year ==2010 & suburb == 'Collingwood'),
                                      filter(data_grouped, year ==2011 & suburb == 'Collingwood'),
                                      filter(data_grouped, year ==2012 & suburb == 'Collingwood'),
                                      filter(data_grouped, year ==2013 & suburb == 'Collingwood'),
                                      filter(data_grouped, year ==2014 & suburb == 'Collingwood'),
                                      filter(data_grouped, year ==2015 & suburb == 'Collingwood'),
                                      filter(data_grouped, year ==2016 & suburb == 'Collingwood'),
                                      filter(data_grouped, year ==2017 & suburb == 'Collingwood'),
                                      filter(data_grouped, year ==2018 & suburb == 'Collingwood'),
                                      filter(data_grouped, year ==2019 & suburb == 'Collingwood'),
), value,
mapping = aes(x = date_captured_month, y = avg_speed, 
              colour = factor(year))) + labs(x="Month", y="speed (km/h)", 
                                             colour="Year") + 
  ggtitle("AVERAGE SPEED IN Collingwood")
Collingwood_speed = Collingwood_speed + geom_line(linetype = "dashed") + geom_point() + theme_classic()
Collingwood_speed = Collingwood_speed + scale_x_discrete(limit = factor(c(1,2,3,4,5,6,7,8,9,10,11,12)))
Collingwood_speed



Richmond_speed = ggplot(bind_rows(filter(data_grouped, year ==2009 & suburb == 'Richmond'),
                                     filter(data_grouped, year ==2010 & suburb == 'Richmond'),
                                     filter(data_grouped, year ==2011 & suburb == 'Richmond'),
                                     filter(data_grouped, year ==2012 & suburb == 'Richmond'),
                                     filter(data_grouped, year ==2013 & suburb == 'Richmond'),
                                     filter(data_grouped, year ==2014 & suburb == 'Richmond'),
                                     filter(data_grouped, year ==2015 & suburb == 'Richmond'),
                                     filter(data_grouped, year ==2016 & suburb == 'Richmond'),
                                     filter(data_grouped, year ==2017 & suburb == 'Richmond'),
                                     filter(data_grouped, year ==2018 & suburb == 'Richmond'),
                                     filter(data_grouped, year ==2019 & suburb == 'Richmond'),
), value,
mapping = aes(x = date_captured_month, y = avg_speed, 
              colour = factor(year))) + labs(x="Month", y="speed (km/h)", 
                                             colour="Year") + 
  ggtitle("AVERAGE SPEED IN Richmond")
Richmond_speed = Richmond_speed + geom_line(linetype = "dashed") + geom_point() + theme_classic()
Richmond_speed = Richmond_speed + scale_x_discrete(limit = factor(c(1,2,3,4,5,6,7,8,9,10,11,12)))
Richmond_speed



Fitzroy_North_traffic = ggplot(bind_rows(filter(data_grouped, year ==2009 & suburb == 'Fitzroy North'),
                                    filter(data_grouped, year ==2010 & suburb == 'Fitzroy North'),
                                    filter(data_grouped, year ==2011 & suburb == 'Fitzroy North'),
                                    filter(data_grouped, year ==2012 & suburb == 'Fitzroy North'),
                                    filter(data_grouped, year ==2013 & suburb == 'Fitzroy North'),
                                    filter(data_grouped, year ==2014 & suburb == 'Fitzroy North'),
                                    filter(data_grouped, year ==2015 & suburb == 'Fitzroy North'),
                                    filter(data_grouped, year ==2016 & suburb == 'Fitzroy North'),
                                    filter(data_grouped, year ==2017 & suburb == 'Fitzroy North'),
                                    filter(data_grouped, year ==2018 & suburb == 'Fitzroy North'),
                                    filter(data_grouped, year ==2019 & suburb == 'Fitzroy North'),
                ), value,
mapping = aes(x = date_captured_month, y = avg_volume, 
              colour = factor(year))) + labs(x="Month", y="traffic volume", 
                                             colour="Year") + 
  ggtitle("TRAFFIC VOLUME IN Fitzroy North")

Fitzroy_North_traffic = Fitzroy_North_traffic + geom_line(linetype = "dashed") + geom_point() + theme_classic()
Fitzroy_North_traffic = Fitzroy_North_traffic + scale_x_discrete(limit = factor(c(1,2,3,4,5,6,7,8,9,10,11,12)))
Fitzroy_North_traffic










  
ggplot(rbind(dat1,dat2), aes(month(date, label=TRUE, abbr=TRUE), 
                             value, group=factor(year(date)), colour=factor(year(date)))) +
  geom_line() +
  geom_point() +
  labs(x="Month", colour="Year") +
  theme_classic()
#https://stackoverflow.com/questions/41206181/ggplot-multiple-years-on-same-plot-by-month
