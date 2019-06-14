# cricketdistributions
A few nifty scripts for calculating the the distribution of peak performance in Cricket

# As you can probably ascertain from the above, the purpose of this script is to provide budding cricket analysts with the facility to calculate the mean age distributions, for peak batting performance.
This may seem fairly niche, but there is a demmand for this form of analytics within the sporting world, so you can apply the logic, code provided to any sport or different disciplines within the sport of cricket.
The example given is from all England ODI data all of which is being pulled from the ESPN Cricinfo API, as seen on the first Googly Analytics post about peak performance.
You will  need to download a copy of the dataset to your local drive:

https://www.kaggle.com/raghav333/espn-cricket-players-data

Save this file as cricket_data, in order for the below logic to work. You're good to go; now please follow the below steps:

# Install relevant packages

install.packages(lubridate)
install.packages(ggplot2)
install.packages(dplyr)
install.packages(ggstatsplot)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggstatsplot)

# Pull players from API to dual dataset

playerid <- 
  cricket_data %>%
  select(ID)
View(playerid)

# Pull players from API to dual dataset

battingData<- data.frame()

playerid = c(17121,24598,53904,53818,17025,15917,16320,358259,15901,16951,16281,16274,230855,16262,16260,16932,16925,249934,16899,16203,469888,16885,17944,16178,16876,297488,16975,18023,8590,53897,8581,250089,8566,8608,8526,8520,8917,8476,18389,8501,18256,8487,215501,308967,53906,10617,9310,9208,9187,9163,10582,9121,9117,53895,297628,9089,9069,9062,297074,9042,550235,297085,10881,232438,414990,10870,297433,8954,10839,10816,10809,53691,10772,11724,10653,10633,11931,11893,12461,11870,12454,14325,11865,211855,14302,297086,11974,14246,14244,14236,14187,244639,14159,53800,646211,14152,300028,14127,297079,13463,12514,297036,53709,12507,12923,12909,12490,12486,469886,12879,12877,878039,13418,13414,13411,12466,12856,13399,14054,12854,12850,53892,210283,13368,13365,879369,13340,249866,12803,18675,18627,288992,15555,15403,19264,15383,15385,15380,515874,15522,19296,15485,53704,15461,18632,20431,18655,20387,457279,311158,20372,20286,20282,20278,20263,20259,19500,297075,298438,20217,303669,20193,20187,254238,21431,20123,19394,19374,19364,515905,19346,244497,19327,19314,20051,21611,21607,21598,47623,21585,21650,21646,461632,21548,21537,297635,249935,21494,21486,21466,254168,23460,22520,351588,22498,247235,466619,22462,22442,308251,22403,22380,22365,212709,22333,509107,53890,22149,22144,296597,22182,23523)

for(i in playerid)  {
  playerInfo <- cricketdata::fetch_player_data(i, "odi")
  playerInfo$id <- i
  battingData <- rbind(battingData, playerInfo)
}

YOB <- cricket_data %>% select("ID","YOB")

battingData <- inner_join(battingData,YOB, by = c("id"="ID"))

View(battingData)

# Turn into year not start-date

library(lubridate)
date <- c(battingData$Start_Date)
battingData$yearofgame <- year(as.Date(date,"%d,%m,%Y"))
battingData$age <- battingData$yearofgame-as.numeric(battingData$YOB)
View(battingData)

# Mutate and grab peak ages

newbattingData <- na.omit(battingDATA)

newbattingData <- newbattingData %>% 
  group_by(id, age) %>% 
  mutate(av = sum(Runs)/n()) %>% 
  group_by(id) %>% 
  mutate(maxAv = max(av)) %>% 
  filter(av == maxAv) %>% 
  select(id, age) %>% 
  group_by(id, age) %>% 
  summarise()
View(newbattingData)


# Plot 
ggstatsplot::gghistostats(
  data = England_ODI_two, # dataframe from which variable is to be taken
  x = age, # numeric variable whose distribution is of interest
  title = "Distribution of Peak Batting Average - (All England ODI Data - 1950-2019)", # title for the plot
  fill.gradient = TRUE, # use color gradient
  type = "bf",
  normal.curve = TRUE,
  normal.curve.color = "red",
  xlab = "Age at Peak Performance (Runs/Inns)", # bayes factor for one sample t-test
  bf.prior = 0.8, # prior width for calculating the bayes factor
  messages = FALSE # turn off the messages
)



