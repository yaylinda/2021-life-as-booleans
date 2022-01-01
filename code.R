library(ggplot2)
library(zoo)
library(lubridate)
library(reshape2)
library(scales)
library(ggthemes)

setwd("~/Developer/2021-life-as-booleans")

#######################################
# READ / CLEAN/ FORMAT DATA
#######################################

data = read.csv("data.csv")

data[data==0] = "False"
data[data==1] = "True"

data$date = as.Date(data$X.1, format = "%m/%d/%y")
data$week = week(data$date)
data$month = format(data$date,"%B")
data$month = factor(data$month, list(
  "January", "February", "March", 
  "April", "May", "June", 
  "July", "August", "September", 
  "October", "November", "December"
))
data$yearmonth = as.yearmon(data$date)
data$yearmonthf = factor(data$yearmonth)
data$day_of_week = factor(data$X, list(
  "Sunday", 
  "Monday", 
  "Tuesday", 
  "Wednesday", 
  "Thursday", 
  "Friday", 
  "Saturday"
))

# calculate week of month, where sunday is new week
weekofmonth = rep(0, length(data$date))
weekNum = 1
currentMonth = 1
for (i in (1:length(data$date))) {
  date = data$date[i]
  if (weekdays(date) == "Sunday") {
    weekNum = weekNum + 1
  }
  if (month(date) > currentMonth) {
    currentMonth = currentMonth + 1
    weekNum = 1
  }
  weekofmonth[i] = weekNum
}
data$monthweek = weekofmonth
data$monthweek = factor(data$monthweek, list(6, 5, 4, 3, 2, 1))

#######################################
# DEFINE VARIABLES LABELS
#######################################

variable_labels = c(
  `felt.enough.sleep` = "Enough\nSleep",
  `slept.at.home` = "Slept at Home",
  `shower` = "Shower",
  `wash.hair` = "Wash Hair", # NEW IN 2021
  `toothbrush.morning` = "Brush Teeth Morning",
  `toothbrush.night` = "Brush Teeth Night",
  `poop` = "Poop",
  `breakfast` = "Break-\nfast",
  `lunch` = "Lunch",
  `dinner` = "Dinner",
  `period` = "Period",
  `ate.at.home` = "Eat In", # NEW IN 2021
  `cooked` = "Cooked", # NEW IN 2021
  `ate.at.restaurant` = "Eat Out", # NEW IN 2021
  `deliver...pick.up.food` = "Delivery", # NEW IN 2021
  `midnight.snack` = "Midnight Snack", # NEW IN 2021
  `birth.control` = "Birth Control",
  `anti.depressant` = "Anti-Depressant",
  `vitamins` = "Vitamins",
  `advil` = "Advil",
  `excederin` = "Excederin",
  `adderall` = "Adderall",
  `caffeine` = "Caffeine",
  `ambien` = "Ambien",
  `alcohol` = "Alcohol",
  `weed` = "Weed",
  `chores` = "Chores",
  `personal.coding` = "Personal\nCoding",
  `went.to.work` = "Went to\nWork",
  `worked.remotely` = "WFH",
  `sad.about.work` = "Sad about Work",
  `happy.about.work` = "Happy about Work",
  `productive.at.work` = "Productive at Work",
  `happy` = "Happy",
  `sad` = "Sad",
  `angry` = "Angry",
  `stressed.or.anxious` = "Felt\nStressed",
  `Annoyed` = "Annoyed",
  `Journaled` = "Journaled",
  `cried` = "Cried",
  `happy.about.relationship` = "Happy about Relationship",
  `sad.about.relationship` = "Sad about Relationship",
  `sex` = "Sex",
  `abandoned.by.sean` = "Abandoned",
  `had.own.activities..other.than.work` = "",
  `upset.at.sean` = "Upset at Sean",
  `sean.cigar` = "Sean Cigar", # NEW IN 2021
  `sean.drunk` = "Sean Drunk",
  `sean.alcohol` = "Sean Alcohol",
  
  `January` = "Jan", 
  `February` = "Feb", 
  `March` = "Mar", 
  `April` = "Apr", 
  `May` = "May", 
  `June` = "Jun", 
  `July` = "Jul", 
  `August` = "Aug", 
  `September` = "Sep", 
  `October` = "Oct", 
  `November` = "Nov", 
  `December` = "Dec"
)

#######################################
# DEFINE DATA SUBSETS & PLOT
#######################################

#--------------------------------------
# All
#--------------------------------------


#--------------------------------------
# All (SFW)
#--------------------------------------


#--------------------------------------
# Hygeine
#--------------------------------------


#--------------------------------------
# Eating
#--------------------------------------


#--------------------------------------
# Drugs
#--------------------------------------


#--------------------------------------
# Work
#--------------------------------------


#--------------------------------------
# Feelings 
#--------------------------------------



#--------------------------------------
# Relationship 
#--------------------------------------


#######################################
# HELPER FUNCTION: melt_and_plot()
#######################################

melt_and_plot = function(data, subtitle) {
  melt = melt(
    data = data, 
    id = names(data)[1:4])
  
  ggplot(
    melt, 
    aes(
      day_of_week, 
      monthweek, 
      fill = as.factor(value)
    )
  ) + 
    coord_equal(ratio = 1) + 
    geom_tile(color = "white") + 
    facet_grid(
      variable ~ month, 
      switch = "y", 
      space = "free", 
      labeller = as_labeller(variable_labels)
    ) +
    labs(
      y = "",
      x = "",
      title = "2021 in Booleans",
      subtitle = subtitle,
      fill = "Legend",
    ) + 
    theme(
      text = element_text(family = "mono", color = "white"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(), 
      axis.ticks = element_blank(),
      plot.title = element_text(size = 60, face = "bold", margin = margin(b = 40)),
      # plot.subtitle = element_text(size = 40, margin = margin(t = 20, b = 40)),
      plot.subtitle = element_blank(),
      plot.caption = element_text(size = 30, margin = margin(t = 30, b = 20), hjust = 0),
      strip.text.x = element_text(size = 25, face = "bold"),
      strip.text.y = element_text(size = 20, face = "bold"),
      legend.title = element_text(size = 30, face = "bold"),
      legend.text = element_text(size = 30),
      legend.box.margin = margin(l = 40),
      legend.background = element_rect(fill = "black"),
      plot.margin = margin(t = 50, r = 80, b = 20, l = 80),
      plot.background = element_rect(fill = "black")
    )
}
