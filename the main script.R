setwd("C:/Users/Love you Allah/Desktop/Enviornmet/env") # set working directory
# activate the required libraries
library(haven)
library(foreign)
library(labelled)
library(dplyr)
library(Hmisc)
library(labelled)
library(haven)
library(ggplot2)
library(plotly)
library(expss)
library(car)
library(carData)

# read the data file
df<- read_dta("data.DTA")
View(df)

# 1st Task (draw a random sample at least 5000)
sample_df_mask <- sample(x=nrow(df),size=9000,replace=F) 
sample_df <- df[sample_df_mask,]
# 2nd Task summery statistics for the household family members
View(lookfor(data=sample_df,"age of"))
View(sample_df[,"hv105"])

### hv105 -> is the column  containing  the Age of household members
### becasue of this note "c(`95+` = "95", `don't know` = "98")"
### we will filter all vales of 98 in sample_df

#method 1
temp_mask=sample_df ["hv105"] !=98  # build a mask based on col
sample_df=sample_df[temp_mask,]   # apply the mask on rows
Age_summary_statistics=summary(sample_df$hv105) # calculating the summary stats
Age_summary_statistics
#method 2
sample_df %>%
  filter("hv105"!=98) %>%
  pull("hv105") %>%
  summary()

##########  3rd Task Draw a histogram ##########   

#method 1
hist(sample_df$hv105,
     breaks=25,
     main="Distribution of Household members age",
     xlab="Age ranges",
     ylab="Frequency",
     col="pink",
     border="black"
)

#method 2 by using plotly

# Clean data to remove NA values
temp_mask=!is.na(sample_df$hv105)
sample_df=sample_df[temp_mask,]
# to be sure that the data is numeric class not anything else a factor
sample_df$hv105<- sample_df%>%
  pull("hv105")%>%
  as.character()%>%
  as.numeric()

# Plot histogram using plot_ly
plot_ly(x = sample_df$hv105,
        type = "histogram", 
        xbins = list(size = 6),
        marker = list(color="green")
)%>%layout(
  title = "Distribution of Household members age", 
  xaxis = list(title = "Value", tickmode= "auto"),
  yaxis = list(title = "Frequency"),
  bargap = 0.02)

########## 3rd Task compute Aging proportion,DDR,OADR and aging index##########  

#getting the required values 
old_pop=sample_df     %>%
  filter(hv105 >= 65) %>%
  pull("hv105")       %>%
  length()

total_pop=  sample_df    %>%
  pull("hv105")          %>%
  length()  

young_pop=nrow(sample_df[sample_df[,"hv105"]<=14,])
work_age=nrow(sample_df[sample_df[,"hv105"]>=15 & sample_df[,"hv105"]<= 64,])

# Note:multiplied by 100 to for easy reading

# Aging proportion 
Aging_proportion=(old_pop/total_pop)*100
#DDR
DDR=((young_pop+old_pop)/work_age)*100
#OADR
OADR=(old_pop/work_age)*100
#Aging index
Aging_index=(old_pop/young_pop)*100


Aging_proportion
DDR
OADR
Aging_index

##########  4th Task use recode and factor functions ########## 


sample_df$age_cat <- car::recode(sample_df$hv105,
                                 "0:14 = 1; 15:64 = 2; 65:95 = 3")
sample_df$age_cat <- factor(sample_df$age_cat,
                            levels = c(1, 2, 3),
                            labels = c("pop < 14", "pop 15-64", "pop > 65"))
sample_df$age_cat

##########  5th Task weighted tabulation of the age category ##########

View(lookfor(sample_df,"weight")) # the column name = hv005

sample_df$weight <- sample_df$hv005/1000000

sample_df %>% 
  tab_cells(age_cat) %>%
  tab_weight(weight)%>%
  tab_stat_cases(total_statistic = "w_cases")%>%
  tab_stat_cpct(total_statistic = "w_cases")%>%
  tab_pivot()%>%
  tab_caption("The weighted tabulation age catagory")

save(sample_df, file = "C:/Users/Love you Allah/Desktop/Enviornmet/env/sample_df.rdata")
