# Visualize data from approval_averages and averages_by_grade

# Packages: ggplot2, plyr, dplyr, tidyr


#SETUP -----------------------------------------------------------------------------------------

# load required packages
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)

# set wd
setwd('c:/rstuff/projects/dt_approval/')

# read in csv files
averages = read.csv('approval_averages.csv')
averages_by_grade = read.csv('averages_by_grade.csv')

# summary of both (SHOULD BE IDENTICAL)
summary(averages)
summary(averages_by_grade)

# rename column X
averages = rename(averages, c("Pollster"="X"))
averages_by_grade = rename(averages_by_grade, c("Pollster"="X"))


#DIVERGING LOLLIPOP (APPROVE) -----------------------------------------------------------------------------------------------

# create copy of averages
avg_copy = data.frame(averages)

# set rownames
avg_copy$Pollster = rownames

# compute normalized adjusted_approve_avg
avg_copy$Adjusted_Approve_Z = round((avg_copy$Adjusted_Approve_AVG - mean(avg_copy$Adjusted_Approve_AVG))
                                    /sd(avg_copy$Adjusted_Approve_AVG), 2)

# above/below flag (for potential diverging bar)
avg_copy$Average_Rank = ifelse(avg_copy$Adjusted_Approve_Z < 0, "Below", "Above")

# order by Adjusted_Approve_Z
avg_copy = avg_copy[order(avg_copy$Adjusted_Approve_Z),]

# make pollster factor
avg_copy$Pollster = factor(avg_copy$Pollster, levels = avg_copy$Pollster)

# create plot
ggplot(avg_copy, aes(x=Pollster, y=Adjusted_Approve_Z, label=Adjusted_Approve_Z))+
  geom_point(stat='identity', fill='black', size=7)+
  geom_segment(aes(y=0, x=Pollster, yend=Adjusted_Approve_Z, xend=Pollster), color='black')+
  geom_text(color='white', size=2)+
  labs(title='Adjusted Approval Comparison', subtitle='Normalized Mean Adjusted Approval from approval_averages.csv')+
  ylim(-3, 3)+
  coord_flip()



#DIVERGING LOLLIPOP (DISAPPROVE) ----------------------------------------------------------------------------------------

# create copy of averages
avg_copy2 = data.frame(averages)

# set rownames
avg_copy2$Pollster = rownames

# compute normalized adjusted_disapprove_avg
avg_copy2$Adjusted_Disapprove_Z = round((avg_copy2$Adjusted_Disapprove_AVG - mean(avg_copy2$Adjusted_Disapprove_AVG))
                                        /sd(avg_copy2$Adjusted_Disapprove_AVG), 2)

# above/below flag (for potential diverging bar)
avg_copy2$Average_Rank = ifelse(avg_copy2$Adjusted_Disapprove_Z < 0, "Below", "Above")

# order by Adjusted_Disapprove_Z
avg_copy2 = avg_copy2[order(avg_copy2$Adjusted_Disapprove_Z),]

# make pollster factor
avg_copy2$Pollster = factor(avg_copy2$Pollster, levels = avg_copy2$Pollster)

# create plot
ggplot(avg_copy2, aes(x=Pollster, y=Adjusted_Disapprove_Z, label=Adjusted_Disapprove_Z))+
  geom_point(stat='identity', fill='black', size=7)+
  geom_segment(aes(y=0, x=Pollster, yend=Adjusted_Disapprove_Z, xend=Pollster), color='black')+
  geom_text(color='white', size=2)+
  labs(title='Adjusted Disapproval Comparison', subtitle='Normalized Mean Adjusted Disapproval from approval_averages.csv')+
  ylim(-3, 5)+
  coord_flip()


#ORDERED BAR (APPROVE BY GRADE) ---------------------------------------------------------------------------

# create copy of averages_by_grade
grade_avg = data.frame(averages_by_grade)

# group mean adjusted approve by grade
Graded_Adjusted_Approve = aggregate(grade_avg$Adjusted_Approve_AVG, by=list(grade_avg$Grade),
                                    FUN=mean)

# rename columns
colnames(Graded_Adjusted_Approve) = c("Grade", "Adjusted_Approval_Average")

# sort by Adjusted_Approval_Average
Graded_Adjusted_Approve = Graded_Adjusted_Approve[order(Graded_Adjusted_Approve$Adjusted_Approval_Average),]

# make grades a factor to retain order
Graded_Adjusted_Approve$Grade = factor(Graded_Adjusted_Approve$Grade, levels=Graded_Adjusted_Approve$Grade)

# create plot
ggplot(Graded_Adjusted_Approve, aes(x=Grade, y=Adjusted_Approval_Average))+
  geom_bar(stat='identity', width=0.5, fill='springgreen4')+
  scale_y_continuous(breaks=seq(0,100,10))+
  labs(title="Average Adjusted Approval by Grade",
       subtitle="Ordered from lowest to highest")


#ORDERED BAR (DISAPPROVE BY GRADE) -------------------------------------------------------------------------

# group mean adjusted disapprove by grade
Graded_Adjusted_Disapprove = aggregate(grade_avg$Adjusted_Disapprove_AVG, by=list(grade_avg$Grade),
                                       FUN=mean)

# rename columns
colnames(Graded_Adjusted_Disapprove) = c("Grade", "Adjusted_Disapproval_Average")

# sort by Adjusted_Disapproval_Average
Graded_Adjusted_Disapprove = Graded_Adjusted_Disapprove[order(Graded_Adjusted_Disapprove$Adjusted_Disapproval_Average),]

# make grades a factor to retain order
Graded_Adjusted_Disapprove$Grade = factor(Graded_Adjusted_Disapprove$Grade, levels=Graded_Adjusted_Disapprove$Grade)

# create plot
ggplot(Graded_Adjusted_Disapprove, aes(x=Grade, y=Adjusted_Disapproval_Average))+
  geom_bar(stat='identity', width=0.5, fill='red3')+
  scale_y_continuous(breaks=seq(0,100,10))+
  labs(title="Average Adjusted Disapproval by Grade",
        subtitle="Ordered from lowest to highest")


#MULTIPLE VARIABLE BAR (BY GRADE) ------------------------------------------------------------------------------------

# merge Graded_Adjusted_Approve and Graded_Adjusted_Disapprove
Graded_Totals = merge(Graded_Adjusted_Approve, Graded_Adjusted_Disapprove, by=1, all=TRUE)

# make grade a factor
Graded_Totals$Grade = factor(Graded_Totals$Grade, levels=c('A+', 'A', 'A-', 'A/B', 'B+', 'B',
                                                           'B-', 'B/C', 'C+', 'C', 'C-', 'C/D',
                                                           'D+', 'D', 'D-'))
# order by grade
Graded_Totals = Graded_Totals[order(Graded_Totals$Grade),]

# 'gather' the data into a new variable
Gathered_Totals = Graded_Totals %>%
  gather("Key", "Adjusted_Average", -Grade)
  
# create plot
ggplot(Gathered_Totals, aes(x=Grade, y=Adjusted_Average, fill=Key))+
  scale_fill_manual(values=c("springgreen4", "red3"))+
  scale_y_continuous(breaks=seq(0,50,10))+
  geom_col(position="dodge", width=0.5)+
  labs(title="Adjusted Averages Comparison",
       subtitle="Ordered by Grade (descending)")







