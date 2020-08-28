# Clean data from approval_pollist.csv and
# approval_topline.csv

# Packages: ggplot2, reshape2, plyr

# load in required packages if not selecting check box in R Studio
library(ggplot2)
library(plyr)
library(reshape2)


# Set the current working directory
setwd('C:/rstuff/projects/dt_approval/')


# Make function for spacing
spacing = function() {
  strrep('-',70)
}

# Load the datasets
poll_list = read.csv('approval_polllist.csv')
topline = read.csv('approval_topline.csv')


#DATA SUMMARY-----------------------------------------------------------------------


# Print head
head(poll_list)
spacing()
head(topline)

# Print a summary
summary(poll_list)
spacing()
summary(topline)

# Print dimensions
dim(poll_list)
spacing()
dim(topline)

# Print column names
names(poll_list)
spacing()
names(topline)


#DATA COPIES -------------------------------------------------------------------------------------


# Creat dataset copies for testing purposes
pl_copy = data.frame(poll_list)
tl_copy = data.frame(topline)
head(pl_copy)
spacing()
head(tl_copy)

# Check memory addresses to ensure original and copy are different
tracemem(pl_copy)==tracemem(poll_list)
tracemem(tl_copy)==tracemem(topline)


#DATA EXPLORATION -------------------------------------------------------------------------------


# Variables for unique column values for poll_list
c_subgroup = unique(poll_list$subgroup)
c_pollster = unique(poll_list$pollster)
c_grade = unique(poll_list$grade)
c_population = unique(poll_list$population)

# Print the variables and length
print(c_subgroup)
print(length(c_subgroup))
print(c_pollster)
print(length(c_pollster))
print(c_grade)
print(length(c_grade))
print(c_population)
print(length(c_population))

# topline only has subgroup categorical column. Identical to poll_list subgroup.

# Check total NA values
sum(is.na(pl_copy))
sum(is.na(tl_copy))


#GRADE AND N/A CLEANING -----------------------------------------------------------------------------


# create new dataframe with desired columns from pl_copy
pollster_data = pl_copy[,c(6,7,12,13,14,15)]
head(pollster_data)

# create a factor for grades
pollster_data$grade = factor(pollster_data$grade, levels = c('A+', 'A', 'A-', 'A/B', 'B+', 'B',
                                                             'B-', 'B/C', 'C+', 'C', 'C-', 'C/D',
                                                             'D+', 'D', 'D-'))

# order pollster_data by grade (descending order)
pollster_data = pollster_data[order(pollster_data$grade),]
head(pollster_data)
tail(pollster_data)
summary(pollster_data)

# omit rows with grade NA values
pollster_data = pollster_data[complete.cases(pollster_data[,2]),]
head(pollster_data)
tail(pollster_data)
summary(pollster_data)


#ADJUSTED APPROVE CLEAN/RESHAPE ----------------------------------------------------------------------------------------

# create factor of pollster and get adjusted_approve averages
pollster_avg_app = aggregate(pollster_data[,5], list(pollster_data$pollster, pollster_data$grade), mean)
head(pollster_avg_app)
tail(pollster_avg_app)

# sort averages by highest adjusted approve
sorted_avg = data.frame(pollster_avg_app)
sorted_avg = sorted_avg[order(sorted_avg$x, decreasing = TRUE),]
head(sorted_avg)
tail(sorted_avg)

# rename sorted_avg columns
sorted_avg = rename(sorted_avg, c("Group.1"="Pollster", "Group.2"="Grade", "x"="Adjusted_Approve_AVG"))


# make pollster index/row names
rownames(sorted_avg) = sorted_avg$Pollster

# remove the pollster colummn
sorted_avg$Pollster = NULL




#ADJUSTED DISAPPROVE CLEAN/RESHAPE ---------------------------------------------------------------------------------------

# create factor of pollster and get adjusted_disapprove averages
pollster_avg_dis = aggregate(pollster_data[,6], list(pollster_data$pollster, pollster_data$grade), mean)
head(pollster_avg_dis)
tail(pollster_avg_dis)

# sort averages by highest adjusted disapprove
sorted_avg2 = data.frame(pollster_avg_dis)
sorted_avg2 = sorted_avg2[order(sorted_avg2$x, decreasing = TRUE),]
head(sorted_avg2)
tail(sorted_avg2)

# rename sorted_avg2 columns
sorted_avg2 = rename(sorted_avg2, c("Group.1"="Pollster", "Group.2"="Grade", "x"="Adjusted_Disapprove_AVG"))

# make pollster index/row names
rownames(sorted_avg2) = sorted_avg2$Pollster

# remove the pollster column
sorted_avg2$Pollster = NULL


#ADJUSTED AVERAGES DATA AND DF REMOVAL --------------------------------------------------------------------------------
total_avg = merge(sorted_avg, sorted_avg2, by=0, all=TRUE)
head(total_avg)
tail(total_avg)

# drop Grade.y since it is identical to Grade.x and rename Grade.x
total_avg$Grade.y = NULL
total_avg = rename(total_avg, c("Grade.x"="Grade"))

# rename row.names and make index, remove pollster column
total_avg = rename(total_avg, c("Row.names"="Pollster"))
rownames(total_avg) = total_avg$Pollster
total_avg$Pollster = NULL
head(total_avg)

# create a copy of total_avg sorted by grade
total_avg_by_grade = data.frame(total_avg)
total_avg_by_grade = total_avg_by_grade[order(total_avg_by_grade$Grade),]
head(total_avg_by_grade)
tail(total_avg_by_grade)

# delete data frames and factors no longer needed
rm(list = c("pl_copy", "poll_list", "pollster_avg_app", "pollster_avg_dis", "pollster_data", "sorted_avg",
            "sorted_avg2", "tl_copy", "topline", "c_grade", "c_pollster", "c_population", "c_subgroup"))

# summary of total_avg and total_avg_by_grade (SHOULD BE IDENTICAL)
summary(total_avg)
summary(total_avg_by_grade)


#DATA OUTPUT -----------------------------------------------------------------------------------------------

# output total_avg and total_avg_by_grade as csv files to cwd
write.csv(total_avg, 'approval_averages.csv', row.names = TRUE)
write.csv(total_avg_by_grade, 'averages_by_grade.csv', row.names = TRUE)



# create a copy of pollster_data to sort by approve (decreasing) BELOW IS REFERENCE
# approval_data = data.frame(pollster_data)
# approval_data = approval_data[order(approval_data$adjusted_approve, decreasing = TRUE),]
# head(approval_data)