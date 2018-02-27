#Load packages
library('nFactors')
library('psych')
library('GPArotation')
library('nnet')
library('car')
library('ggplot2')
library('reshape2')

#Read in data
df_conf<-read.csv("C:/Users/julie.wisch/Research3ActMath/ConfPost_1011.csv")  ##Paste the path and name of the file between the "" and change \ to /
df_amars<-read.csv("C:/Users/julie.wisch/Research3ActMath/AMARSPost_1011.csv")


#--------Setting up the Confidence Pretest---------------------------------#
#Massage data
#Changing column names
names(df_conf)            #Looks at column names and numbers
df_conf[c(1)]<-NULL  #Drop timestamp column

colnames(df_conf)<-c("name", "q1", "q2_rev", "q3_rev", "q4",
                "q5_rev", "q6", "q7", "q8", "q9_rev")  ##   #Rename all the the columns. Short and without capitolized letters is easiest

#q1 - I usually do well in mathematics.
#q2 - Mathematics is more difficult for me than most of my classmates
#q3 - Mathematics is not one of my strengths
#q4 - I learn things quickly in mathematics
#q5 - Mathematics makes me confused and nervous
#q6 - I am good at working out difficult mathematics problems
#q7 - My teacher thinks I can do well in mathematics classes with difficult materials
#q8 - My teacher tells me I am good at mathematics
#q9 - Mathematics is harder for me than any other subject

#-2 demonstrates the least confidence in math, +2 demonstrates the most confidence
df_conf[,2] <- sapply(df_conf[,2],switch,"Disagree a lot"=-2,"Disagree a little"=-1, "Agree a little"=1, "Agree a lot"=2)
df_conf[,3] <- sapply(df_conf[,3],switch,"Disagree a lot"=2,"Disagree a little"=1, "Agree a little"=-1, "Agree a lot"=-2)
df_conf[,4] <- sapply(df_conf[,4],switch,"Disagree a lot"=2,"Disagree a little"=1, "Agree a little"=-1, "Agree a lot"=-2)
df_conf[,5] <- sapply(df_conf[,5],switch,"Disagree a lot"=-2,"Disagree a little"=-1, "Agree a little"=1, "Agree a lot"=2)
df_conf[,6] <- sapply(df_conf[,6],switch,"Disagree a lot"=2,"Disagree a little"=1, "Agree a little"=-1, "Agree a lot"=-2)
df_conf[,7] <- sapply(df_conf[,7],switch,"Disagree a lot"=-2,"Disagree a little"=-1, "Agree a little"=1, "Agree a lot"=2)
df_conf[,8] <- sapply(df_conf[,8],switch,"Disagree a lot"=-2,"Disagree a little"=-1, "Agree a little"=1, "Agree a lot"=2)
df_conf[,9] <- sapply(df_conf[,9],switch,"Disagree a lot"=-2,"Disagree a little"=-1, "Agree a little"=1, "Agree a lot"=2)
df_conf[,10] <- sapply(df_conf[,10],switch,"Disagree a lot"=2,"Disagree a little"=1, "Agree a little"=-1, "Agree a lot"=-2)

#--------Setting up the AMARS Pretest---------------------------------#

names(df_amars)            #Looks at column names and numbers
df_amars[c(1)]<-NULL  #Drop timestamp column

colnames(df_amars)<-c("name", "q1", "q2", "q3", "q4",
                     "q5", "q6", "q7", "q8", "q9", "q10",
                     "q11", "q12", "q13", "q14", "q15", "q16",
                     "q17", "q18", "q19", "q20", "q21", "q22",
                     "q23", "q24", "q25")

#-5 indicates the most anxiety, 0 indicates no anxiety
#Need to make this flip through all the columns
for (i in 2:26){
df_amars[,i] <- sapply(df_amars[,i],switch,"Very much anxiety"=-5,"Much anxiety"=-4, "A fair amount of anxiety"=-3, "A little anxiety"=-1, "No anxiety at all"=0)}

#----actual coding begins here---------------------------------#
df_conf$mean <- rowMeans(df_conf[,2:10]) #Calculates mean for individual students on confidence assessment
df_amars$mean <- rowMeans(df_amars[,2:25]) #Calculates mean for individual students on confidence assessment

df = data.frame(df_conf$name, df_conf$mean, df_amars$name, df_amars$mean)

write.csv(df, file="C:/Users/julie.wisch/Research3ActMath/summarydata_1011.csv")

#conduct manual data checks, performing any necessary adjustments 
#this reads in cleaned up data

df_data<-read.csv("C:/Users/julie.wisch/Research3ActMath/summarydata_cleaned.csv")
plot(df_data$Pre_Mean, df_data$Post_Mean)  #there doesn't appear to be any correlation between conf and anxiety...I would have thought high confidence = low anxiety
plot(df_data$pre_amars_mean, df_data$post_amars_mean)  #pretty strong correlation between pre and post anxiety and confidence.  kids didn't change much

t.test(df_data$Pre_Mean, df_data$Post_Mean, paired = TRUE) #p > 0.05 means we accept null
t.test(df_data$pre_amars_mean, df_data$post_amars_mean, paired = TRUE) #p > 0.05 means we accept null
t.test(df_data$TimetoSolunPost, df_data$TimeToSolunPre, paired = TRUE) #p < 0.05, reject null.  Kids spent significantly more time on solving!!!!



m.perf<-lm(df_data$Post_Mean~Race2+Gender+TimeToSolunPre*TimetoSolunPost+ClassGrade+ASPIRE, data = df_data)
m.perf<-lm(df_data$Post_Mean~TimetoSolunPost + ASPIRE, data = df_data)

Anova(m.perf, Type=2)
summary(m.perf)
coefficients(m.perf)
m.perf<-lm(df_data$Post_Mean~TimetoSolunPost, data = df_data)


plot(df_data$ASPIRE, df_data$Post_Mean, pch = 16, cex = 1.3, col = "black", xlab = "ASPIRE Score", ylab = "Mathematical Practices Posttest Score")
abline(lm(df_data$Post_Mean~df_data$ASPIRE))

plot(df_data$TimetoSolunPost, df_data$Post_Mean, pch = 16, cex = 1.3, col = "black", xlab = "Solution Time (mins)", ylab = "Mathematical Practices Posttest Score")
abline(lm(df_data$Post_Mean~df_data$TimetoSolunPost))

m.conf<-lm(df_data$post_conf_mean~Race2+Gender+ClassGrade+ASPIRE, data = df_data)
Anova(m.conf)
m.amars<-lm(df_data$post_amars_mean~Race2+Gender+ClassGrade+ASPIRE, data = df_data)
Anova(m.amars)

df_graph<-read.csv("C:/Users/julie.wisch/Research3ActMath/summarydata_tograph3.csv")

library(reshape2)
df_graph<-melt(df_graph, id.vars=c("name"))
write.csv(df_graph, file="C:/Users/julie.wisch/Research3ActMath/tograph3.csv")

df_graph<-read.csv("C:/Users/julie.wisch/Research3ActMath/tograph2.csv")

tapply(X = df_graph$value, INDEX = list(df_graph$names), FUN = mean)
pairwise.t.test(df_graph$value, df_graph$names, p.adj = "none")

pairwise.t.test(value, names, p.adj= "bonferroni")

p <-ggplot(df_graph, aes(x=Practice, y=Score)) + geom_jitter(height = 0.1, width = .25) +   scale_shape_manual(values=c(2)) + theme_classic()
p + xlab("Mathematical Practice") + ylab ("Student Score Differential") + geom_hline(yintercept=0)

#scale_fill_manual to adjust colors
