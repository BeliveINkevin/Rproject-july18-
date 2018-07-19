library(readr)
df <- read_csv("C:/Documents/My Excel/avocado.csv")
View(df)
avocado <- df[,-c(1,5:7)]  # I delete the column 1 because of R wanted to fill in the missing column names with 'x1' [1]
#Also, columns representing "4046","4225",and"4770" are deleted because I feel that Product lookup codes
#are not significant in the calculations since many avocados can vary in size/quality but might have the same PLU as other avocados
#including PLU's in the calculations can cause some ambiguity

#First step: I will filter the data by year. Since this data displays results from
#year 2015 thru 2018, I will filter out each year
install.packages("dplyr")

library(dplyr)
#Note the C next to avoYYYY stands for conventional and the O next to avoYYYY stands for organic
#I'm basically filtering it by year and splitting it based on whether its organic or conventional
avo2015<-dplyr::filter(avocado,year==2015)
avo2015C<-dplyr::filter(avocado,year==2015 & type=="conventional")
avo2015O<-dplyr::filter(avocado,year==2015 & type=="organic")
avo2016<-dplyr::filter(avocado,year==2016)
avo2016C<-dplyr::filter(avocado,year==2016 & type=="conventional")
avo2016O<-dplyr::filter(avocado,year==2016 & type=="organic")
avo2017<-dplyr::filter(avocado,year==2017)
avo2017C<-dplyr::filter(avocado,year==2017 & type=="conventional")
avo2017O<-dplyr::filter(avocado,year==2017 & type=="organic")
avo2018<-dplyr::filter(avocado,year==2018)
avo2018C<-dplyr::filter(avocado,year==2018 & type=="conventional")
avo2018O<-dplyr::filter(avocado,year==2018 & type=="organic")

#unique(avoYYYY$region)  will list the distinct regions in the particular year
aggregate(avo2015[, 2:3], list(avo2015$region), mean) #This function gives the region with the "average" of the average price
#and the "average" of the total volumn sold for avocados during the entire year
aggregate(avo2015C[, 2:3], list(avo2015C$region), mean)
aggregate(avo2015O[, 2:3], list(avo2015O$region), mean)

aggregate(avo2016[, 2:3], list(avo2016$region), mean)
aggregate(avo2016C[, 2:3], list(avo2016C$region), mean)
aggregate(avo2016O[, 2:3], list(avo2016O$region), mean)

aggregate(avo2017[, 2:3], list(avo2017$region), mean)
aggregate(avo2017C[, 2:3], list(avo2017C$region), mean)
aggregate(avo2017O[, 2:3], list(avo2017O$region), mean)

aggregate(avo2018[, 2:3], list(avo2018$region), mean)
aggregate(avo2018C[, 2:3], list(avo2018C$region), mean)
aggregate(avo2018O[, 2:3], list(avo2018O$region), mean)

#Calculate the yearly averages(Across all regions) for ORGANIC avocados ONLY
mean(avo2015O$AveragePrice) #1.673324
mean(avo2016O$AveragePrice) #1.571684
mean(avo2017O$AveragePrice) #1.735521
mean(avo2018O$AveragePrice) #1.567176
#Calculate the yearly averages(Across all regions) for CONVENTIONAL avocados ONLY
mean(avo2015C$AveragePrice) #1.077963
mean(avo2016C$AveragePrice) #1.105595
mean(avo2017C$AveragePrice) #1.294888
mean(avo2018C$AveragePrice) #1.127886

avo2015Cbags<-aggregate(avo2015C[, 4:7], list(avo2015C$region), sum)#This will give the sum of all bags that
#the specific region sold during year 2015 for CONVENTIONAL avocados
#For lines 62 and 65, NR stands for Non-regional which is a specific location and R stands for regional
avo2015CbagsNR<-avo2015Cbags[-c(16,26,30,36,46:47,52:53),] #note that the regions GreatLakes,MidSouth, Northeast, plains, 
#SouthCentral, Southeast, TotalUS, and West are omitted because they are regions and they
#don't correspond to a specific location
avo2015CbagsR<-avo2015Cbags[-c(1:15,17:25,27:29,31:35,36:45,48:51,54),] #This line gives the total
#amount of bags sold per region

avo2016Cbags<-aggregate(avo2016C[, 4:7], list(avo2016C$region), sum)
avo2016CbagsNR<-avo2016Cbags[-c(16,26,30,36,46:47,52:53),]
avo2016CbagsR<-avo2016Cbags[-c(1:15,17:25,27:29,31:35,36:45,48:51,54),]

avo2017Cbags<-aggregate(avo2017C[, 4:7], list(avo2017C$region), sum)
avo2017CbagsNR<-avo2017Cbags[-c(16,26,30,36,46:47,52:53),]
avo2017CbagsR<-avo2017Cbags[-c(1:15,17:25,27:29,31:35,36:45,48:51,54),]

avo2018Cbags<-aggregate(avo2018C[, 4:7], list(avo2018C$region), sum)
avo2018CbagsNR<-avo2018Cbags[-c(16,26,30,36,46:47,52:53),]
avo2018CbagsR<-avo2018Cbags[-c(1:15,17:25,27:29,31:35,36:45,48:51,54),]

summary(avo2015Cbags)
summary(avo2015CbagsNR)     
summary(avo2015CbagsR)

summary(avo2016Cbags)
summary(avo2016CbagsNR)     
summary(avo2016CbagsR)

summary(avo2017Cbags)
summary(avo2017CbagsNR)     
summary(avo2017CbagsR)

summary(avo2018Cbags)
summary(avo2018CbagsNR)     
summary(avo2018CbagsR)

###***REPEATING THE ENTIRE PROCEDURE THIS TIME FOR ORGANIC AVOCADOS***###
avo2015Obags<-aggregate(avo2015O[, 4:7], list(avo2015O$region), sum)
avo2015ObagsNR<-avo2015Obags[-c(16,26,30,36,46:47,52:53),] 
avo2015ObagsR<-avo2015Obags[-c(1:15,17:25,27:29,31:35,36:45,48:51,54),] 

avo2016Obags<-aggregate(avo2016O[, 4:7], list(avo2016O$region), sum)
avo2016ObagsNR<-avo2016Obags[-c(16,26,30,36,46:47,52:53),]
avo2016ObagsR<-avo2016Obags[-c(1:15,17:25,27:29,31:35,36:45,48:51,54),]

avo2017Obags<-aggregate(avo2017O[, 4:7], list(avo2017O$region), sum)
avo2017ObagsNR<-avo2017Obags[-c(16,26,30,36,46:47,52:53),]
avo2017ObagsR<-avo2017Obags[-c(1:15,17:25,27:29,31:35,36:45,48:51,54),]

avo2018Obags<-aggregate(avo2018O[, 4:7], list(avo2018O$region), sum)
avo2018ObagsNR<-avo2018Obags[-c(16,26,30,36,46:47,52:53),]
avo2018ObagsR<-avo2018Obags[-c(1:15,17:25,27:29,31:35,36:45,48:51,54),]

summary(avo2015Obags)
summary(avo2015ObagsNR)     
summary(avo2015ObagsR)

summary(avo2016Obags)
summary(avo2016ObagsNR)     
summary(avo2016ObagsR)

summary(avo2017Obags)
summary(avo2017ObagsNR)     
summary(avo2017ObagsR)

summary(avo2018Obags)
summary(avo2018ObagsNR)     
summary(avo2018ObagsR)

