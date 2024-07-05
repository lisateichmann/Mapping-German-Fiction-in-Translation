### Chapter 2.0 Data Quality and Sampling Bias

#Load packages
setwd("/cloud/project/Data")
install.packages(c("ggplot2", "dplyr", "gridExtra", "stringr", "readxl"))
library(ggplot2)
library(dplyr)
library(gridExtra)
library(stringr)
library(readxl)

#Import data extracted from DNB from pre-processing script for all years
alldnb_2020<-read.csv("alldnb.csv", header=T,row.names=NULL,sep=",")
alldnb_2021<-read.csv("alldnb_2021_2.csv", header=T,row.names=NULL,sep=",")

###Consistency
#Consistency = the percentage of values that match across records (DNB, VIAF, Index Translationum)
# The raw data is restricted and can therefore not be shared here, which is why only frequency tables have been included here

#Table with number of translated titles

###CLASSIFIED
#itdata <- read_excel("IT_GER_alltrans_readable.xls")
#viafdata <- read.csv("viafdata_geronly.csv", header=T)

#Summary statistics
#summary(itdata)
#summary(viafdata)

###2.0.2.2. Accuracy
#Accuracy = ratio of data to errors

#split column to seperate author from translator in the field "creator"
translators <- as.data.frame(str_split_fixed(alldnb_2021$creator, ";", 3))

#rename columns
names(translators)[1] <- "author"
names(translators)[2] <- "translator"
names(translators)[3] <- "additional"

#delete empty rows
translators<-translators[!apply(translators == "", 1, all),]
write.csv(translators, file="alldnb_translators_ch2.0_v2.csv")

#clean "Verfasser" and "Ubersetzer" in Open Office
authortranslator <- read.csv("alldnb_translators_nospace_ch2.0_v2.csv", header=TRUE, sep="\t")

#check for author name ambiguaty
View(table(authortranslator$author))
nrow(table(authortranslator$author))

#OR

###REVISE WITH CH 2.1.

author_uniq <- unique(authortranslator$author)
length(author_uniq)
View(as.data.frame(unique(authortranslator$author)))

#write.csv((table(authortranslator$author)), file="alldnb_author_freq_ch2.0_v2.csv")

#finding: returns 6457 unique author names, which are coherent
#3988 unique author names after cleaning!

#Check for accuracy in titles
#the column title includes both, the translated and original title and oftentimes just includes the edition (Bd. or a number) NEED TO BE EXCLUDED!
#the column uniform.title is the original German title

View(table(alldnb$title))
View(as.data.frame(unique(alldnb$title)))
length((unique(alldnb$title)))

View(table(alldnb$uniform.title))
View(as.data.frame(unique(alldnb$uniform.title)))
length((unique(alldnb$title)))

#publisher

View(table(alldnb$publisher))

#see if all publishers have place and if yes how consistent place names are

publisher_place <- as.data.frame(str_split_fixed(alldnb$publisher, ":", 2))
names(publisher_place)[1] <- "place"
names(publisher_place)[2] <- "publisher"
write.csv(publisher_place, file = "alldnb_publisherplace_ch2.0_v2.csv")

View(table(publisher_place$place))

#format

View(table(alldnb$format))

#subset to check if both page number and dimensions are included

format_compl <- as.data.frame(str_split_fixed(alldnb$format, ";", 2))
View(table(format_compl$V2))

format_grams <- as.data.frame(str_split_fixed(format_compl$V2, ",", 2))
View(table(format_grams$V2))

#binding prize contains binding type and price

binding_price <- as.data.frame(str_split_fixed(alldnb$binding.price, ":", 2))
View(table(binding_price$V2))

###2.0.2.3. Completeness
#Completeness = #Of NA’s for each category (country code, language)/total of values

View(table(alldnb$creator))
#2331/35972 = 6.5% missing values of creator
View(table(alldnb$publisher))
#2468/35972 = 6.9% missing values of publisher
View(table(alldnb$country))
#2693/35972 = 7.5% missing values of country
View(table(alldnb$ISBN))
#2286/35972 missing ISBN
View(table(translators$translator))
#15288/35972 = 42% missing values of translator
View(table(alldnb$volume))
#33506 missing volume
View(table(alldnb$edition))
#21692 missing edition
View(table(format_compl$V2))
#2800 missing dimension (7.7%)
View(table(format_grams$V2))
#35619/35972 (99%) missing weight
View(table(alldnb$binding.price))
#1315/35972 (3.6%) missing binding price
View(table(alldnb$collective.title))
#18091 missing collective title
View(table(alldnb$subject.headings))
#29580 missing subject headings
View(table(alldnb$uniform.title))
#8591/35972 or 23% of all titles have a missing original title


View(table(alldnb$language))
View(table(alldnb$year))
#no missing values for language and year


###Plot


plotdf <- data.frame(category = c("creator", "publisher", "country", "ISBN", "translator", "volume", "edition", "dimension", "weight", "binding price", "collective title", "subject heading", "uniform title"),
                 percentage_nas = c(6.5, 6.9, 7.5, 6.3, 42, 93, 60, 7.7, 99, 3.6, 50.2, 82.2, 23.8)
)

write.csv(plotdf, file="alldnb_na_perc_ch2.0.csv")

ggplot(plotdf,aes(x= reorder(category,-percentage_nas),percentage_nas))+geom_bar(stat ="identity") + xlab("Accuracy in the DNB translation dataset according to missing values in percentages across categories")+coord_flip()+theme_bw()

#more styled
ggplot(plotdf,aes(x= reorder(category,-percentage_nas),percentage_nas))+geom_bar(stat ="identity")+ xlab("Category") + ylab("Percentage of Missing Values")+ labs(title = "Completeness in the DNB translation dataset", subtitle = "Percentages across categories") +coord_flip()+theme_bw()
setwd("~/Desktop/Thesis/Dissertation/Data/figures/ch2.0")
ggsave("figure_ch1.0_dnball_completeness_v2.png")


#Another method for counting missing values

sum(translators$V2=="")
which(translators$V2=="", arr.ind=TRUE)

#OR
table(is.na(alldnb$creator))
table(is.na(alldnb$publisher))
table(is.na(alldnb$country))
table(is.na(alldnb$language))

###2.0.3. Sampling Bias and Representativeness
#	Precision/Recall Assessment for 2020
#False positives = what has been labeled a translation and is not
#False negatives = what has not been labeled a translation but is

#example: 2020 dataset

###Precision

rs_alldnb <- sample_n(alldnb, 100)
write.csv(rs_alldnb, file="alldnb_random_sample_precision_ch2.0.csv")

#Rapackages for help

setwd("~/Desktop/Thesis/Dissertation/Data/Results/Chapter2.0")

#bibliometrix only works for SCOPUS file formats
dnb2020<-read.csv("dnb-datashop_ger_2020.csv", header=T,row.names=NULL,sep=";")

#dataQualityR NOT WORKING
data(alldnb)
num.file <- paste(tempdir(), "dq_num.csv", sep= "")
cat.file <- paste(tempdir(), "dq_cat.csv", sep= "")
checkDataQuality(data= alldnb, out.file.num= num.file, out.file.cat= cat.file)

###for gender: https://github.com/COMHIS/bibliographica/tree/master/inst/extdata/names