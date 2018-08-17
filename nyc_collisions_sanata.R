
library(tidyverse)
library(tm)
library(stringr)
library(ggmap)
library(reshape2)

path <- "C:\\Users\\Sanata\\Dropbox\\NYC Council Data\\"
url <- "https://data.cityofnewyork.us/api/views/h9gi-nx95/rows.csv?accessType=DOWNLOAD"

#import datasets 

crash <- read.csv(url, 
                  sep=",", stringsAsFactors = F, header = T, 
                  na.strings = c("", " "))

##### Functions #####

getUnique <- function(df, vars.idx){ #return a vector of unique values across columns in a dataframe 
  
  list.all <- lapply(vars.idx, function(x) unique( df[, x] ) ) %>% unlist() 
  vec.unique <- unique( list.all[!is.na(list.all)] )
  return(vec.unique)
  
}

getUnique.top <- function(df, vars.idx, top){ #return a vector of unique most frequent values across columns in a dataframe
  list.all <- lapply(vars.idx, function(x) 
    sort(table(df[, x]), decreasing=T)[1:top] ) %>% unlist()
  vec.unique <- unique(names(list.all))
  return(vec.unique)
}

clean.str <- function(vec){ #returns a cleaned string vector 
  vec <- tolower(vec)
  vec <- removePunctuation(vec)
  vec <- stripWhitespace(vec)
  return(vec)
}

countWords.col <- function(words, df){
  #counts the number of times each word in a character vector appears across multiple columns in a data frame 
  #returns a data frame with the number of observations per words as the single variable  
  
  counts <- lapply(1:length(words), function(x)
    filter_all(df, any_vars(.==words[x])) %>% 
      summarise(n = n()) ) %>% 
    do.call(rbind, .) 
  return(counts)
  
}

countWords.col <- function(words, df){
  #counts the number of times each word in a character vector appears across multiple columns in a data frame 
  #returns a data frame with the number of observations per words as the single variable  
  
  counts <- lapply(1:length(words), function(x)
    filter_all(df, any_vars(.==words[x])) %>% 
      summarise(n = n()) ) %>% 
    do.call(rbind, .) 
  return(counts)
  
}

count.noNA <- function(df, col.names){
  #counts the number of rows containing no NAs across multiple columns in a data frame
  #returns the number of rows (numeric)
  
  counts <- df  %>% 
    filter_at(col.names, any_vars(!is.na(.))) %>% 
    summarise(n = n()) 
  return(counts$n)
} 

get.proptest.values <- function(ptest.list, names){ #returns a data frame of test statistics for a list of prop tests
  
  a <- sapply(1:length(ptest.list), function(x)
    return ( cbind(ptest.list[[x]]$estimate[1], 
                   ptest.list[[x]]$estimate[2], 
                   ptest.list[[x]]$p.value) ) )
  b  <- as.data.frame(t(a))
  colnames(b) <- c("prop1", "prop2", "pval")
  rownames(b) <- names
  b$diff <- b$prop1 - b$prop2
  b$ratio <- ifelse(b$diff>0, b$prop1 / b$prop2, b$prop2/b$prop1)
  return(b)
  
  
}



lookup.col <- function(string, df){
  
  check <- sapply(1:ncol(df), function(x)
    str_detect(df[, x ], string) 
  )
  torow <- apply(check, 1, function(x) ifelse(any(x==T, na.rm=T), 1, 0))
  return(torow)
  
}

lookup.row.na <- function(df){
  
  check <- sapply(1:nrow(df), function(x)
    any(is.na(df[x, ], string) == T)
  )
  return(check)
  
}

##########


###### VALIDATION ####### 

#compare missing rates for injury, death, and none 


summary(crash)
str(crash)

##see missing values 
miss.df <- crash %>% as_tibble() %>% 
              summarise_all( funs(n = sum(is.na(.)==0) )) %>% 
              gather(var, value, everything())
miss.df <- miss.df %>% 
            mutate( missing.rate = 1 - (value / nrow(crash)))


## check locations
#find obs with no location data
a <- with(crash,  which(is.na(BOROUGH) & is.na(ZIP.CODE) & is.na(LATITUDE) 
                        & is.na(LONGITUDE) & is.na(ON.STREET.NAME) & 
                          is.na(CROSS.STREET.NAME) & is.na(OFF.STREET.NAME))  )

#estimate how many observations of injuries and deaths would be lost if dropped
b <- with(crash[a, ],  which(NUMBER.OF.PERSONS.INJURED>0 | NUMBER.OF.PERSONS.KILLED>0)  )
length(b) / sum(crash$NUMBER.OF.PERSONS.INJURED>0 | crash$NUMBER.OF.PERSONS.KILLED>0)
#2.5% observations would be lost - decide to keep unless working with location data 

#validate lat/long values 
#NYC limits: West-East = -74.257159, -73.699215; Nort-South = 40.915568, 40.495992 (from nyc.gov)
lat.min = 40.495992
lat.max = 40.915568 
long.min = -74.257159
long.max = -73.699215
#check within limits 
out.lat <- which(! crash$LATITUDE >= lat.min & crash$LATITUDE <= lat.max )
out.long <- which(! crash$LONGITUDE >= long.min & crash$LONGITUDE <= long.max )




######### CLEANING ########## 

##1. make hour variable 
head(crash$DATE)
head(crash$TIME)

crash$time.hr <- as.numeric(case_when(
                           nchar(crash$TIME)==4 ~ substr(crash$TIME, 1, 1), 
                           nchar(crash$TIME)==5 ~ substr(crash$TIME, 1, 2), 
                           TRUE ~ NA_character_) 
                           )
#table(crash$time.hr) #check assignment 


##2. recode contributing type vars 

#get vars 
cont.fact.vars <- paste0("CONTRIBUTING.FACTOR.VEHICLE.", 1:5)
cont.fact.vars.idx <- which(colnames(crash) %in% paste0("CONTRIBUTING.FACTOR.VEHICLE.", 1:5))

#look at unique categories 
sort(unique(crash$CONTRIBUTING.FACTOR.VEHICLE.1))

#recode unspecified to NA 
crash <- crash %>% 
  mutate_at(paste0("CONTRIBUTING.FACTOR.VEHICLE.", 1:5), funs(recode(., "Unspecified" = NA_character_)) )

#recode repeated values 
contrib.factors <- getUnique(df = crash, vars.idx = cont.fact.vars.idx )

test <- lapply(1:length(contrib.factors), function(x)
            agrep(contrib.factors[x], contrib.factors[-x], ignore.case=T)) #find approx matches
getLengths <- lapply(test, length)
repeated <- which(getLengths > 0)
sort(contrib.factors[repeated])
#recode by hand 
crash <- crash %>% 
  mutate_at(paste0("CONTRIBUTING.FACTOR.VEHICLE.", 1:5), 
            funs(recode(., 
                        "Cell Phone (hand-Held)" = "Cell Phone (hand-held)", 
                        "Drugs (illegal)" = "Drugs (Illegal)" , 
                        "Illnes" = "Illness")) )
#re-populate unique contributing factor categories 
contrib.factors <- getUnique(df = crash, vars.idx = cont.fact.vars.idx )



##3. categorize vehicle codes 

#get vehicle type vars
vc.vars <- paste0("VEHICLE.TYPE.CODE.", 1:5) 
vc.vars.idx <- which(colnames(crash) %in% paste0("VEHICLE.TYPE.CODE.", 1:5)) 

#clean columns and add to dataframe  
crash[, paste0("VEHICLE.TYPE.CODE.", 1:5, ".clean")] <- sapply(vc.vars.idx, 
                                                               function(x) 
                                                                clean.str(crash[, x]))
vc.vars.clean <-  paste0("VEHICLE.TYPE.CODE.", 1:5, ".clean") 
vc.vars.clean.idx <- which(colnames(crash) %in% paste0("VEHICLE.TYPE.CODE.", 1:5, ".clean") )

##Categorize the top 20 vehicle type codes  
top20vec <- getUnique.top(df = crash, vars.idx = vc.vars.clean.idx, top = 20) #calculate top 20 across all variables
top20vec
#condense categories by hand  
crash <- crash %>% 
  mutate_at(paste0("VEHICLE.TYPE.CODE.", 1:5, ".clean"), 
            funs(recode(., 
               "4dsd"= "sedan", 
               "box truck" = "large com veh6 or more tires", 
               "station wagonsport utility vehicle" =  "sport utility station wagon", 
               "livery vehicle" = "taxi", 
               "ambulance" = "rescue", 
               "fire truck" = "rescue", 
               "scooter" = "two wheel", 
               "motorcycle" = "two wheel", 
               "bike" = "bicycle"))
            )
#recreate top 20 list with recodes 
top20vec <- getUnique.top(df = crash, vars.idx = vc.vars.clean.idx, top = 20)
top20vec 
#select final categories, removing "unknown" 
final.vtypes <- top20vec[c(1:7, 9:14)]
#create final recoded variables 
crash[, paste0("VEHICLE.TYPE.CODE.", 1:5, ".recode")] <- sapply(vc.vars.clean, 
                                                                function(x)
                                                                case_when(crash[, x] %in% final.vtypes ~ crash[, x], 
                                                                         TRUE ~ NA_character_))

##check missing values 
vc.vars.recode.idx <- which(colnames(crash) %in% paste0("VEHICLE.TYPE.CODE.", 1:5, ".recode"))
vc.vars.recode <- paste0("VEHICLE.TYPE.CODE.", 1:5, ".recode")

##compare missing rates in recoded vs original variables 
missing.recode <- crash %>% 
                    summarise_at(vc.vars.recode, funs(meanNA = mean(is.na(.)))) %>% round(., 2)
missing.orig <- crash %>% 
                  summarise_at(vc.vars.clean, funs(meanNA = mean(is.na(.)))) %>% round(., 2)
lost.obs <- missing.orig - missing.recode 
#lost 2% of type 1 codes, 7% of type 2 codes, 1% of type 3 codes
#note: with more time, I would use approximate matching (agrep or amatch in stringdist) to identify 
#more vechicle type codes and reduce missing rates acros codes 

##4. Create indicators 
crash <- crash %>% 
  mutate(
    victim = ifelse(NUMBER.OF.PERSONS.KILLED>0 | NUMBER.OF.PERSONS.INJURED>0, 1, 0), 
    injury = ifelse(NUMBER.OF.PERSONS.INJURED>0, 1, 0), 
    fatal = ifelse(NUMBER.OF.PERSONS.KILLED>0, 1, 0 )
  )


#consolidate variables 
crash$CONTRIBUTING.FACTOR.ALL <- apply(crash[, paste0("CONTRIBUTING.FACTOR.VEHICLE.", 1:5)], 
                                      1, paste, collapse = ",")
crash$CONTRIBUTING.FACTOR.ALL <- ifelse(str_count(crash$CONTRIBUTING.FACTOR.ALL, "NA")==5, 
                                        NA, 
                                        crash$CONTRIBUTING.FACTOR.ALL)

crash$VEHICLE.TYPE.CODE.ALL <- apply(crash[, paste0("VEHICLE.TYPE.CODE.", 1:5, ".recode")], 
                                     1, paste, collapse = ",")
crash$VEHICLE.TYPE.CODE.ALL <- ifelse(str_count(crash$VEHICLE.TYPE.CODE.ALL, "NA")==5, 
                                      NA, 
                                      crash$VEHICLE.TYPE.CODE.ALL)

save(crash, file=paste0(path, "NYPD_Motor_Vehicle_Collisions_edited.RData"))



####### ASSESS ######### 


load(paste0(path, "NYPD_Motor_Vehicle_Collisions_edited.RData"))

### Best and worst driving times ###
tbl.by.hr <- crash %>%
              filter(TIME != "0:00") %>%  
              count(time.hr) 
              
save(tbl.by.hr, file=paste0(path, "rateByHour.RData"))

#plot
mybar <- barplot(as.matrix(tbl.by.hr.mean[, -1]), beside=T, names.arg="", 
                ylim=c(0, 100000) , axes=F, 
                main = "Collisions by Hour, 2012-2018", 
                xlab = "24-Hour Time", ylab = "Number of Collisions", 
                cex.lab = 1.5, cex.main=1.5)              
axis(side = 2, at = seq(0, 100000, 20000), labels=paste0(seq(0, 100, 20), "K"), cex.axis=1.3)
axis(side = 1, at = mybar, labels=c(0:23))



### Factors impacting injury and mortality rates ###

#create indices 
vic <- crash$victim
inj <- crash$injury
fatal <- crash$fatal

#get rates 
rates.by.type <- crash %>% 
                    select(victim, injury, fatal) %>% 
                    summarise_all( funs(mean = mean(., na.rm=T))) %>% round(., 3)
rates.by.type 
save(rates.by.type, file=paste0(path, "ratesByType.RData"))

##1. set indicator 
#ind <- inj 
ind <- fatal 
#ind.sv <- "injury"
ind.sv <- "fatal"

##2. eyeball compare top 15 factors for the two populations 
top.ind <- getUnique.top(df = crash[ind, ], vars.idx = cont.fact.vars.idx, top=10)
top.noind <- getUnique.top(df = crash[!ind, ], vars.idx = cont.fact.vars.idx, top=10)
top.ind[which(! top.ind %in% top.noind )]
#passenger distraction, physical disability are unique among top 10 factors in crashes with victims 
#unsafe speed, passenger distraction, physical disability, and pedestrian/cyclists are unique 
#among top 10 factors in crashes with fatalities 


##3. Calculate injury and mortality rates by factor 

t <- lapply(1:length(contrib.factors), function(x)
            crash %>% 
            filter(str_detect(CONTRIBUTING.FACTOR.ALL, contrib.factors[x])) %>% 
            summarise(mean = mean(injury, na.rm=T), 
                      se = sd(injury, na.rm=T) / sqrt(n()) ) ) %>% 
            do.call(rbind, .)

rownames(t) <- contrib.factors
t$conf.u <- t$mean + 1.96*t$se
t$conf.l <- t$mean - 1.96*t$se
t <- t[-which(t$conf.l<0 | is.na(t$se)), ] #drop low observations
t <- t[order(t$mean, decreasing = T), ] 

rates.injury <- t
save(rates.injury, file = paste0(path, "factor_rates_injury.RData"))

#Calculate mortality rates by factor 
t <- lapply(1:length(contrib.factors), function(x)
          crash %>% 
            filter(str_detect(CONTRIBUTING.FACTOR.ALL, contrib.factors[x])) %>% 
            summarise(mean = mean(fatal, na.rm=T), 
                      se = sd(fatal, na.rm=T) / sqrt(n()) ) ) %>% 
            do.call(rbind, .)

rownames(t) <- contrib.factors
t$conf.u <- t$mean + 1.96*t$se
t$conf.l <- t$mean - 1.96*t$se
t <- t[-which(t$conf.l<0 | is.na(t$se)), ] #drop low observations
t <- t[order(t$mean, decreasing = T), ] 

rates.fatal <- t
save(rates.fatal, file = paste0(path, "factor_rates_fatal.RData"))


#4. compare to most frequent factors overall 
all.factors <- countWords.col(words = contrib.factors, df = crash[, cont.fact.vars])
n.all.factors <- count.noNA(df = crash, col.names = cont.fact.vars) %>% 
                    rep(., length(contrib.factors))
p.all.factors <- sort( all.factors$n / n.all.factors , decreasing = T)
names(p.all.factors) <- contrib.factors 
save(p.all.factors, file = paste0(path, "topContribFactors.RData"))

#5. Compare times 
sort(table(crash$time.hr[ind]), decreasing = T)
sort(table(crash$time.hr[!ind]), decreasing = T)
#doesn't seem to be a difference in times of injury & non-injury crashes 
#there IS a difference in times for fatal & non-fatal: includes later times (21h, 23h , 4h)

#6. Compare location 
tbl.by.brgh <- crash %>%
                  count(BOROUGH, fatal) %>% 
                  arrange(n)


### Effect of vehicle type on mortality #### 

#0. check missing vehicle types for fatal crashes vs. non-fatal crashes 
m1 <- which(crash$NUMBER.OF.PERSONS.KILLED>0 & is.na(crash$VEHICLE.TYPE.CODE.1.recode) & 
        is.na(crash$VEHICLE.TYPE.CODE.2.recode))
length(m) / sum(crash$NUMBER.OF.PERSONS.KILLED>0) #missing 112, or 7.6% of fatal crash observations 

m2 <- which(crash$NUMBER.OF.PERSONS.KILLED==0 & is.na(crash$VEHICLE.TYPE.CODE.1.recode) & 
              is.na(crash$VEHICLE.TYPE.CODE.2.recode))
length(m2) / sum(crash$NUMBER.OF.PERSONS.KILLED==0) #missing 22431, or 1.7% of non-fatal crash observations

#1. Identify most common vehicle types
all.types <- countWords.col(words = final.vtypes, df = crash[, vc.vars.recode])
n.all.types <- count.noNA(df = crash, col.names = vc.vars.recode)
p.all.types <- all.types$n / n.all.types
names(p.all.types) <- final.vtypes
sort(p.all.types, decreasing=T)


#2.Assign vehicle type indicators based on results above
v.yes <- lapply(1:length(final.vtype.cats), function(x)
            crash %>% 
            filter(str_detect(VEHICLE.TYPE.CODE.ALL, final.vtype.cats[x])) %>% 
              summarise(mean.yes = round(mean(fatal, na.rm=T), 3)*100 ) ) %>% 
              do.call(rbind, .)

v.no <- lapply(1:length(final.vtype.cats), function(x)
            crash %>% 
              filter(str_detect(VEHICLE.TYPE.CODE.ALL, final.vtype.cats[x])==F) %>% 
              summarise(mean.no = round(mean(fatal, na.rm=T), 3)*100 ) ) %>% 
              do.call(rbind, .)

rates.by.v <- bind_cols(v.no, v.yes)
rownames(rates.by.v) <- final.vtype.cats
rates.by.v$diff <- rates.by.v$mean.yes - rates.by.v$mean.no 
save(rates.by.v, file=paste0(path, "mratesByVehic.RData"))

#3. Check time
sort(table(crash$time.hr[fatal]))
hist(crash$time.hr[fatal])
#the most fatal crashes happen between 17h-23h, otherwise uniform (later period than total crash)



####### Exploratory Analysis ###### 

#1. compare common vehicle types in fatal and non-fatal crashes (t-tests)
#1a. calculate successes: find number of occurrences of each vehicle type
ind <- fatal 
x.ind <- countWords.col(words = final.vtypes, df = crash[ind, vc.vars.recode])
x.notind <- countWords.col(words = final.vtypes, df = crash[!ind, vc.vars.recode])

#1b. calculate totals 
n.ind <- count.noNA(df = crash[ind, ], col.names = vc.vars.recode) %>% 
  rep(., length(final.vtypes))
n.notind <- count.noNA(df = crash[!ind, ], col.names = vc.vars.recode) %>% 
  rep(., length(final.vtypes))

#1c. two-sample proportion t-test 
ptest.list <- lapply(1:length(final.vtypes),  function(i)
  prop.test(c(x.ind$n[i], x.notind$n[i]), 
            c(n.ind[i], n.notind[i]))
)
ptest <- get.proptest.values(ptest.list, names=final.vtypes) #prop1 is with the indicator 
ptest <- ptest[-which(ptest$pval > 0.05), ] #Remove p-vals > 0.05 

#1d. order by greatest differences and export (preferred choice is ratio)
ptest <- ptest[order(ptest$ratio, decreasing = T), ]
save(ptest, file = paste0(path, "topVehicleType_fatal.RData"))

#1e. plot 
d <- sqrt(((log(ptest.inj$prop1/ptest.inj$prop2))^2)/2)
ptest.inj$dist <- d

plot(log(ptest.inj$prop1), log(ptest.inj$prop2), pch= ifelse(ptest.inj$dist >=0.64, 16, 1 ), 
     main = "Rates of Contributing Factors (Logged)", xlab = "Injury", ylab = "No Injury")
text(log(ptest.inj$prop1[ptest.inj$dist >=0.64]), log(ptest.inj$prop2[ptest.inj$dist >=0.64]) - 0.3, substr(rownames(ptest.inj)[ptest.inj$dist >=0.64], 1, 20), cex = 0.75)
lines(c(-10, 10), c(-10, 10), lty = 2)


