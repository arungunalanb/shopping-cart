cohort.clients <- data.frame(cohort=c('Cohort01','Cohort02',
                                      'Cohort03','Cohort04','Cohort05','Cohort06','Cohort07',
                                      'Cohort08','Cohort09','Cohort10','Cohort11','Cohort12'),
                             M01=c(11000,0,0,0,0,0,0,0,0,0,0,0),
                             M02=c(1900,10000,0,0,0,0,0,0,0,0,0,0),
                             M03=c(1400,2000,11500,0,0,0,0,0,0,0,0,0),
                             M04=c(1100,1300,2400,13200,0,0,0,0,0,0,0,0),
                             M05=c(1000,1100,1400,2400,11100,0,0,0,0,0,0,0),
                             M06=c(900,900,1200,1600,1900,10300,0,0,0,0,0,0),
                             M07=c(850,900,1100,1300,1300,1900,13000,0,0,0,0,0),
                             M08=c(850,850,1000,1200,1100,1300,1900,11500,0,0,0,0),
                             M09=c(800,800,950,1100,1100,1250,1000,1200,11000,0,0,0),
                             M10=c(800,780,900,1050,1050,1200,900,1200,1900,13200,0,0),
                             M11=c(750,750,900,1000,1000,1180,800,1100,1150,2000,11300,0),
                             M12=c(740,700,870,1000,900,1100,700,1050,1025,1300,1800,20000))

cohort.clients.r <- cohort.clients #create new data frame
totcols <- ncol(cohort.clients.r) #count number of columns in data set for (i in 1:nrow(cohort.clients.r)) { #for loop for shifting each row

for (i in 1:nrow(cohort.clients.r)) { #for loop for shifting each row
  df <- cohort.clients.r[i,] #select row from data frame
  df <- df[ , !df[]==0] #remove columns with zeros
  partcols <- ncol(df) #count number of columns in row (w/o zeros)
  #fill columns after values by zeros
  if (partcols < totcols) df[, c((partcols+1):totcols)] <- 0
  cohort.clients.r[i,] <- df #replace initial row by new one
}

#calculate retention (1)
x <- cohort.clients.r[,c(2:13)]
y <- cohort.clients.r[,2]
reten.r <- apply(x, 2, function(x) x/y )
reten.r <- data.frame(cohort=(cohort.clients.r$cohort), reten.r)
