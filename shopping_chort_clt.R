
setwd("F:/xangars/ShoppingCart")

cohort.sum <- data.frame(cohort=c('Cohort01', 'Cohort02', 'Cohort03', 'Cohort04', 'Cohort05', 'Cohort06', 'Cohort07', 'Cohort08', 'Cohort09', 'Cohort10', 'Cohort11', 'Cohort12'),
                         M1=c(270000,0,0,0,0,0,0,0,0,0,0,0),
                         M2=c(85000,275000,0,0,0,0,0,0,0,0,0,0),
                         M3=c(72000,63000,277000,0,0,0,0,0,0,0,0,0),
                         M4=c(52000,42000,76000,361000,0,0,0,0,0,0,0,0),
                         M5=c(50000,45000,60000,80000,288000,0,0,0,0,0,0,0),
                         M6=c(51000,52000,55000,51000,58000,253000,0,0,0,0,0,0),
                         M7=c(51000,69000,48000,45000,42000,54000,272000,0,0,0,0,0),
                         M8=c(46000,85000,77000,41000,38000,37000,74000,352000,0,0,0,0),
                         M9=c(38000,42000,72000,41000,31000,30000,49000,107000,285000,0,0,0),
                         M10=c(39000,38000,45000,33000,34000,34000,46000,83000,69000,279000,0,0),
                         M11=c(38000,42000,31000,32000,26000,28000,43000,82000,51000,87000,282000,0),
                         M12=c(35000,35000,38000,45000,35000,32000,48000,44000,47000,52000,92000,500000))
library(ggplot2)
library(reshape2)

cohort.chart <- melt(cohort.sum, id.vars = "cohort")
colnames(cohort.chart) <- c('cohort', 'month', 'revenue')

#define palette
blues <- colorRampPalette(c('lightblue', 'darkblue'))

#plot data
p <- ggplot(cohort.chart, aes(x=month, y=revenue, group=cohort))
p + geom_area(aes(fill = cohort)) +
  scale_fill_manual(values = blues(nrow(cohort.sum))) +
  ggtitle('Total revenue by Cohort')