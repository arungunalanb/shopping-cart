setwd("F:/xangars/ShoppingCart")

# loading libraries
library(dplyr)
library(reshape2)
library(plotrix)

# creating an example of orders
set.seed(15)
df <- data.frame(orderId=sample(c(1:1000), 5000, replace=TRUE),
                 product=sample(c('NULL','a','b','c','d'), 5000, replace=TRUE,
                                prob=c(0.15, 0.65, 0.3, 0.15, 0.1)))
df <- df[df$product!='NULL', ]

# removing duplicates

df <- df[!duplicated(df), ]

# arranging data set
df <- df %>%
  arrange(orderId, product)

# creating product's matrix
  prod.matrix <- dcast(df, orderId ~ product, fun.aggregate = NULL)

  prod.matrix <- prod.matrix %>%
  mutate(cart = paste(a, b, c, d, sep='')) # check all product names

  prod.matrix$cart <- gsub('NA', '', prod.matrix$cart)

# calculating number of product's combinations
  comb <- prod.matrix %>%
  group_by(cart) %>%
  summarise(num=n())

head(comb)

# calculating number of products in combinations
comb$l <- sapply(comb$cart, nchar)

# calculating total number of orders/carts
tot <- sum(comb$num)

# spliting orders for sets with 1 product and more than 1 product
one.prod <- comb[comb$l==1,]
sev.prod <- comb[comb$l!=1,]

# arranging several products' data set
sev.prod <- arrange(sev.prod, desc(l)) 

# defining parameters for pie chart
iniR <- 0.2 # initial radius
all.colors <- list(NO='white', a='#fec44f', b='#fc9272', c='#a1d99b', d='#fee0d2') # palette
colors <- all.colors[ c(1:(nrow(one.prod)+1))] # set of colors

# 0 circle: blank
pie(1, radius=iniR, init.angle=90, col=c('white'), border = NA, labels='', edges=300)

# drawing circles from last to 2nd
for (i in nrow(one.prod):1) {
  p <- grep(one.prod$cart[i], sev.prod$cart)
  col <- rep('NO', times=nrow(sev.prod))
  col[p] <- one.prod$cart[i]
  floating.pie(0,0,c(sev.prod$num, tot-sum(sev.prod$num)), radius=(2+i)*iniR, startpos=pi/2, col=as.character(colors[ c(col, 'NO')]), border="#44aaff")
}

# 1 circle: orders with 1 product
floating.pie(0,0,c(tot-sum(one.prod$num),one.prod$num), radius=2*iniR, startpos=pi/2, col=as.character(colors[ c('NO',one.prod$cart)]), border="#44aaff")

# legend
legend(1.5, 2*iniR, gsub("_"," ",names(colors)[-1]), col=as.character(colors[-1]), pch=19, bty='n', ncol=1)

