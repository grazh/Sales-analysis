#Gold

library(readxl)
library(neuralnet)
library(ggplot2)
data <- read_xlsx("data.xlsx")

# Gold от Education

gold_educ <- cbind(data$Education, data$MntGoldProds)
gold_educ <- data.frame(gold_educ, stringsAsFactors = FALSE)
names(gold_educ) <- c("educ", "gold")
gold_educ[1:5,]
ed_group <- frame()
ed_group["my_mean"] <- mean(as.numeric(gold_educ$gold))
ed_group["Cycle"] <- mean(as.numeric(gold_educ[grep("Cycle", gold_educ$educ), 2]))
ed_group["Basic"] <- mean(as.numeric(gold_educ[grep("Basic", gold_educ$educ), 2]))
ed_group["Graduation"] <- mean(as.numeric(gold_educ[grep("Graduation", gold_educ$educ), 2]))
ed_group["Master"] <- mean(as.numeric(gold_educ[grep("Master", gold_educ$educ), 2]))
ed_group["PhD"] <- mean(as.numeric(gold_educ[grep("PhD", gold_educ$educ), 2]))
ed_group

barplot(ed_group, width = 1, col = "blue")
# выпускники больше всего тратятся на золото

# Gold от kids

gold_kids <- cbind(data$Kidhome, data$MntGoldProds)
gold_kids <- data.frame(gold_kids, stringsAsFactors = FALSE)
names(gold_kids) <- c("kids", "gold")
gold_kids[1:5,]

mean(gold_kids[grep(0, gold_kids$kids), 2])
mean(gold_kids[grep(1, gold_kids$kids), 2])
mean(gold_kids[grep(2, gold_kids$kids), 2])
data$Kidhome
gold_kids <- scale(gold_kids, center = FALSE)
gold_kids
model1 <- neuralnet(data = gold_kids, gold ~ kids, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model1)


# Gold от teens

gold_teen <- cbind(data$Teenhome, data$MntGoldProds)
gold_teen <- data.frame(gold_teen, stringsAsFactors = FALSE)
names(gold_teen) <- c("teen", "gold")
gold_teen[1:5,]

mean(gold_teen[grep(0, gold_teen$teen), 2])
mean(gold_teen[grep(1, gold_teen$teen), 2])
mean(gold_teen[grep(2, gold_teen$teen), 2])
sd(gold_teen[grep(0, gold_teen$teen), 2])
sd(gold_teen[grep(1, gold_teen$teen), 2])
sd(gold_teen[grep(2, gold_teen$teen), 2])

gold_teen <- scale(gold_teen, center = FALSE)
model2 <- neuralnet(data = gold_teen, gold ~ teen, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model2)

# Как много PhD и Master без детей

gold_educ <- cbind(data$Education, data$MntGoldProds, data$Kidhome)
gold_educ <- data.frame(gold_educ, stringsAsFactors = FALSE)
names(gold_educ) <- c("educ", "gold", "kids")
gold_educ[1:3,]
master <- gold_educ[grep("PhD", gold_educ$educ), 2:3]
length(master[grep(0, master$kids), 1])
master[grep(0, master$kids), 1:2]


masta <- gold_educ[grep("Master", gold_educ$educ), 2:3]
length(masta[grep(0, masta$kids), 1])
length(masta[masta == 0])
mean(as.numeric(masta[grep(0, masta$kids), 1]))

phd <- gold_educ[grep("PhD", gold_educ$educ), 2:3]
phd
length(phd[grep(0, phd$kids), 1])
mean(as.numeric(phd[grep(0, phd$kids), 1]))

# Gold Master и PhD с подростками
gold_educ <- cbind(data$Education, data$MntGoldProds, data$Teenhome)
gold_educ <- data.frame(gold_educ, stringsAsFactors = FALSE)
names(gold_educ) <- c("educ", "gold", "teen")
gold_educ[1:3,]


masta <- gold_educ[grep("Master", gold_educ$educ), 2:3]
length(masta[grep(0, masta$teen), 1])
masta[grep(0, masta$teen), 1]
mean(as.numeric(masta[grep(0, masta$teen), 1]))

phd <- gold_educ[grep("PhD", gold_educ$educ), 2:3]
phd
length(phd[grep(0, phd$teen), 1])
mean(as.numeric(phd[grep(0, phd$teen), 1]))


model2 <- neuralnet(data = data, MntGoldProds ~ Kidhome + Teenhome + Recency, hidden = c(3, 3), threshold = 0.001, lifesign = "full")
plot(model2)


# покупки золота по году рождения

group <- data$Year_Birth
group[group<=1950] = 1
group[group<=1960 & group>=1951] = 2
group[group<=1970 & group>=1961] = 3
group[group<=1980 & group>=1971] = 4
group[group<=1990 & group>=1981] = 5
group[group<=2000 & group>=1991] = 6

group

year_gold <- cbind(group, data$Year_Birth, data$MntGoldProds)
year_gold <- data.frame(year_gold)
mean(year_gold[grep(1, year_gold$group), 3])
length(year_gold[grep(1, year_gold$group), 3])
# Клиенты старше 1950 года (61 чел) тратят 219
mean(year_gold[grep(2, year_gold$group), 3])
length(year_gold[grep(2, year_gold$group), 3])
# Клиенты второй группы (239 челов) тратят 266
mean(year_gold[grep(3, year_gold$group), 3])
length(year_gold[grep(3, year_gold$group), 3])
# Клиенты 3 группы (271 чел) третят 234
mean(year_gold[grep(4, year_gold$group), 3])
length(year_gold[grep(4, year_gold$group), 3])
# Клиенты 4 группы (345 челов) тратят 198
mean(year_gold[grep(5, year_gold$group), 3])
length(year_gold[grep(5, year_gold$group), 3])
# Клиенты 5 группы (166 челов) тратят 190
mean(year_gold[grep(6, year_gold$group), 3])
length(year_gold[grep(6, year_gold$group), 3])
# Клиенты 6 группы (23 чела) тратят 319



# Кластеры по зп


h1 <- hclust(dist(data$Income))
summary(h1)
plot(h1, hang = -1, main = "Иерархическая кластеризация по зарплате")
rect.hclust(h1, k = 6)
group <- cutree(h1, k = 6)
group[1:100]


inc_group <- data.frame(cbind(group, data$Income, data$MntGoldProds, data$Kidhome, data$Year_Birth, data$Education), stringsAsFactors = FALSE)
names(inc_group) <- c("group", "income", "sweets", "kids", "year", "educ")
inc_group[1:5, ]

my_mean <- function(x)
{
  return (mean(as.numeric(x)))
}



my_mean(inc_group[grep(1, inc_group$group), 3])
length(inc_group[grep(1, inc_group$group), 3])
min(inc_group[grep(1, inc_group$group), 2])
max(inc_group[grep(1, inc_group$group), 2])

my_mean(inc_group[grep(2, inc_group$group), 3])
length(inc_group[grep(2, inc_group$group), 3])
min(inc_group[grep(2, inc_group$group), 2])
max(inc_group[grep(2, inc_group$group), 2])

my_mean(inc_group[grep(3, inc_group$group), 3])
length(inc_group[grep(3, inc_group$group), 3])
min(inc_group[grep(3, inc_group$group), 2])
max(inc_group[grep(3, inc_group$group), 2])

my_mean(inc_group[grep(4, inc_group$group), 3])
length(inc_group[grep(4, inc_group$group), 3])
min(inc_group[grep(4, inc_group$group), 2])
max(inc_group[grep(4, inc_group$group), 2])

my_mean(inc_group[grep(5, inc_group$group), 3])
length(inc_group[grep(5, inc_group$group), 3])
min(inc_group[grep(5, inc_group$group), 2])
max(inc_group[grep(5, inc_group$group), 2])

my_mean(inc_group[grep(6, inc_group$group), 3])
length(inc_group[grep(6, inc_group$group), 3])
min(inc_group[grep(6, inc_group$group), 2])
max(inc_group[grep(6, inc_group$group), 2])




# Ответ на первую компанию
cmp <- cbind(data$MntGoldProds, data$AcceptedCmp1)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "cmp1")
cmp[1:3, ]

mean(cmp[cmp$cmp1 == 1, 1])
length(cmp[cmp$cmp1 == 1, 1])


# Ответ на вторую компанию
cmp <- cbind(data$MntGoldProds, data$AcceptedCmp2)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "cmp2")
cmp[1:3, ]

my_mean(cmp[cmp$cmp2 == 1, 1])
length(cmp[cmp$cmp2 == 1, 1])

# Ответ на третью компанию
cmp <- cbind(data$MntGoldProds, data$AcceptedCmp3)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "cmp3")
cmp[1:3, ]

my_mean(cmp[cmp$cmp3 == 1, 1])
length(cmp[cmp$cmp3 == 1, 1])

# Ответ на четвертую компанию
cmp <- cbind(data$MntGoldProds, data$AcceptedCmp4)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "cmp4")
cmp[1:3, ]

my_mean(cmp[cmp$cmp4 == 1, 1])
length(cmp[cmp$cmp4 == 1, 1])

# Ответ на пятую компанию
cmp <- cbind(data$MntGoldProds, data$AcceptedCmp5)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "cmp5")
cmp[1:3, ]

my_mean(cmp[cmp$cmp5 == 1, 1])
length(cmp[cmp$cmp5 == 1, 1])
#
# Ответ на последнюю компанию
cmp <- cbind(data$MntGoldProds, data$Response)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "response")
cmp[1:3, ]

my_mean(cmp[cmp$response == 1, 1])
length(cmp[cmp$response == 1, 1])


#######################################################################################################################
#####################################################################################
#####################################################################################

# Как много PhD и Master без детей

sweets_educ <- cbind(data$Education, data$MntGoldProds, data$Kidhome)
sweets_educ <- data.frame(sweets_educ, stringsAsFactors = FALSE)
names(sweets_educ) <- c("educ", "sweets", "kids")
sweets_educ[1:3,]
master <- sweets_educ[grep("PhD", sweets_educ$educ), 2:3]
length(master[grep(0, master$kids), 1])

masta <- sweets_educ[grep("Master", sweets_educ$educ), 2:3]
length(masta[grep(0, masta$kids), 1])
length(masta[masta == 0])
mean(as.numeric(masta[grep(0, masta$kids), 1]))

phd <- sweets_educ[grep("PhD", sweets_educ$educ), 2:3]
length(phd[grep(0, phd$kids), 1])
mean(as.numeric(phd[grep(0, phd$kids), 1]))

#Покупка золота Master и PhD с подростками
sweets_educ <- cbind(data$Education, data$MntGoldProds, data$Teenhome)
sweets_educ <- data.frame(sweets_educ, stringsAsFactors = FALSE)
names(sweets_educ) <- c("educ", "sweets", "teen")
sweets_educ[1:3,]


masta <- sweets_educ[grep("Master", sweets_educ$educ), 2:3]
length(masta[grep(0, masta$teen), 1])
mean(as.numeric(masta[grep(0, masta$teen), 1]))

phd <- sweets_educ[grep("PhD", sweets_educ$educ), 2:3]
length(phd[grep(0, phd$teen), 1])
mean(as.numeric(phd[grep(0, phd$teen), 1]))



# Группа с зп 60+ без детей
short <- inc_group[inc_group$income > 60000, 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1]
tmp[1:30]

my_mean(tmp)

# Сравним со средними тратами на золото
my_mean(tmp) > my_mean(data$MntGoldProds)
length(tmp)



# Группа с зп 60+ без детей PhD
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$educ == "PhD", 1]
tmp[1:30]


my_mean(tmp)
my_mean(tmp) > my_mean(data$MntGoldProds)
length(tmp)


# Группа с зп 60+ без детей Cycle
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$educ == "2n Cycle", 1]
tmp[1:30]


my_mean(tmp)
my_mean(tmp) > my_mean(data$MntGoldProds)
length(tmp)

# Группа с зп 80+ без детей Cycle
short <- inc_group[inc_group$income > 80000, 3:6]
short[1:3, ]
tmp <- short[short$educ == "2n Cycle", 1]
tmp[1:30]


my_mean(tmp)
my_mean(tmp) > my_mean(data$MntGoldProds)
length(tmp)


# Группа с зп 60+ без детей Grad
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$educ == "Graduation", 1]
tmp[1:30]


my_mean(tmp)
my_mean(tmp) > my_mean(data$MntGoldProds)
length(tmp)

# Группа с зп 80+ без детей Grad
short <- inc_group[inc_group$income > 80000, 3:6]
short[1:3, ]
tmp <- short[short$educ == "Graduation", 1]
tmp[1:30]


my_mean(tmp)
my_mean(tmp) > my_mean(data$MntGoldProds)
length(tmp)



# Группа с зп 60+ с PhD
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "PhD", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntGoldProds)
length(tmp)


# Группа с зп 60+ с сайкл
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "2n Cycle", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntGoldProds)
length(tmp)


# Группа с зп 60+ с Grad
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "Graduation", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntGoldProds)
length(tmp) 


# Группа с зп 80+ с сайкл
short <- inc_group[inc_group$income > 80000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "2n Cycle", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntGoldProds)
length(tmp)


# Группа с зп 80+ с Grad
short <- inc_group[inc_group$income > 80000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "Graduation", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntGoldProds)
length(tmp) 

# Группа с PhD без детей
short <- inc_group[inc_group$educ == "PhD", 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntGoldProds)
length(tmp)



# Группа с Cycle без детей
short <- inc_group[inc_group$educ == "2n Cycle", 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntGoldProds)
length(tmp)


# Группа с Grad без детей
short <- inc_group[inc_group$educ == "Graduation", 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntGoldProds)
length(tmp)


# Группа с зп 60+ без детей старше 1970
short <- inc_group[inc_group$income > 60000, 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- tmp[tmp$year <= 1960, 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntGoldProds)
length(as.numeric(tmp))