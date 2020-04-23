#Sweet products

#Sweets от Education

sweets_educ <- cbind(data$Education, data$MntSweetProducts)
sweets_educ <- data.frame(sweets_educ, stringsAsFactors = FALSE)
names(sweets_educ) <- c("educ", "sweets")
sweets_educ[1:5,]
ed_group <- frame()
ed_group["Mean"] <- mean(as.numeric(sweets_educ$sweets))
ed_group["Cycle"] <- mean(as.numeric(sweets_educ[grep("Cycle", sweets_educ$educ), 2]))
ed_group["Basic"] <- mean(as.numeric(sweets_educ[grep("Basic", sweets_educ$educ), 2]))
ed_group["Graduation"] <- mean(as.numeric(sweets_educ[grep("Graduation", sweets_educ$educ), 2]))
ed_group["Master"] <- mean(as.numeric(sweets_educ[grep("Master", sweets_educ$educ), 2]))
ed_group["PhD"] <- mean(as.numeric(sweets_educ[grep("PhD", sweets_educ$educ), 2]))
ed_group

barplot(ed_group, width = 1, col = "blue")

#Cycle больше всего тратятся на сладкое (234.6), на втором месте выпускники (229.77)

#Sweets от kids

sweets_kids <- cbind(data$Kidhome, data$MntSweetProducts)
sweets_kids <- data.frame(sweets_kids, stringsAsFactors = FALSE)
names(sweets_kids) <- c("kids", "sweets")
sweets_kids[1:5,]

mean(sweets_kids[grep(0, sweets_kids$kids), 2])
mean(sweets_kids[grep(1, sweets_kids$kids), 2])
mean(sweets_kids[grep(2, sweets_kids$kids), 2])
sweets_kids <- scale(sweets_kids, center = FALSE)
sweets_kids[1:3,]
model1 <- neuralnet(data = sweets_kids, sweets ~ kids, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model1)


#sweets от teens

sweets_teen <- cbind(data$Teenhome, data$MntSweetProducts)
sweets_teen <- data.frame(sweets_teen, stringsAsFactors = FALSE)
names(sweets_teen) <- c("teen", "sweets")
sweets_teen[1:5,]

mean(sweets_teen[grep(0, sweets_teen$teen), 2])
mean(sweets_teen[grep(1, sweets_teen$teen), 2])
mean(sweets_teen[grep(2, sweets_teen$teen), 2])
sd(sweets_teen[grep(0, sweets_teen$teen), 2])
sd(sweets_teen[grep(1, sweets_teen$teen), 2])
sd(sweets_teen[grep(2, sweets_teen$teen), 2])

sweets_teen <- scale(sweets_teen, center = FALSE)
model2 <- neuralnet(data = sweets_teen, sweets ~ teen, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model2)

# Нейронка по рыбе

model2 <- neuralnet(data = data, MntSweetProducts ~ Kidhome + Teenhome + Recency, hidden = c(3, 3), threshold = 0.001, lifesign = "full")
plot(model2)


#Покупка рыбы по году рождения

group <- data$Year_Birth
group[group<=1950] = 1
group[group<=1960 & group>=1951] = 2
group[group<=1970 & group>=1961] = 3
group[group<=1980 & group>=1971] = 4
group[group<=1990 & group>=1981] = 5
group[group<=2000 & group>=1991] = 6

group[1:100]

year_sweets <- cbind(group, data$Year_Birth, data$MntSweetProducts)
year_sweets <- data.frame(year_sweets)
mean(year_sweets[grep(1, year_sweets$group), 3])
length(year_sweets[grep(1, year_sweets$group), 3])
# Клиенты старше 1950 года (61 чел) тратят 189.2
mean(year_sweets[grep(2, year_sweets$group), 3])
length(year_sweets[grep(2, year_sweets$group), 3])
# Клиенты второй группы (239 челов) тратят 217.58
mean(year_sweets[grep(3, year_sweets$group), 3])
length(year_sweets[grep(3, year_sweets$group), 3])
# Клиенты 3 группы (271 чел) тратят 201.9
mean(year_sweets[grep(4, year_sweets$group), 3])
length(year_sweets[grep(4, year_sweets$group), 3])
# Клиенты 4 группы (345 челов) тратят 175.588
mean(year_sweets[grep(5, year_sweets$group), 3])
length(year_sweets[grep(5, year_sweets$group), 3])
# Клиенты 5 группы (166 челов) тратят 173.3
mean(year_sweets[grep(6, year_sweets$group), 3])
length(year_sweets[grep(6, year_sweets$group), 3])
# Клиенты 6 группы (23 чела) тратят 232.8



# Кластеры по зп


h1 <- hclust(dist(data$Income))
summary(h1)
plot(h1, hang = -1, main = "Иерархическая кластеризация по зарплате")
rect.hclust(h1, k = 6)
group <- cutree(h1, k = 6)
group[1:100]


inc_group <- data.frame(cbind(group, data$Income, data$MntSweetProducts, data$Kidhome, data$Year_Birth, data$Education), stringsAsFactors = FALSE)
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
#Первая группа с зп от 89058 до 105471 тратит на сладкое 581.5 (26 человек)

my_mean(inc_group[grep(2, inc_group$group), 3])
length(inc_group[grep(2, inc_group$group), 3])
min(inc_group[grep(2, inc_group$group), 2])
max(inc_group[grep(2, inc_group$group), 2])
#Вторая группа с зп от 43456 до 60714 тратит на сладкое 105.3 (265 человек)

my_mean(inc_group[grep(3, inc_group$group), 3])
length(inc_group[grep(3, inc_group$group), 3])
min(inc_group[grep(3, inc_group$group), 2])
max(inc_group[grep(3, inc_group$group), 2])
#Третья группа с зп от 61010 до 88347 тратит на сладкое 380.8 (393 человек)

my_mean(inc_group[grep(4, inc_group$group), 3])
length(inc_group[grep(4, inc_group$group), 3])
min(inc_group[grep(4, inc_group$group), 2])
max(inc_group[grep(4, inc_group$group), 2])
#Четвертая группа с зп от 18100 до 43185 тратит на сладкое 49.5 (370 человек)

my_mean(inc_group[grep(5, inc_group$group), 3])
length(inc_group[grep(5, inc_group$group), 3])
min(inc_group[grep(5, inc_group$group), 2])
max(inc_group[grep(5, inc_group$group), 2])
#Пятая группа с зп от 2447 до 17487 тратит на сладкое 40.5 (47 человек)

my_mean(inc_group[grep(6, inc_group$group), 3])
length(inc_group[grep(6, inc_group$group), 3])
min(inc_group[grep(6, inc_group$group), 2])
max(inc_group[grep(6, inc_group$group), 2])
#Шестая группа с зп от 153924 до 162397 тратит на сладкое 12.25 (4 человека) 




# Ответ на первую компанию
cmp <- cbind(data$MntSweetProducts, data$AcceptedCmp1)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "cmp1")
cmp[1:3, ]

mean(cmp[cmp$cmp1 == 1, 1])
length(cmp[cmp$cmp1 == 1, 1])
# Клиенты ответившие на первую компанию (73 человека) тратят 433.0411


# Ответ на вторую компанию
cmp <- cbind(data$MntSweetProducts, data$AcceptedCmp2)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "cmp2")
cmp[1:3, ]

my_mean(cmp[cmp$cmp2 == 1, 1])
length(cmp[cmp$cmp2 == 1, 1])
# Клиенты ответившие на вторую компанию (15 человек) тратят 269.2667

# Ответ на третью компанию
cmp <- cbind(data$MntSweetProducts, data$AcceptedCmp3)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "cmp3")
cmp[1:3, ]

my_mean(cmp[cmp$cmp3 == 1, 1])
length(cmp[cmp$cmp3 == 1, 1])
# Клиенты ответившие на третью компанию (78 человек) тратят 175.2592

# Ответ на четвертую компанию
cmp <- cbind(data$MntSweetProducts, data$AcceptedCmp4)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "cmp4")
cmp[1:3, ]

my_mean(cmp[cmp$cmp4 == 1, 1])
length(cmp[cmp$cmp4 == 1, 1])
# Клиенты ответившие на четвертую компанию (81 человек) тратят 252.8642

# Ответ на пятую компанию
cmp <- cbind(data$MntSweetProducts, data$AcceptedCmp5)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "cmp5")
cmp[1:3, ]

my_mean(cmp[cmp$cmp5 == 1, 1])
length(cmp[cmp$cmp5 == 1, 1])
# Клиенты ответившие на пятую компанию (79 человек) тратят 468.6456

# Ответ на последнюю компанию
cmp <- cbind(data$MntSweetProducts, data$Response)
cmp <- data.frame(cmp)
names(cmp) <- c("sweet", "response")
cmp[1:3, ]

my_mean(cmp[cmp$response == 1, 1])
length(cmp[cmp$response == 1, 1])

# Клиенты ответившие на последнюю компанию (157 человек) тратят 281.2484

#######################################################################################################################
#####################################################################################
#####################################################################################

# Как много PhD и Master без детей

sweets_educ <- cbind(data$Education, data$MntSweetProducts, data$Kidhome)
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

#Покупка сладкого Master и PhD с подростками
sweets_educ <- cbind(data$Education, data$MntSweetProducts, data$Teenhome)
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

# Сравним со средними тратами на сладкое
my_mean(tmp) > my_mean(data$MntMeatProducts)
length(tmp)



# Группа с зп 60+ без детей PhD
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$educ == "PhD", 1]
tmp[1:30]


my_mean(tmp)
my_mean(tmp) > my_mean(data$MntMeatProducts)
length(tmp)


# Группа с зп 60+ без детей Cycle
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$educ == "2n Cycle", 1]
tmp[1:30]


my_mean(tmp)
my_mean(tmp) > my_mean(data$MntMeatProducts)
length(tmp)

# Группа с зп 80+ без детей Cycle
short <- inc_group[inc_group$income > 80000, 3:6]
short[1:3, ]
tmp <- short[short$educ == "2n Cycle", 1]
tmp[1:9]


my_mean(tmp)
my_mean(tmp) > my_mean(data$MntMeatProducts)
length(tmp)


# Группа с зп 60+ без детей Grad
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$educ == "Graduation", 1]
tmp[1:30]


my_mean(tmp)
my_mean(tmp) > my_mean(data$MntMeatProducts)
length(tmp)

# Группа с зп 80+ без детей Grad
short <- inc_group[inc_group$income > 80000, 3:6]
short[1:3, ]
tmp <- short[short$educ == "Graduation", 1]
tmp[1:30]


my_mean(tmp)
my_mean(tmp) > my_mean(data$MntMeatProducts)
length(tmp)



# Группа с зп 60+ с PhD
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "PhD", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp)


# Группа с зп 60+ с сайкл
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "2n Cycle", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp)


# Группа с зп 60+ с Grad
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "Graduation", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp) 


# Группа с зп 80+ с сайкл
short <- inc_group[inc_group$income > 80000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "2n Cycle", 1]
tmp[1:5]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp)


# Группа с зп 80+ с Grad
short <- inc_group[inc_group$income > 80000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "Graduation", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp) 

# Группа с PhD без детей
short <- inc_group[inc_group$educ == "PhD", 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp)



# Группа с Cycle без детей
short <- inc_group[inc_group$educ == "2n Cycle", 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp)


# Группа с Grad без детей
short <- inc_group[inc_group$educ == "Graduation", 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp)


# Группа с зп 60+ без детей старше 1970
short <- inc_group[inc_group$income > 60000, 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- tmp[tmp$year <= 1960, 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(as.numeric(tmp))

