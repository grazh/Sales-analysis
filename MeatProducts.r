#Meat products

#Meat от Education

Meat_educ <- cbind(data$Education, data$MntMeatProducts)
Meat_educ <- data.frame(Meat_educ, stringsAsFactors = FALSE)
names(Meat_educ) <- c("educ", "Meat")
Meat_educ[1:5,]
ed_group <- frame()
ed_group["Mean"] <- mean(as.numeric(Meat_educ$Meat))
ed_group["Cycle"] <- mean(as.numeric(Meat_educ[grep("Cycle", Meat_educ$educ), 2]))
ed_group["Basic"] <- mean(as.numeric(Meat_educ[grep("Basic", Meat_educ$educ), 2]))
ed_group["Graduation"] <- mean(as.numeric(Meat_educ[grep("Graduation", Meat_educ$educ), 2]))
ed_group["Master"] <- mean(as.numeric(Meat_educ[grep("Master", Meat_educ$educ), 2]))
ed_group["PhD"] <- mean(as.numeric(Meat_educ[grep("PhD", Meat_educ$educ), 2]))
ed_group

barplot(ed_group, width = 1, col = "blue")

#Выпускники больше всего тратятся на мясо (245.1), на втором месте PhD (233.8)

#Meat от kids

Meat_kids <- cbind(data$Kidhome, data$MntMeatProducts)
Meat_kids <- data.frame(Meat_kids, stringsAsFactors = FALSE)
names(Meat_kids) <- c("kids", "Meat")
Meat_kids[1:5,]

mean(Meat_kids[grep(0, Meat_kids$kids), 2])
mean(Meat_kids[grep(1, Meat_kids$kids), 2])
mean(Meat_kids[grep(2, Meat_kids$kids), 2])
Meat_kids <- scale(Meat_kids, center = FALSE)
Meat_kids[1:3,]
model1 <- neuralnet(data = Meat_kids, Meat ~ kids, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model1)


#Meat от teens

Meat_teen <- cbind(data$Teenhome, data$MntMeatProducts)
Meat_teen <- data.frame(Meat_teen, stringsAsFactors = FALSE)
names(Meat_teen) <- c("teen", "Meat")
Meat_teen[1:5,]

mean(Meat_teen[grep(0, Meat_teen$teen), 2])
mean(Meat_teen[grep(1, Meat_teen$teen), 2])
mean(Meat_teen[grep(2, Meat_teen$teen), 2])
sd(Meat_teen[grep(0, Meat_teen$teen), 2])
sd(Meat_teen[grep(1, Meat_teen$teen), 2])
sd(Meat_teen[grep(2, Meat_teen$teen), 2])

Meat_teen <- scale(Meat_teen, center = FALSE)
model2 <- neuralnet(data = Meat_teen, Meat ~ teen, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model2)

# Нейронка по Meat 

model2 <- neuralnet(data = data, MntMeatProducts ~ Kidhome + Teenhome + Recency, hidden = c(3, 3), threshold = 0.001, lifesign = "full")
plot(model2)


#Покупка Meat по году рождения

group <- data$Year_Birth
group[group<=1950] = 1
group[group<=1960 & group>=1951] = 2
group[group<=1970 & group>=1961] = 3
group[group<=1980 & group>=1971] = 4
group[group<=1990 & group>=1981] = 5
group[group<=2000 & group>=1991] = 6

group[1:100]

year_Meat <- cbind(group, data$Year_Birth, data$MntMeatProducts)
year_Meat <- data.frame(year_Meat)
mean(year_Meat[grep(1, year_Meat$group), 3])
length(year_Meat[grep(1, year_Meat$group), 3])
# Клиенты старше 1950 года (61 чел) тратят 282.9
mean(year_Meat[grep(2, year_Meat$group), 3])
length(year_Meat[grep(2, year_Meat$group), 3])
# Клиенты второй группы (239 челов) тратят 243.3
mean(year_Meat[grep(3, year_Meat$group), 3])
length(year_Meat[grep(3, year_Meat$group), 3])
# Клиенты 3 группы (271 чел) тратят 219.9
mean(year_Meat[grep(4, year_Meat$group), 3])
length(year_Meat[grep(4, year_Meat$group), 3])
# Клиенты 4 группы (345 челов) тратят 204.2
mean(year_Meat[grep(5, year_Meat$group), 3])
length(year_Meat[grep(5, year_Meat$group), 3])
# Клиенты 5 группы (166 челов) тратят 198.05
mean(year_Meat[grep(6, year_Meat$group), 3])
length(year_Meat[grep(6, year_Meat$group), 3])
# Клиенты 6 группы (23 чела) тратят 379.6



# Кластеры по зп


h1 <- hclust(dist(data$Income))
summary(h1)
plot(h1, hang = -1, main = "Иерархическая кластеризация по зарплате")
rect.hclust(h1, k = 6)
group <- cutree(h1, k = 6)
group[1:100]

inc_group <- data.frame(cbind(group, data$Income, data$MntMeatProducts, data$Kidhome, data$Year_Birth, data$Education), stringsAsFactors = FALSE)
names(inc_group) <- c("group", "income", "meat", "kids", "year", "educ")
inc_group[1:5, ]

my_mean <- function(x)
{
  return (mean(as.numeric(x)))
}


my_mean(inc_group[grep(1, inc_group$group), 3])
length(inc_group[grep(1, inc_group$group), 3])
min(inc_group[grep(1, inc_group$group), 2])
max(inc_group[grep(1, inc_group$group), 2])
#Первая группа с зп от 89058 до 105471 тратит на мясо 718.7 (26 человек)

my_mean(inc_group[grep(2, inc_group$group), 3])
length(inc_group[grep(2, inc_group$group), 3])
min(inc_group[grep(2, inc_group$group), 2])
max(inc_group[grep(2, inc_group$group), 2])
#Вторая группа с зп от 43456 до 60714 тратит на мясо 108.0913 (265 человек)

my_mean(inc_group[grep(3, inc_group$group), 3])
length(inc_group[grep(3, inc_group$group), 3])
min(inc_group[grep(3, inc_group$group), 2])
max(inc_group[grep(3, inc_group$group), 2])
#Третья группа с зп от 61010 до 88347 тратит на мясо 454.9967 (393 человек)

my_mean(inc_group[grep(4, inc_group$group), 3])
length(inc_group[grep(4, inc_group$group), 3])
min(inc_group[grep(4, inc_group$group), 2])
max(inc_group[grep(4, inc_group$group), 2])
#Четвертая группа с зп от 18100 до 43185 тратит на мясо 36.95514.84595 (370 человек)

my_mean(inc_group[grep(5, inc_group$group), 3])
length(inc_group[grep(5, inc_group$group), 3])
min(inc_group[grep(5, inc_group$group), 2])
max(inc_group[grep(5, inc_group$group), 2])
#Пятая группа с зп от 2447 до 17487 тратит на мясо 44.23404 (47 человек)

my_mean(inc_group[grep(6, inc_group$group), 3])
length(inc_group[grep(6, inc_group$group), 3])
min(inc_group[grep(6, inc_group$group), 2])
max(inc_group[grep(6, inc_group$group), 2])
#Шестая группа с зп от 153924 до 162397 тратит на мясо 45 (4 человека) 


# Ответ на первую компанию
cmp <- cbind(data$MntMeatProducts, data$AcceptedCmp1)
cmp <- data.frame(cmp)
names(cmp) <- c("meat", "cmp1")
cmp[1:3, ]

mean(cmp[cmp$cmp1 == 1, 1])
length(cmp[cmp$cmp1 == 1, 1])
# Клиенты ответившие на первую компанию (73 человека) тратят 550.5


# Ответ на вторую компанию
cmp <- cbind(data$MntMeatProducts, data$AcceptedCmp2)
cmp <- data.frame(cmp)
names(cmp) <- c("meat", "cmp2")
cmp[1:3, ]

mean(cmp[cmp$cmp2 == 1, 1])
length(cmp[cmp$cmp2 == 1, 1])
# Клиенты ответившие на вторую компанию (15 человек) тратят 340.34

# Ответ на третью компанию
cmp <- cbind(data$MntMeatProducts, data$AcceptedCmp3)
cmp <- data.frame(cmp)
names(cmp) <- c("meat", "cmp3")
cmp[1:3, ]

mean(cmp[cmp$cmp3 == 1, 1])
length(cmp[cmp$cmp3 == 1, 1])
# Клиенты ответившие на третью компанию (78 человек) тратят 215.9

# Ответ на четвертую компанию
cmp <- cbind(data$MntMeatProducts, data$AcceptedCmp4)
cmp <- data.frame(cmp)
names(cmp) <- c("meat", "cmp4")
cmp[1:3, ]

mean(cmp[cmp$cmp4 == 1, 1])
length(cmp[cmp$cmp4 == 1, 1])
# Клиенты ответившие на четвертую компанию (81 человек) тратят 333.0889

# Ответ на пятую компанию
cmp <- cbind(data$MntMeatProducts, data$AcceptedCmp5)
cmp <- data.frame(cmp)
names(cmp) <- c("meat", "cmp5")
cmp[1:3, ]

mean(cmp[cmp$cmp5 == 1, 1])
length(cmp[cmp$cmp5 == 1, 1])
# Клиенты ответившие на пятую компанию (79 человек) тратят 572.5595

# Ответ на последнюю компанию
cmp <- cbind(data$MntMeatProducts, data$Response)
cmp <- data.frame(cmp)
names(cmp) <- c("meat", "response")
cmp[1:3, ]

mean(cmp[cmp$response == 1, 1])
length(cmp[cmp$response == 1, 1])

# Клиенты ответившие на последнюю компанию (157 человек) тратят 360.7045


#####################################################################################
##########################################################################################################################################################################
#####################################################################################

# Как много PhD и Master без детей

Meat_educ <- cbind(data$Education, data$MntMeatProducts, data$Kidhome)
Meat_educ <- data.frame(Meat_educ, stringsAsFactors = FALSE)
names(Meat_educ) <- c("educ", "Meat", "kids")
Meat_educ[1:3,]
master <- Meat_educ[grep("PhD", Meat_educ$educ), 2:3]
length(master[grep(0, master$kids), 1])

masta <- Meat_educ[grep("Master", Meat_educ$educ), 2:3]
length(masta[grep(0, masta$kids), 1])
length(masta[masta == 0])
mean(as.numeric(masta[grep(0, masta$kids), 1]))

phd <- Meat_educ[grep("PhD", Meat_educ$educ), 2:3]
length(phd[grep(0, phd$kids), 1])
mean(as.numeric(phd[grep(0, phd$kids), 1]))


# Meat Master и PhD с подростками
Meat_educ <- cbind(data$Education, data$MntMeatProducts, data$Teenhome)
Meat_educ <- data.frame(Meat_educ, stringsAsFactors = FALSE)
names(Meat_educ) <- c("educ", "Meat", "teen")
Meat_educ[1:3,]


masta <- Meat_educ[grep("Master", Meat_educ$educ), 2:3]
length(masta[grep(0, masta$teen), 1])
mean(as.numeric(masta[grep(0, masta$teen), 1]))

phd <- Meat_educ[grep("PhD", Meat_educ$educ), 2:3]
length(phd[grep(0, phd$teen), 1])
mean(as.numeric(phd[grep(0, phd$teen), 1]))


# Группа с зп 60+ без детей
short <- inc_group[inc_group$income > 60000, 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1]
tmp[1:30]

my_mean(tmp)

# Сравним со средними тратами на мясо
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

# Группа с зп 60+ с PhD
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "PhD", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp)

# Группа с зп 80+ с PhD
short <- inc_group[inc_group$income > 80000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "PhD", 1]
tmp[1:10]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp) 


# Группа с зп 60+ с grad
short <- inc_group[inc_group$income > 60000, 3:6]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- short[short$educ == "Graduation", 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(tmp)

# Группа с зп 80+ с grad
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


# Группа с grad без детей
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
tmp <- tmp[tmp$year <= 1970, 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(as.numeric(tmp))


# Группа с зп 60+ без детей младше 1991
short <- inc_group[inc_group$income > 60000, 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- tmp[tmp$year >= 1991, 1]
tmp[1:10]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(as.numeric(tmp))


# Группа с зп 80+ без детей старше 1970
short <- inc_group[inc_group$income > 80000, 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- tmp[tmp$year <= 1970, 1]
tmp[1:30]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(as.numeric(tmp))


# Группа с зп 80+ без детей младше 1991
short <- inc_group[inc_group$income > 80000, 3:5]
short[1:3, ]
tmp <- short[short$kids == 0, 1:3]
tmp <- tmp[tmp$year >= 1991, 1]
tmp[1:5]

my_mean(tmp)
my_mean(tmp) > mean(data$MntMeatProducts)
length(as.numeric(tmp))

