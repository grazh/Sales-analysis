#Попытка сделать Fruits

# Fruits от Education

fruits_educ <- cbind(data$Education, data$MntFruits)
fruits_educ <- data.frame(fruits_educ, stringsAsFactors = FALSE)
names(fruits_educ) <- c("educ", "fruits")
fruits_educ[1:5,]
ed_group <- frame()
ed_group["Mean"] <- mean(as.numeric(fruits_educ$fruits))
ed_group["Cycle"] <- mean(as.numeric(fruits_educ[grep("Cycle", fruits_educ$educ), 2]))
ed_group["Basic"] <- mean(as.numeric(fruits_educ[grep("Basic", fruits_educ$educ), 2]))
ed_group["Graduation"] <- mean(as.numeric(fruits_educ[grep("Graduation", fruits_educ$educ), 2]))
ed_group["Master"] <- mean(as.numeric(fruits_educ[grep("Master", fruits_educ$educ), 2]))
ed_group["PhD"] <- mean(as.numeric(fruits_educ[grep("PhD", fruits_educ$educ), 2]))
ed_group

barplot(ed_group, width = 1, col = "blue")

#Походу выпускники больше всего тратятся на фрукты, на втором месте снова 2сайкл

#Fruits от kids

fruits_kids <- cbind(data$Kidhome, data$MntFruits)
fruits_kids <- data.frame(fruits_kids, stringsAsFactors = FALSE)
names(fruits_kids) <- c("kids", "fruits")
fruits_kids[1:5,]

mean(fruits_kids[grep(0, fruits_kids$kids), 2])
mean(fruits_kids[grep(1, fruits_kids$kids), 2])
mean(fruits_kids[grep(2, fruits_kids$kids), 2])
data$Kidhome
fruits_kids <- scale(fruits_kids, center = FALSE)
fruits_kids
model1 <- neuralnet(data = fruits_kids, fruits ~ kids, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model1)


#Fruits от teens

fruits_teen <- cbind(data$Teenhome, data$MntFruits)
fruits_teen <- data.frame(fruits_teen, stringsAsFactors = FALSE)
names(fruits_teen) <- c("teen", "fruits")
fruits_teen[1:5,]

mean(fruits_teen[grep(0, fruits_teen$teen), 2])
mean(fruits_teen[grep(1, fruits_teen$teen), 2])
mean(fruits_teen[grep(2, fruits_teen$teen), 2])
sd(fruits_teen[grep(0, fruits_teen$teen), 2])
sd(fruits_teen[grep(1, fruits_teen$teen), 2])
sd(fruits_teen[grep(2, fruits_teen$teen), 2])

fruits_teen <- scale(fruits_teen, center = FALSE)
model2 <- neuralnet(data = fruits_teen, fruits ~ teen, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model2)

# Как много PhD и Master без детей

fruits_educ <- cbind(data$Education, data$MntFruits, data$Kidhome)
fruits_educ <- data.frame(fruits_educ, stringsAsFactors = FALSE)
names(fruits_educ) <- c("educ", "fruits", "kids")
fruits_educ[1:3,]
master <- fruits_educ[grep("PhD", fruits_educ$educ), 2:3]
length(master[grep(0, master$kids), 1])
master[grep(0, master$kids), 1:2]


masta <- fruits_educ[grep("Master", fruits_educ$educ), 2:3]
length(masta[grep(0, masta$kids), 1])
length(masta[masta == 0])
mean(as.numeric(masta[grep(0, masta$kids), 1]))

phd <- fruits_educ[grep("PhD", fruits_educ$educ), 2:3]
phd
length(phd[grep(0, phd$kids), 1])
mean(as.numeric(phd[grep(0, phd$kids), 1]))

# fruits Master и PhD с подростками
fruits_educ <- cbind(data$Education, data$MntFruits, data$Teenhome)
fruits_educ <- data.frame(fruits_educ, stringsAsFactors = FALSE)
names(fruits_educ) <- c("educ", "fruits", "teen")
fruits_educ[1:3,]


masta <- fruits_educ[grep("Master", fruits_educ$educ), 2:3]
length(masta[grep(0, masta$teen), 1])
masta[grep(0, masta$teen), 1]
mean(as.numeric(masta[grep(0, masta$teen), 1]))

phd <- fruits_educ[grep("PhD", fruits_educ$educ), 2:3]
phd
length(phd[grep(0, phd$teen), 1])
mean(as.numeric(phd[grep(0, phd$teen), 1]))


# Нейронка по Fruits 

model2 <- neuralnet(data = data, MntFruits ~ Kidhome + Teenhome + Recency, hidden = c(3, 3), threshold = 0.001, lifesign = "full")
plot(model2)


#Покупка Fruits по году рождения

group <- data$Year_Birth
group[group<=1950] = 1
group[group<=1960 & group>=1951] = 2
group[group<=1970 & group>=1961] = 3
group[group<=1980 & group>=1971] = 4
group[group<=1990 & group>=1981] = 5
group[group<=2000 & group>=1991] = 6

group

year_fruits <- cbind(group, data$Year_Birth, data$MntFruits)
year_fruits <- data.frame(year_fruits)
mean(year_fruits[grep(1, year_fruits$group), 3])
length(year_fruits[grep(1, year_fruits$group), 3])
# Клиенты старше 1950 года (61 чел) тратят 255
mean(year_fruits[grep(2, year_fruits$group), 3])
length(year_fruits[grep(2, year_fruits$group), 3])
# Клиенты второй группы (239 челов) тратят 233
mean(year_fruits[grep(3, year_fruits$group), 3])
length(year_fruits[grep(3, year_fruits$group), 3])
# Клиенты 3 группы (271 чел) третят 258
mean(year_fruits[grep(4, year_fruits$group), 3])
length(year_fruits[grep(4, year_fruits$group), 3])
# Клиенты 4 группы (345 челов) тратят 209
mean(year_fruits[grep(5, year_fruits$group), 3])
length(year_fruits[grep(5, year_fruits$group), 3])
# Клиенты 5 группы (166 челов) тратят 216
mean(year_fruits[grep(6, year_fruits$group), 3])
length(year_fruits[grep(6, year_fruits$group), 3])
# Клиенты 6 группы (23 чела) тратят 324



# Кластеры по зп


h1 <- hclust(dist(data$Income))
summary(h1)
plot(h1, hang = -1, main = "Иерархическая кластеризация по зарплате")
rect.hclust(h1, k = 6)
group <- cutree(h1, k = 6)
group

inc_group <- data.frame(cbind(group, data$Income, data$MntFruits))
inc_group[]

names(inc_group) <- c("group", "income", "fruits")
max(data$Income) # Минимальная зп 2447, максимальная 162397.



mean(inc_group[grep(1, inc_group$group), 3])
length(inc_group[grep(1, inc_group$group), 3])
min(inc_group[grep(1, inc_group$group), 2])
max(inc_group[grep(1, inc_group$group), 2])
#Первая группа с зп от 89058 до 105471 тратит на фрукты 671.53 (26 человек)

mean(inc_group[grep(2, inc_group$group), 3])
length(inc_group[grep(2, inc_group$group), 3])
min(inc_group[grep(2, inc_group$group), 2])
max(inc_group[grep(2, inc_group$group), 2])
#Вторая группа с зп от 43456 до 60714 тратит на фрукты 128.64 (265 человек)

mean(inc_group[grep(3, inc_group$group), 3])
length(inc_group[grep(3, inc_group$group), 3])
min(inc_group[grep(3, inc_group$group), 2])
max(inc_group[grep(3, inc_group$group), 2])
#Третья группа с зп от 61010 до 88347 тратит на фрукты 464.3359 (393 человек)

mean(inc_group[grep(4, inc_group$group), 3])
length(inc_group[grep(4, inc_group$group), 3])
min(inc_group[grep(4, inc_group$group), 2])
max(inc_group[grep(4, inc_group$group), 2])
#Четвертая группа с зп от 18100 до 43185 тратит на фрукты 56.84595 (370 человек)

mean(inc_group[grep(5, inc_group$group), 3])
length(inc_group[grep(5, inc_group$group), 3])
min(inc_group[grep(5, inc_group$group), 2])
max(inc_group[grep(5, inc_group$group), 2])
#Пятая группа с зп от 2447 до 17487 тратит на фрукты 44.23404 (47 человек)

mean(inc_group[grep(6, inc_group$group), 3])
length(inc_group[grep(6, inc_group$group), 3])
min(inc_group[grep(6, inc_group$group), 2])
max(inc_group[grep(6, inc_group$group), 2])
#Шестая группа с зп от 153924 до 162397 тратит на фрукты 45 (4 человека) 
