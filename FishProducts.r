#Fish products

#Fish от Education

Fish_educ <- cbind(data$Education, data$MntFishProducts)
Fish_educ <- data.frame(Fish_educ, stringsAsFactors = FALSE)
names(Fish_educ) <- c("educ", "Fish")
Fish_educ[1:5,]
ed_group <- frame()
ed_group["Mean"] <- mean(as.numeric(Fish_educ$Fish))
ed_group["Cycle"] <- mean(as.numeric(Fish_educ[grep("Cycle", Fish_educ$educ), 2]))
ed_group["Basic"] <- mean(as.numeric(Fish_educ[grep("Basic", Fish_educ$educ), 2]))
ed_group["Graduation"] <- mean(as.numeric(Fish_educ[grep("Graduation", Fish_educ$educ), 2]))
ed_group["Master"] <- mean(as.numeric(Fish_educ[grep("Master", Fish_educ$educ), 2]))
ed_group["PhD"] <- mean(as.numeric(Fish_educ[grep("PhD", Fish_educ$educ), 2]))
ed_group

barplot(ed_group, width = 1, col = "blue")

#Cycle больше всего тратятся на рыбу (326.57), на втором месте выпускники (316.2)

#Fish от kids

Fish_kids <- cbind(data$Kidhome, data$MntFishProducts)
Fish_kids <- data.frame(Fish_kids, stringsAsFactors = FALSE)
names(Fish_kids) <- c("kids", "Fish")
Fish_kids[1:5,]

mean(Fish_kids[grep(0, Fish_kids$kids), 2])
mean(Fish_kids[grep(1, Fish_kids$kids), 2])
mean(Fish_kids[grep(2, Fish_kids$kids), 2])
data$Kidhome
Fish_kids <- scale(Fish_kids, center = FALSE)
Fish_kids
model1 <- neuralnet(data = Fish_kids, Fish ~ kids, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model1)


#Fish от teens

Fish_teen <- cbind(data$Teenhome, data$MntFishProducts)
Fish_teen <- data.frame(Fish_teen, stringsAsFactors = FALSE)
names(Fish_teen) <- c("teen", "Fish")
Fish_teen[1:5,]

mean(Fish_teen[grep(0, Fish_teen$teen), 2])
mean(Fish_teen[grep(1, Fish_teen$teen), 2])
mean(Fish_teen[grep(2, Fish_teen$teen), 2])
sd(Fish_teen[grep(0, Fish_teen$teen), 2])
sd(Fish_teen[grep(1, Fish_teen$teen), 2])
sd(Fish_teen[grep(2, Fish_teen$teen), 2])

Fish_teen <- scale(Fish_teen, center = FALSE)
model2 <- neuralnet(data = Fish_teen, Fish ~ teen, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model2)

# Как много PhD и Master без детей

Fish_educ <- cbind(data$Education, data$MntFishProducts, data$Kidhome)
Fish_educ <- data.frame(Fish_educ, stringsAsFactors = FALSE)
names(Fish_educ) <- c("educ", "Fish", "kids")
Fish_educ[1:3,]
master <- Fish_educ[grep("PhD", Fish_educ$educ), 2:3]
length(master[grep(0, master$kids), 1])
master[grep(0, master$kids), 1:2]


masta <- Fish_educ[grep("Master", Fish_educ$educ), 2:3]
length(masta[grep(0, masta$kids), 1])
length(masta[masta == 0])
mean(as.numeric(masta[grep(0, masta$kids), 1]))

phd <- Fish_educ[grep("PhD", Fish_educ$educ), 2:3]
phd
length(phd[grep(0, phd$kids), 1])
mean(as.numeric(phd[grep(0, phd$kids), 1]))

#Покупка рыбы Master и PhD с подростками
Fish_educ <- cbind(data$Education, data$MntFishProducts, data$Teenhome)
Fish_educ <- data.frame(Fish_educ, stringsAsFactors = FALSE)
names(Fish_educ) <- c("educ", "Fish", "teen")
Fish_educ[1:3,]


masta <- Fish_educ[grep("Master", Fish_educ$educ), 2:3]
length(masta[grep(0, masta$teen), 1])
masta[grep(0, masta$teen), 1]
mean(as.numeric(masta[grep(0, masta$teen), 1]))

phd <- Fish_educ[grep("PhD", Fish_educ$educ), 2:3]
phd
length(phd[grep(0, phd$teen), 1])
mean(as.numeric(phd[grep(0, phd$teen), 1]))


# Нейронка по рыбе

model2 <- neuralnet(data = data, MntFishProducts ~ Kidhome + Teenhome + Recency, hidden = c(3, 3), threshold = 0.001, lifesign = "full")
plot(model2)


#Покупка рыбы по году рождения

group <- data$Year_Birth
group[group<=1950] = 1
group[group<=1960 & group>=1951] = 2
group[group<=1970 & group>=1961] = 3
group[group<=1980 & group>=1971] = 4
group[group<=1990 & group>=1981] = 5
group[group<=2000 & group>=1991] = 6

group

year_Fish <- cbind(group, data$Year_Birth, data$MntFishProducts)
year_Fish <- data.frame(year_Fish)
mean(year_Fish[grep(1, year_Fish$group), 3])
length(year_Fish[grep(1, year_Fish$group), 3])
# Клиенты старше 1950 года (61 чел) тратят 316.8
mean(year_Fish[grep(2, year_Fish$group), 3])
length(year_Fish[grep(2, year_Fish$group), 3])
# Клиенты второй группы (239 челов) тратят 300.94
mean(year_Fish[grep(3, year_Fish$group), 3])
length(year_Fish[grep(3, year_Fish$group), 3])
# Клиенты 3 группы (271 чел) тратят 262.38
mean(year_Fish[grep(4, year_Fish$group), 3])
length(year_Fish[grep(4, year_Fish$group), 3])
# Клиенты 4 группы (345 челов) тратят 235.3
mean(year_Fish[grep(5, year_Fish$group), 3])
length(year_Fish[grep(5, year_Fish$group), 3])
# Клиенты 5 группы (166 челов) тратят 254.488
mean(year_Fish[grep(6, year_Fish$group), 3])
length(year_Fish[grep(6, year_Fish$group), 3])
# Клиенты 6 группы (23 чела) тратят 428.2



# Кластеры по зп


h1 <- hclust(dist(data$Income))
summary(h1)
plot(h1, hang = -1, main = "Иерархическая кластеризация по зарплате")
rect.hclust(h1, k = 6)
group <- cutree(h1, k = 6)
group

inc_group <- data.frame(cbind(group, data$Income, data$MntFishProducts))
inc_group[]

names(inc_group) <- c("group", "income", "Fish")
max(data$Income) # Минимальная зп 2447, максимальная 162397.



mean(inc_group[grep(1, inc_group$group), 3])
length(inc_group[grep(1, inc_group$group), 3])
min(inc_group[grep(1, inc_group$group), 2])
max(inc_group[grep(1, inc_group$group), 2])
#Первая группа с зп от 89058 до 105471 тратит на рыбу 684.92 (26 человек)

mean(inc_group[grep(2, inc_group$group), 3])
length(inc_group[grep(2, inc_group$group), 3])
min(inc_group[grep(2, inc_group$group), 2])
max(inc_group[grep(2, inc_group$group), 2])
#Вторая группа с зп от 43456 до 60714 тратит на рыбу 150.9887 (265 человек)

mean(inc_group[grep(3, inc_group$group), 3])
length(inc_group[grep(3, inc_group$group), 3])
min(inc_group[grep(3, inc_group$group), 2])
max(inc_group[grep(3, inc_group$group), 2])
#Третья группа с зп от 61010 до 88347 тратит на рыбу 531.6 (393 человек)

mean(inc_group[grep(4, inc_group$group), 3])
length(inc_group[grep(4, inc_group$group), 3])
min(inc_group[grep(4, inc_group$group), 2])
max(inc_group[grep(4, inc_group$group), 2])
#Четвертая группа с зп от 18100 до 43185 тратит на рыбу 71.286 (370 человек)

mean(inc_group[grep(5, inc_group$group), 3])
length(inc_group[grep(5, inc_group$group), 3])
min(inc_group[grep(5, inc_group$group), 2])
max(inc_group[grep(5, inc_group$group), 2])
#Пятая группа с зп от 2447 до 17487 тратит на рыбу 50.34 (47 человек)

mean(inc_group[grep(6, inc_group$group), 3])
length(inc_group[grep(6, inc_group$group), 3])
min(inc_group[grep(6, inc_group$group), 2])
max(inc_group[grep(6, inc_group$group), 2])
#Шестая группа с зп от 153924 до 162397 тратит на рыбу 36.75 (4 человека) 
