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
data$Kidhome
Meat_kids <- scale(Meat_kids, center = FALSE)
Meat_kids
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

# Как много PhD и Master без детей

Meat_educ <- cbind(data$Education, data$MntMeatProducts, data$Kidhome)
Meat_educ <- data.frame(Meat_educ, stringsAsFactors = FALSE)
names(Meat_educ) <- c("educ", "Meat", "kids")
Meat_educ[1:3,]
master <- Meat_educ[grep("PhD", Meat_educ$educ), 2:3]
length(master[grep(0, master$kids), 1])
master[grep(0, master$kids), 1:2]


masta <- Meat_educ[grep("Master", Meat_educ$educ), 2:3]
length(masta[grep(0, masta$kids), 1])
length(masta[masta == 0])
mean(as.numeric(masta[grep(0, masta$kids), 1]))

phd <- Meat_educ[grep("PhD", Meat_educ$educ), 2:3]
phd
length(phd[grep(0, phd$kids), 1])
mean(as.numeric(phd[grep(0, phd$kids), 1]))

# Meat Master и PhD с подростками
Meat_educ <- cbind(data$Education, data$MntMeatProducts, data$Teenhome)
Meat_educ <- data.frame(Meat_educ, stringsAsFactors = FALSE)
names(Meat_educ) <- c("educ", "Meat", "teen")
Meat_educ[1:3,]


masta <- Meat_educ[grep("Master", Meat_educ$educ), 2:3]
length(masta[grep(0, masta$teen), 1])
masta[grep(0, masta$teen), 1]
mean(as.numeric(masta[grep(0, masta$teen), 1]))

phd <- Meat_educ[grep("PhD", Meat_educ$educ), 2:3]
phd
length(phd[grep(0, phd$teen), 1])
mean(as.numeric(phd[grep(0, phd$teen), 1]))


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

group

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
group

inc_group <- data.frame(cbind(group, data$Income, data$MntMeatProducts))
inc_group[]

names(inc_group) <- c("group", "income", "Meat")
max(data$Income) # Минимальная зп 2447, максимальная 162397.



mean(inc_group[grep(1, inc_group$group), 3])
length(inc_group[grep(1, inc_group$group), 3])
min(inc_group[grep(1, inc_group$group), 2])
max(inc_group[grep(1, inc_group$group), 2])
#Первая группа с зп от 89058 до 105471 тратит на мясо 671.53 (26 человек)

mean(inc_group[grep(2, inc_group$group), 3])
length(inc_group[grep(2, inc_group$group), 3])
min(inc_group[grep(2, inc_group$group), 2])
max(inc_group[grep(2, inc_group$group), 2])
#Вторая группа с зп от 43456 до 60714 тратит на мясо 128.64 (265 человек)

mean(inc_group[grep(3, inc_group$group), 3])
length(inc_group[grep(3, inc_group$group), 3])
min(inc_group[grep(3, inc_group$group), 2])
max(inc_group[grep(3, inc_group$group), 2])
#Третья группа с зп от 61010 до 88347 тратит на мясо 464.3359 (393 человек)

mean(inc_group[grep(4, inc_group$group), 3])
length(inc_group[grep(4, inc_group$group), 3])
min(inc_group[grep(4, inc_group$group), 2])
max(inc_group[grep(4, inc_group$group), 2])
#Четвертая группа с зп от 18100 до 43185 тратит на мясо 56.84595 (370 человек)

mean(inc_group[grep(5, inc_group$group), 3])
length(inc_group[grep(5, inc_group$group), 3])
min(inc_group[grep(5, inc_group$group), 2])
max(inc_group[grep(5, inc_group$group), 2])
#Пятая группа с зп от 2447 до 17487 тратит на мясо 44.23404 (47 человек)

mean(inc_group[grep(6, inc_group$group), 3])
length(inc_group[grep(6, inc_group$group), 3])
min(inc_group[grep(6, inc_group$group), 2])
max(inc_group[grep(6, inc_group$group), 2])
#Шестая группа с зп от 153924 до 162397 тратит на мясо 45 (4 человека) 
