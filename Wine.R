
# Добавляем библиотеки, открываем файл


library(readxl)
library(neuralnet)
library(ggplot2)
data <- read_xlsx("data.xlsx")



# Wines от Education

wine_educ <- cbind(data$Education, data$MntWines)
wine_educ <- data.frame(wine_educ, stringsAsFactors = FALSE)
names(wine_educ) <- c("educ", "wine")
wine_educ[1:5,]
ed_group <- frame()
ed_group["Mean"] <- mean(as.numeric(wine_educ$wine))
ed_group["Cycle"] <- mean(as.numeric(wine_educ[grep("Cycle", wine_educ$educ), 2]))
ed_group["Basic"] <- mean(as.numeric(wine_educ[grep("Basic", wine_educ$educ), 2]))
ed_group["Graduation"] <- mean(as.numeric(wine_educ[grep("Graduation", wine_educ$educ), 2]))
ed_group["Master"] <- mean(as.numeric(wine_educ[grep("Master", wine_educ$educ), 2]))
ed_group["PhD"] <- mean(as.numeric(wine_educ[grep("PhD", wine_educ$educ), 2]))
ed_group

barplot(ed_group, width = 1, col = "blue")
# Больше всего расходы на вино у PhD и Master, далее с небольшим отрывом Graduate


# Wines от kids

wine_kids <- cbind(data$Kidhome, data$MntWines)
wine_kids <- data.frame(wine_kids, stringsAsFactors = FALSE)
names(wine_kids) <- c("kids", "wine")
wine_kids[1:5,]

mean(wine_kids[grep(0, wine_kids$kids), 2])
mean(wine_kids[grep(1, wine_kids$kids), 2])
mean(wine_kids[grep(2, wine_kids$kids), 2])
data$Kidhome
wine_kids <- scale(wine_kids, center = FALSE)
wine_kids
model1 <- neuralnet(data = wine_kids, wine ~ kids, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model1)
# больше всего на вино тратят клиенты без детей, на их фоне с 1 и 2 детьмя тратят в 4 и 7,5 раз меньше. Таким образом клиенты
# без детей нам предпочтительней для продажи вина. Нейросеть также ставит отрицательный коэфициент влиянию 
# количества детей на количество покупаемого вина, так что мы минимизиуем количество маленьких детей.


# Wines от teens

wine_teen <- cbind(data$Teenhome, data$MntWines)
wine_teen <- data.frame(wine_teen, stringsAsFactors = FALSE)
names(wine_teen) <- c("teen", "wine")
wine_teen[1:5,]

mean(wine_teen[grep(0, wine_teen$teen), 2])
mean(wine_teen[grep(1, wine_teen$teen), 2])
mean(wine_teen[grep(2, wine_teen$teen), 2])
sd(wine_teen[grep(0, wine_teen$teen), 2])
sd(wine_teen[grep(1, wine_teen$teen), 2])
sd(wine_teen[grep(2, wine_teen$teen), 2])
# Здесь предпочтительнее клиенты с 2мя подростками, но разница не большая.
wine_teen <- scale(wine_teen, center = FALSE)
model2 <- neuralnet(data = wine_teen, wine ~ teen, threshold = 0.001, lifesign = "full", hidden = 0)
plot(model2)
# вес подростков в количестве покупаемого вина стремится к 0

# Как много PhD и Master без детей

wine_educ <- cbind(data$Education, data$MntWines, data$Kidhome)
wine_educ <- data.frame(wine_educ, stringsAsFactors = FALSE)
names(wine_educ) <- c("educ", "wine", "kids")
wine_educ[1:3,]
master <- wine_educ[grep("PhD", wine_educ$educ), 2:3]
length(master[grep(0, master$kids), 1])
master[grep(0, master$kids), 1:2]


masta <- wine_educ[grep("Master", wine_educ$educ), 2:3]
length(masta[grep(0, masta$kids), 1])
length(masta[masta == 0])
mean(as.numeric(masta[grep(0, masta$kids), 1]))

phd <- wine_educ[grep("PhD", wine_educ$educ), 2:3]
phd
length(phd[grep(0, phd$kids), 1])
mean(as.numeric(phd[grep(0, phd$kids), 1]))

# в это половине 99 Master без детей со средними тратами на вино 480.7 и 157 PhD со средними тратами 584.8 без детей


# Вино Master и PhD с подростками
wine_educ <- cbind(data$Education, data$MntWines, data$Teenhome)
wine_educ <- data.frame(wine_educ, stringsAsFactors = FALSE)
names(wine_educ) <- c("educ", "wine", "teen")
wine_educ[1:3,]


masta <- wine_educ[grep("Master", wine_educ$educ), 2:3]
length(masta[grep(0, masta$teen), 1])
masta[grep(0, masta$teen), 1]
mean(as.numeric(masta[grep(0, masta$teen), 1]))

phd <- wine_educ[grep("PhD", wine_educ$educ), 2:3]
phd
length(phd[grep(0, phd$teen), 1])
mean(as.numeric(phd[grep(0, phd$teen), 1]))


# Нейронка по вину

model2 <- neuralnet(data = data, MntWines ~ Kidhome + Teenhome + Recency, hidden = c(3, 3), threshold = 0.001, lifesign = "full")
plot(model2)
# ошибка велика, то есть соответствие модели реальности очень условное и на количество приобретаемого вина что-то влияет сильнее,
# чем количество детей, подростков и дней с последней покупки


# покупки вина по году рождения

group <- data$Year_Birth
group[group<=1950] = 1
group[group<=1960 & group>=1951] = 2
group[group<=1970 & group>=1961] = 3
group[group<=1980 & group>=1971] = 4
group[group<=1990 & group>=1981] = 5
group[group<=2000 & group>=1991] = 6

group

year_wine <- cbind(group, data$Year_Birth, data$MntWines)
year_wine <- data.frame(year_wine)
mean(year_wine[grep(1, year_wine$group), 3])
length(year_wine[grep(1, year_wine$group), 3])
# Клиенты старше 1950 года (61 человек) тратят 472
mean(year_wine[grep(2, year_wine$group), 3])
length(year_wine[grep(2, year_wine$group), 3])
# Клиенты второй группы (239 человек) тратят 383
mean(year_wine[grep(3, year_wine$group), 3])
length(year_wine[grep(3, year_wine$group), 3])
# Клиенты 3 группы (271 человек) тартят 324
mean(year_wine[grep(4, year_wine$group), 3])
length(year_wine[grep(4, year_wine$group), 3])
# Клиенты 4 группы (345 человек) тартят 256
mean(year_wine[grep(5, year_wine$group), 3])
length(year_wine[grep(5, year_wine$group), 3])
# Клиенты 5 группы (166 человек) тратят 208
mean(year_wine[grep(6, year_wine$group), 3])
length(year_wine[grep(6, year_wine$group), 3])
# Клиенты 6 группы (23 человека) тратят 422



# Кластеры по зп


h1 <- hclust(dist(data$Income))
summary(h1)
plot(h1, hang = -1, main = "????????????? ????????????? ?? ????????")
rect.hclust(h1, k = 6)
group <- cutree(h1, k = 6)
group

inc_group <- data.frame(cbind(group, data$Income, data$MntWines))
inc_group[]

names(inc_group) <- c("group", "income", "wine")
max(data$Income) # ??????????? ???????? 2447, ???????????? 162397.



mean(inc_group[grep(1, inc_group$group), 3])
length(inc_group[grep(1, inc_group$group), 3])
min(inc_group[grep(1, inc_group$group), 2])
max(inc_group[grep(1, inc_group$group), 2])
# Первая группа с зп от 89058 до 105471 тратит на вино 819.8 (26 человек)

mean(inc_group[grep(2, inc_group$group), 3])
length(inc_group[grep(2, inc_group$group), 3])
min(inc_group[grep(2, inc_group$group), 2])
max(inc_group[grep(2, inc_group$group), 2])

mean(inc_group[grep(3, inc_group$group), 3])
length(inc_group[grep(3, inc_group$group), 3])
min(inc_group[grep(3, inc_group$group), 2])
max(inc_group[grep(3, inc_group$group), 2])

mean(inc_group[grep(4, inc_group$group), 3])
length(inc_group[grep(4, inc_group$group), 3])
min(inc_group[grep(4, inc_group$group), 2])
max(inc_group[grep(4, inc_group$group), 2])

mean(inc_group[grep(5, inc_group$group), 3])
length(inc_group[grep(5, inc_group$group), 3])
min(inc_group[grep(5, inc_group$group), 2])
max(inc_group[grep(5, inc_group$group), 2])

mean(inc_group[grep(6, inc_group$group), 3])
length(inc_group[grep(6, inc_group$group), 3])
min(inc_group[grep(6, inc_group$group), 2])
max(inc_group[grep(6, inc_group$group), 2])

mean(inc_group[grep(7, inc_group$group), 3])
length(inc_group[grep(7, inc_group$group), 3])
min(inc_group[grep(7, inc_group$group), 2])
max(inc_group[grep(7, inc_group$group), 2])

mean(inc_group[grep(8, inc_group$group), 3])
length(inc_group[grep(8, inc_group$group), 3])
min(inc_group[grep(8, inc_group$group), 2])
max(inc_group[grep(8, inc_group$group), 2])
