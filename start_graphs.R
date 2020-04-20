
# Добавляем библиотеки, открываем файл


library(readxl)
library(neuralnet)
library(ggplot2)
data <- read_xlsx("data.xlsx")


# Функция Normalize


normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}


# Распределение года рождения

hist(data$Year_Birth, col = "grey") 
# клиенты - преимущественно люди 1970 года рождения


# Распределение семейного положения


set_frame <- function(mat_stat)
{
    percents_mat <- frame()
    for (i in levels(mat_stat))
    {
      percents_mat[i] = length(mat_stat[mat_stat == i])/length(mat_stat)
    }
  return(percents_mat)
}


mat_stat <- factor(data$Marital_Status)
percents_mat <- set_frame(mat_stat)
percents_mat

barplot(percents_mat, width =1, col = "green", main = "Семейное положение клиентов")
sum(percents_mat)
# Позиция Alone статистически не значима, составляет менее 1%, большинство клиентов скорее всего
# имеют общий со своим партнером бюджет, тк доминируют Married - 37.5% и Together - 26%, Single + Divorced + Widow = 36.4%


# Распределение образования

data$Education[data$Education == "2n Cycle"] = "Cycle" # смысл эотго я объясню позже)
education <- factor(data$Education)
education
percents_edu <- set_matrix(education)
percents_edu
barplot(percents_edu, width = 1, col = "blue")
sum(percents_edu)
# Половина клиентов окончили университет, имеют ученую степень Graduation


# Средний заработок по уроню образования


count_mean <- function(data, percents_edu)
{
  wealth <- frame()
  income <- cbind(data$Income, data$Education)
  for (j in names(percents_edu))
  {
    wealth[j] <- 2*sum(as.numeric(income[income[,2] == j][1:(length(income[income[,2] == j])%/%2)]))/length(income[income[,2] == j])
    
  }
  return (wealth)
}

wealth <- count_mean(data, percents_edu)
wealth
barplot(wealth, width = 1, col = "red")
sum(wealth)


# Богатые группы


barplot(normalize(wealth * percents_edu), width = 1, col = "yellow") # средний заработок умноженный на относительное количество
# Клиенты с уровнем образования Basic держат гораздо меньший бюджет, чем люди с более высоким уровнем образования. Есть гипотеза,
# что они тратят критически мало денег в магазине.

# Анализ количества детей и подростков


kid_home <- data$Kidhome
qplot(y = kid_home,  geom = "histogram", main = "Количество маленьких детей", xlab = "наблюдений", ylab = "количество маленьких детей")

teen_home <- data$Teenhome
qplot(y = teen_home,  geom = "histogram", main = "Количество подростков", xlab = "наблюдений", ylab = "количество подростков")

qplot(y = kid_home + teen_home, geom = "histogram", main = "Количество детей", xlab = "наблюдений", ylab = "количество детей")
# Больштнство клментов имеют 1 ребенка(kid или teen), на втором месте находятся бездетные клиенты

# Recency

rececy <- data$Recency
hist(rececy) # ни о чем не говорит в отрыве от других данных


# Анализ трат

wines <- data$MntWines
mean(wines)
fruits <- data$MntFruits
mean(fruits)
meat <- data$MntMeatProducts
mean(meat)
fish <- data$MntFishProducts
mean(fish)
sweet <- data$MntSweetProducts
mean(sweet)
gold <- data$MntGoldProds
mean(gold)

expenditure <- cbind("Wines" = mean(wines), "Fruits" = mean(fruits), "Meat" = mean(meat), "Fish" = mean(fish),
                     "Sweet" = mean(sweet), "Gold" = mean(gold))
expenditure
barplot(expenditure, width = 1, main = "Средние траты на каждый тип товаров")

expenses <- cbind(data$MntWines, data$MntFruits, data$MntMeatProducts, data$MntFishProducts, data$MntSweetProducts, data$MntGoldProds)
expenses <- data.frame(expenses)
expenses
boxplot(x = expenses, y = 1, names = colnames(expenditure))
# В среднем траты на вино больше и кучнее(плотнее распределены), поэтому вино выглядит привлекательно, когда в дальнейшем
# будем находить связи, стоит обратить внимание на группу, которая больше всего тратит на вино. Далее идет рыба и золотые изделия.

# Responce rate

length(data$Response[data$Response == 1])/length(data$Response)

mean(data$Response)


# Динамика среднего по каждой научной степени по компаниям 1-5


count_mean_for_cmp <- function(test, name, n, graph)
{
  for (i in 2:7)
  {
    graph[i-1, n] = mean(as.numeric(as.character((test[grep(name, test$educ), i]))))
    
  }
  graph <- data.frame(graph)
  return(graph)
}


test <- cbind("educ" = data$Education, "cmp1" = data$AcceptedCmp1, "cmp2" = data$AcceptedCmp2, "cmp3" = data$AcceptedCmp3,
              "cmp4" = data$AcceptedCmp4, "cmp5" = data$AcceptedCmp5, "resp" = data$Response)
test <- data.frame(test)

educate_degrees <- c(levels(factor(data$Education)), "ALL", "Period")
educate_degrees

graph <- data.frame(row.names = c("cmp1", "cmp2", "cmp3", "cmp4", "cmp5", "resp"), stringsAsFactors = FALSE)

i = 1
for (names_e in educate_degrees){
  graph <- count_mean_for_cmp(test, names_e, i, graph)
  i = i + 1
}

colnames(graph) <- educate_degrees
graph$ALL[1] = sum(data$AcceptedCmp1)/length(data$AcceptedCmp1)
graph$ALL[2] = sum(data$AcceptedCmp2)/length(data$AcceptedCmp2)
graph$ALL[3] = sum(data$AcceptedCmp3)/length(data$AcceptedCmp3)
graph$ALL[4] = sum(data$AcceptedCmp4)/length(data$AcceptedCmp4)
graph$ALL[5] = sum(data$AcceptedCmp5)/length(data$AcceptedCmp5)
graph$ALL[6] = sum(data$Response)/length(data$Response)
for (i in 1:(dim(graph)[1]))
{
  graph$Period[i] = i
  print(i)
}
graph


ggplot(graph, aes(x = Period, y = Basic)) + geom_line(color = "blue") + geom_point(size = 1.5)
ggplot(graph, aes(x = Period, y = Graduation)) + geom_line(color = "red") + geom_point(size = 1.5)
ggplot(graph, aes(x = Period, y = Master)) + geom_line(color = "green") + geom_point(size = 1.5)
ggplot(graph, aes(x = Period, y = Cycle)) + geom_line(color = "pink") + geom_point(size = 1.5) # пригодилось изменеие 2n Cycle на 
# просто Cycle, так как к колонке в датафрейме нужно обращаться без пробелов
ggplot(graph, aes(x = Period, y = PhD)) + geom_line(color = "black") + geom_point(size = 1.5)
ggplot(graph, aes(x = Period, y = ALL)) + geom_line(color = "black") + geom_point(size = 1.5)
# На основе графиков иожно полагать, что магазин пробовал разного рода рекламу и учился на своих ошибках, так например во втором
# периоде количество ответов на рекламу упало у всех клиентов. Последняя реклама (ее эффект отражен в Response)
# оказалась самой эффективной. На основе деления клиентов на группы по уровню образования удалось заметить сильные отклонения
# от средней величины(Response) только у группы Basic


# Wines от Education

wine_educ <- cbind(data$Education, data$MntWines)
wine_educ <- data.frame(wine_educ, stringsAsFactors = FALSE)
names(wine_educ) <- c("educ", "wine")
wine_educ[1:5,]
mean(as.numeric(wine_educ$wine))
mean(as.numeric(wine_educ[grep("Basic", wine_educ$educ), 2]))
mean(as.numeric(wine_educ[grep("Graduation", wine_educ$educ), 2]))
mean(as.numeric(wine_educ[grep("Master", wine_educ$educ), 2]))
mean(as.numeric(wine_educ[grep("Cycle", wine_educ$educ), 2]))
mean(as.numeric(wine_educ[grep("PhD", wine_educ$educ), 2]))
# Больше всего расходы на вино у PhD и Master, далее с небольшим отрывом

# Как дети влияют на response

kids <- cbind(data$Kidhome, data$Response, data$MntWines)
kids <- data.frame(kids)
kids[1:5,]

mean(kids[grep(1, kids$child), 3])
mean(kids[grep(2, kids$child), 3])
mean(kids[grep(0, kids$child), 3])
# таким образом, у клиентов с 2-мя детьми количество ответов на рекламу = 0, так что они 
names(kids) <- c("child", "resp", "wine")
