setwd("/Users/viktoriazajceva/Desktop/R/20 Проект")
library(tidyverse)
library(stringr)

#=========Часть 1: Подготовка данных =========

###### СБЕР ###### 
setwd("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные/СберИндекс")

###### Безналичные платежи ######
transactions_Sber <- read.csv("Доля безналичных платежей в торговом обороте.csv",sep=";")

transactions_Sber <- transactions_Sber %>% separate(col = Дата, into = c("Год", "Месяц", NA), sep = "-")

transactions_Sber$Месяц <- as.numeric(transactions_Sber$Месяц)
transactions_Sber$Год <- as.numeric(transactions_Sber$Год)

transactions_Sber <- transactions_Sber %>%
  mutate(Месяц = case_when(
    Месяц == 1 ~ "Январь",
    Месяц == 2 ~ "Февраль",
    Месяц == 3 ~ "Март",
    Месяц == 4 ~ "Апрель",
    Месяц == 5 ~ "Май",
    Месяц == 6 ~ "Июнь",
    Месяц == 7 ~ "Июль",
    Месяц == 8 ~ "Август",
    Месяц == 9 ~ "Сентябрь",
    Месяц == 10 ~ "Октябрь",
    Месяц == 11 ~ "Ноябрь",
    Месяц == 12 ~ "Декабрь"
      ))


transactions_Sber <- transactions_Sber[transactions_Sber$Регион != "Россия", ]
transactions_Sber <- transactions_Sber[transactions_Sber$Год == 2020,]
transactions_Sber <- transactions_Sber %>% mutate(Регион = str_to_title(Регион))

# проверяю, какие названия регионов не совпадают между сбер и домклик
unique(transactions_Sber$Регион)
unique(loan_and_mortgage$Регион)

sber_reg <- unique(transactions_Sber$Регион)
dom_reg <- unique(loan_and_mortgage$Регион)


for (i in sber_reg){
  if (!(i %in% dom_reg)) {
    print(i)
  }
}

# переименовываю различающиеся
transactions_Sber <- transactions_Sber %>%
  mutate(Регион = case_when(
    Регион == "Адыгея" ~ "Республика Адыгея",
    Регион == "Алтай" ~ "Республика Алтай",
    Регион == "Ханты-Мансийский Ао - Югра" ~ "Ханты-Мансийский Автономный Округ - Югра",
    Регион == "Ненецкий Ао" ~ "Ненецкий Автономный Округ",
    Регион == "Ямало-Ненецкий Ао" ~ "Ямало-Ненецкий Автономный Округ",
    Регион == "Чукотский Ао" ~ "Чукотский Автономный Округ",
    Регион == "Мордовия" ~ "Республика Мордовия",
    Регион == "Республика Карачаево-Черкессия" ~ "Карачаево-Черкесская Республика",
    Регион == "Республика Северная Осетия-Алания" ~ "Республика Северная Осетия - Алания",
    TRUE ~ Регион
  ))

# что отсутствует в сбере transactions_Sber?
sber_reg <- unique(transactions_Sber$Регион)
for (i in dom_reg){
  if (!(i %in% sber_reg)) {
    print(i)
  }
}
# в СБЕР нет Московской и Ленинградской областей -> на 2 меньше

month_order1 <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", 
                  "Ноябрь", "Декабрь")
transactions_Sber <- transactions_Sber %>% mutate(Месяц = factor(Месяц, levels = month_order1))

transactions_Sber <- transactions_Sber %>% arrange(Регион, Месяц)

colnames(transactions_Sber)[4] <- "Индекс.БП"

write.csv(transactions_Sber, "Безналичные платежи_final.csv")


#### туризм ####
tourists_Sber <- read.csv("Количество внутренних туристов.csv",sep=";")
tourists_Sber <- tourists_Sber %>% separate(col = Дата, into = c("Год", "Месяц", NA), sep = "-")

tourists_Sber$Месяц <- as.numeric(tourists_Sber$Месяц)
tourists_Sber$Год <- as.numeric(tourists_Sber$Год)

tourists_Sber <- tourists_Sber %>%
  mutate(Месяц = case_when(
    Месяц == 1 ~ "Январь",
    Месяц == 2 ~ "Февраль",
    Месяц == 3 ~ "Март",
    Месяц == 4 ~ "Апрель",
    Месяц == 5 ~ "Май",
    Месяц == 6 ~ "Июнь",
    Месяц == 7 ~ "Июль",
    Месяц == 8 ~ "Август",
    Месяц == 9 ~ "Сентябрь",
    Месяц == 10 ~ "Октябрь",
    Месяц == 11 ~ "Ноябрь",
    Месяц == 12 ~ "Декабрь"
  ))


tourists_Sber <- tourists_Sber[tourists_Sber$Регион != "Россия", ]
tourists_Sber <- tourists_Sber[tourists_Sber$Год == 2020,]
tourists_Sber <- tourists_Sber %>% mutate(Регион = str_to_title(Регион))

# проверяю, какие названия регионов не совпадают между сбер и домклик
unique(tourists_Sber$Регион) #82
unique(loan_and_mortgage$Регион) #83

sber_reg2 <- unique(tourists_Sber$Регион)
dom_reg <- unique(loan_and_mortgage$Регион)


for (i in sber_reg2){
  if (!(i %in% dom_reg)) {
    print(i)
  }
}

# переименовываю различающиеся
tourists_Sber <- tourists_Sber %>%
  mutate(Регион = case_when(
    Регион == "Адыгея" ~ "Республика Адыгея",
    Регион == "Алтай" ~ "Республика Алтай",
    Регион == "Ханты-Мансийский Ао - Югра" ~ "Ханты-Мансийский Автономный Округ - Югра",
    Регион == "Ненецкий Ао" ~ "Ненецкий Автономный Округ",
    Регион == "Ямало-Ненецкий Ао" ~ "Ямало-Ненецкий Автономный Округ",
    Регион == "Чукотский Ао" ~ "Чукотский Автономный Округ",
    Регион == "Мордовия" ~ "Республика Мордовия",
    Регион == "Республика Карачаево-Черкессия" ~ "Карачаево-Черкесская Республика",
    Регион == "Республика Северная Осетия-Алания" ~ "Республика Северная Осетия - Алания",
    TRUE ~ Регион
  ))

# что отсутствует в сбере tourists_Sber?
sber_reg2 <- unique(tourists_Sber$Регион)
for (i in dom_reg){
  if (!(i %in% sber_reg2)) {
    print(i)
  }
}
# отсутствует Ненецкий Автономный Округ

month_order1 <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", 
                  "Ноябрь", "Декабрь")
tourists_Sber <- tourists_Sber %>% mutate(Месяц = factor(Месяц, levels = month_order1))

tourists_Sber <- tourists_Sber %>% arrange(Регион, Месяц)

colnames(tourists_Sber)[4] <- "Индекс.Т" #туризм

write.csv(tourists_Sber, "Внутренний туризм_final.csv")

transactions_and_tourism <- full_join(transactions_Sber,tourists_Sber)
transactions_and_tourism <- transactions_and_tourism %>% arrange(Регион, Месяц) #московская и ленинградская области ушли в конец


#### потребительская активность #### 
# csv ФАЙЛ НЕ ЧИТАЕТСЯ
library(readxl) 
activity_Sber <- read_excel("Индекс потребительской активности.xlsx")

activity_Sber <- activity_Sber %>% separate(col = Дата, into = c("Год", "Месяц", NA), sep = "-")

activity_Sber$Месяц <- as.numeric(activity_Sber$Месяц)
activity_Sber$Год <- as.numeric(activity_Sber$Год)

activity_Sber <- activity_Sber %>%
  mutate(Месяц = case_when(
    Месяц == 1 ~ "Январь",
    Месяц == 2 ~ "Февраль",
    Месяц == 3 ~ "Март",
    Месяц == 4 ~ "Апрель",
    Месяц == 5 ~ "Май",
    Месяц == 6 ~ "Июнь",
    Месяц == 7 ~ "Июль",
    Месяц == 8 ~ "Август",
    Месяц == 9 ~ "Сентябрь",
    Месяц == 10 ~ "Октябрь",
    Месяц == 11 ~ "Ноябрь",
    Месяц == 12 ~ "Декабрь"
  ))


activity_Sber <- activity_Sber[activity_Sber$Регион != "Россия", ]
activity_Sber <- activity_Sber[activity_Sber$Год == 2020,]
activity_Sber <- activity_Sber %>% mutate(Регион = str_to_title(Регион))

# проверяю, какие названия регионов не совпадают между сбер и домклик
unique(activity_Sber$Регион) #76
unique(loan_and_mortgage$Регион) #83

sber_reg3 <- unique(activity_Sber$Регион)
dom_reg <- unique(loan_and_mortgage$Регион)


for (i in sber_reg3){
  if (!(i %in% dom_reg)) {
    print(i)
  }
}

# переименовываю различающиеся
activity_Sber <- activity_Sber %>%
  mutate(Регион = case_when(
    Регион == "Адыгея" ~ "Республика Адыгея",
    Регион == "Алтай" ~ "Республика Алтай",
    Регион == "Ханты-Мансийский Ао - Югра" ~ "Ханты-Мансийский Автономный Округ - Югра",
    Регион == "Ненецкий Ао" ~ "Ненецкий Автономный Округ",
    Регион == "Ямало-Ненецкий Ао" ~ "Ямало-Ненецкий Автономный Округ",
    Регион == "Чукотский Ао" ~ "Чукотский Автономный Округ",
    Регион == "Мордовия" ~ "Республика Мордовия",
    Регион == "Республика Карачаево-Черкессия" ~ "Карачаево-Черкесская Республика",
    Регион == "Республика Северная Осетия-Алания" ~ "Республика Северная Осетия - Алания",
    TRUE ~ Регион
  ))

# что отсутствует в сбере tourists_Sber?
sber_reg3 <- unique(activity_Sber$Регион)
for (i in dom_reg){
  if (!(i %in% sber_reg3)) {
    print(i)
  }
}
# Отсутствует 7: "Еврейская Автономная Область","Ленинградская Область", "Ненецкий Автономный Округ", "Республика Алтай", 
#"Республика Ингушетия", "Чеченская Республика","Чукотский Автономный Округ"

month_order1 <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", 
                  "Ноябрь", "Декабрь")
activity_Sber <- activity_Sber %>% mutate(Месяц = factor(Месяц, levels = month_order1))

activity_Sber <- activity_Sber %>% arrange(Регион, Месяц)



colnames(activity_Sber)[4] <- "Индекс.А" #туризм
activity_Sber$Индекс.А <- as.numeric(activity_Sber$Индекс.А)

# так как данные по дням, нужно посчитать медианы для каждого месяца

activity_Sber_grouped <- activity_Sber %>% group_by(Месяц, Регион) %>% summarise(N=n(),
                                                                         "Месячный Индекс.А" = median(Индекс.А))
# правильно, так как для каждого месяца 76 регионов и 30/31 наблюдений (дней), для февраля 29
activity_Sber_grouped <- activity_Sber_grouped %>% arrange(Регион, Месяц)

activity_Sber_grouped <- activity_Sber_grouped[,-3] #удаляю N
 
setwd("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные/СберИндекс") 
write.csv(activity_Sber_grouped, "Потребительская активность_final.csv")

#### Объединение датафреймов Сбера ####
Sber_full <- full_join(transactions_Sber,tourists_Sber) %>% full_join(activity_Sber_grouped)


#### ДОМКЛИК ####
#### Рейтинг регионов по количеству заявок на кредит####
setwd("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные/ДомКлик/Рейтинг регионов по количеству заявок на кредит")
library(readxl)

# все excel файлы должны быть закрыты
file_list <- list.files(pattern = "\\.xlsx$") #так как в эту же папку сохраняю полученный датафрейм csv
data_list <- list()
 
for (file in file_list) {
  clean_name <- sub("\\.xlsx$", "", file)
  data <- read_excel(file)
  data_list[[clean_name]] <- data
}

data_list # список с 12 датафреймами
# связать все строки из списка в один датафрейм
loan_applications <- bind_rows(data_list, .id = "filename") # аргумент .id = "filename" добавляет столбец filename с источником (названием исходного файла) данных

loan_applications <- loan_applications[,-c(2:3, 8:10)]

loan_applications <- loan_applications %>% separate(col = filename, into = c("Месяц", "Год"), sep = " ")
unique(loan_applications$Месяц)
loan_applications <- loan_applications %>% mutate(Месяц = str_to_title(Месяц))

#сортировка регионов по алфавиту и месяцев по порядку 

unique(loan_applications$Месяц)

month_order1 <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", 
                  "Ноябрь", "Декабрь")

loan_applications <- loan_applications %>% mutate(Месяц = factor(Месяц, levels = month_order1))

loan_applications <- loan_applications %>% arrange(Регион, Месяц)

loan_applications <- na.omit(loan_applications)

# все месяца и регионы с заглавной буквы

library(stringr)
loan_applications <- loan_applications %>% mutate(Месяц = str_to_title(Месяц))
loan_applications <- loan_applications %>% mutate(Регион = str_to_title(Регион))

colnames(loan_applications)
loan_applications <- loan_applications %>% separate(col = "Доля онлайн-заявок", into = c("Доля онлайн-заявок", NA))
loan_applications <- loan_applications %>% separate(col = "Доля заявок в офисе банка", into = c("Доля заявок в офисе банка", NA))

colnames(loan_applications)[5] <- "Доля онлайн-заявок (%)"
colnames(loan_applications)[6] <- "Доля заявок в офисе банка (%)"

class(loan_applications$`Доля онлайн-заявок (%)`)
loan_applications$`Доля онлайн-заявок (%)` <- as.numeric(loan_applications$`Доля онлайн-заявок (%)`)
loan_applications$`Доля заявок в офисе банка (%)` <- as.numeric(loan_applications$`Доля заявок в офисе банка (%)`)

#
write.csv(loan_applications, "Рейтинг регионов по количеству заявок на кредит.csv")

##### Рейтинг регионов по количеству ипотечных сделок##### 

setwd("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные/ДомКлик/Рейтинг регионов по количеству ипотечных сделок")

file_list2 <- list.files(pattern = "\\.xlsx$")
data_list2 <- list()

for (file in file_list2) {
  clean_name2 <- sub("\\.xlsx$", "", file)
  data2 <- read_excel(file)
  data_list2[[clean_name2]] <- data2
}

data_list2 # список с 12 датафреймами
# связать все строки из списка в один датафрейм
mortgage_deals <- bind_rows(data_list2, .id = "filename") # аргумент .id = "filename" добавляет столбец filename с источником (названием исходного файла) данных

mortgage_deals <- mortgage_deals[,-c(2:3, 8:10)]

mortgage_deals <- mortgage_deals %>% separate(col = filename, into = c("Месяц", "Год"), sep = " ")

unique(mortgage_deals$Месяц)
mortgage_deals <- mortgage_deals %>% mutate(Месяц = str_to_title(Месяц))

#сортировка регионов по алфавиту и месяцев по порядку 

unique(mortgage_deals$Месяц)
month_order1 <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", 
                  "Ноябрь", "Декабрь")

mortgage_deals <- mortgage_deals %>% mutate(Месяц = factor(Месяц, levels = month_order1))
mortgage_deals <- mortgage_deals %>% arrange(Регион, Месяц)

# все регионы с заглавной буквы

library(stringr)
mortgage_deals <- mortgage_deals %>% mutate(Регион = str_to_title(Регион))

# удаляем строки с пропущенными значениями целиком, т.е. с данными по регионам за весь год 

mortgage_deals <- na.omit(mortgage_deals)

colnames(mortgage_deals)
mortgage_deals <- mortgage_deals %>% separate(col = "Доля сделок, первичка", into = c("Доля сделок, первичка (%)", NA))
mortgage_deals <- mortgage_deals %>% separate(col = "Доля сделок, вторичка", into = c("Доля сделок, вторичка (%)", NA))

class(mortgage_deals$`Доля сделок, первичка (%)`)
mortgage_deals$`Доля сделок, первичка (%)` <- as.numeric(mortgage_deals$`Доля сделок, первичка (%)`)
mortgage_deals$`Доля сделок, вторичка (%)` <- as.numeric(mortgage_deals$`Доля сделок, вторичка (%)`)

#
write.csv(mortgage_deals, "Рейтинг регионов по количеству ипотечных сделок.csv")

##### Объединение Домклик рейтингов регинов #####

Domclik <- left_join(loan_applications, mortgage_deals)

Domclik$Год <- as.numeric(Domclik$Год)

setwd("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные/ДомКлик")
write.csv(Domclik, "Рейтинг регионов по количеству заявок на кредит и по количеству ипотечных сделок.csv")

unique(mortgage_deals$Регион)
unique(loan_applications$Регион)
unique(Domclik$Регион)

##### РОССТАТ ##### 
setwd("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные/Росстат")

##### Заработная плата ##### 
wages <- read_excel("Заработная плата.xlsx", sheet = 2)

wages <- wages[,-c(2:13,26:27)]

months <- as.vector(as.matrix(wages[2,]))[2:13]
months <- tools::toTitleCase(months)
months
colnames(wages)[2:13] <- months
colnames(wages)
wages <- wages[-c(1:3),]
colnames(wages)[1] <- "Регион"

library(stringr)
wages[str_detect(wages$Регион,"округ"), ]
okruga <- str_detect(wages$Регион,"округ")
okruga
which(okruga == TRUE) #индексы строк, чтобы удалить их 

wages <- wages[-c(1,20,33,42,50,65,74,85),] #удаляю федаральные округа
wages <- wages[-c(21,62,63),] #удаляю Тюменскую и Архангельскую области, куда всключены автономные округа (оставляю только чисто Тюменскую и Архангельскую области)

# изменяю названию, чтобы было как в других датафреймах
wages[22,1] <- "Архангельская область"
wages[21,1] <- "Ненецкий Автономный округ"
wages[61,1] <- "Ханты-Мансийский Автономный округ - Югра"
wages[62,1] <- "Ямало-Ненецкий Автономный округ"
wages[63,1] <- "Тюменская область"
wages[18,1] <- "Москва"
wages[29,1] <- "Санкт-Петербург"
wages[84,1] <- "Еврейская Автономная область"
wages[85,1] <- "Чукотский Автономный округ"
wages[37,1] <- "Севастополь"

#Севастополя и Республика Крым нет в рейтингах ДОМКЛИК

wages <- wages %>% mutate(Регион = str_to_title(Регион)) # `` вместо ""
unique(wages$Регион) #85

class(wages$Январь)
# столбцы должны быть числовыми
wages <- wages %>% mutate(across(c(2:13), as.numeric))

wages_long <- wages %>% gather("Месяц", "Средняя заработная плата", 2:13)

wages_long <- wages_long %>%
  mutate(Квартал = case_when(
    Месяц %in% c("Январь", "Февраль", "Март") ~ 1,
    Месяц %in% c("Апрель", "Май", "Июнь") ~ 2,
    Месяц %in% c("Июль", "Август", "Сентябрь") ~ 3,
    Месяц %in% c("Октябрь", "Ноябрь", "Декабрь") ~ 4,
  ))

# R не распознает букву Й, поэтому нужно перекопировать названия месяцев из консоли (иначе Май пропадает)
unique(wages_long$Месяц)

month_order1 <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", 
                  "Ноябрь", "Декабрь")
wages_long <- wages_long %>% mutate(Месяц = factor(Месяц, levels = month_order1))

wages_long <- wages_long %>% arrange(Регион, Месяц)

write.csv(wages_long, "Заработная плата.csv")


##### Интернет ##### 

internet <- read_excel("Интернет.xlsx", sheet = 4)
internet <- internet[-c(1:2),c(1,25:28)] #2019

colnames(internet)[2:5] <- c(1,2,3,4) #кварталы
colnames(internet)[1] <- "Регион"
internet <- internet[-c(1:3),]

internet[str_detect(internet$Регион,"округ"), ]
okruga1 <- str_detect(internet$Регион,"округ")
okruga1
which(okruga1 == TRUE) #индексы строк, чтобы удалить их

internet <- internet[-c(1,20,31,40,48,63,71,82),]
internet <- internet[-60,] #удалила тюмень всю

# нет "Ненецкий Автономный округ", только "Архангельская область" -> на одно наблюдение меньше
internet[60,1] <- "Ханты-Мансийский Автономный округ - Югра"
internet[61,1] <- "Ямало-Ненецкий Автономный округ"
internet[62,1] <- "Тюменская область"
internet[18,1] <- "Москва"
internet[28,1] <- "Санкт-Петербург"
internet[36,1] <- "Севастополь"

internet <- internet %>% mutate(Регион = str_to_title(Регион)) 
unique(internet$Регион) #84

class(internet$"1")
# столбцы должны быть числовыми
internet <- internet %>% mutate(across(c(2:5), as.numeric)) #есть пропущенные значения

internet <- internet %>% gather("Квартал", "Число активных абонентов доступа к интернету", 2:5)

internet <- internet %>% arrange(Регион, Квартал)

internet <- internet %>% mutate(Регион = str_to_title(Регион))

internet$Квартал <- as.numeric(internet$Квартал)

write.csv(internet, "Интернет.csv")


##### Безработица ##### 

unemployment <-read_excel("Уровень безработицы населения.xls", sheet = 2)

unemployment <- unemployment[,c(1, 40:50)] #2020
unemployment <- unemployment[-c(1,2),c(1,2,5,8,11)] #кварталы январь  - март  2020, апрель  - июнь  2020, июль  - сентябрь  2020, октябрь - декабрь  2020


colnames(unemployment) <- c("Регион", 1,2,3,4)
unemployment <- unemployment[-c(1,2),]

unemployment[str_detect(unemployment$Регион,"округ"), ]
okruga2 <- str_detect(unemployment$Регион,"округ")
okruga2
which(okruga2 == TRUE) #индексы строк, чтобы удалить их

unemployment <- unemployment[-c(1,20,33,42,50,65,73,84),] #удаляю федеральные округа
unemployment <- unemployment[-21,] #удалила архангельскую обл всю
unemployment <- unemployment[-61,] #удалила тюменскую всю

unemployment[22,1] <- "Архангельская область"
unemployment[21,1] <- "Ненецкий Автономный округ"
unemployment[61,1] <- "Ханты-Мансийский Автономный округ - Югра"
unemployment[63,1] <- "Тюменская область"
unemployment[18,1] <- "Москва"
unemployment[29,1] <- "Санкт-Петербург"
unemployment[37,1] <- "Севастополь"

unemployment <- unemployment %>% mutate(Регион = str_to_title(Регион)) 
unique(unemployment$Регион) #85

class(unemployment$"1")
# столбцы должны быть числовыми
unemployment <- unemployment %>% mutate(across(c(2:5), as.numeric)) 

unemployment <- unemployment %>% gather("Квартал", "Уровень безработицы (%)", 2:5)

unemployment$Квартал <- as.numeric(unemployment$Квартал)

unemployment <- unemployment %>% arrange(Регион, Квартал)

write.csv(unemployment, "Безработица.csv")

##### объединение РОССТАТ ##### 

Rosstat_full <- full_join(unemployment, internet) %>% full_join(wages_long)

# поменяю столбцы местами
colnames(Rosstat_full)
column_order <- c("Регион", "Квартал", "Месяц", "Средняя заработная плата", "Уровень безработицы (%)", "Число активных абонентов доступа к интернету")
Rosstat_full <- Rosstat_full[ ,column_order]


setwd("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные/Росстат")
write.csv(Rosstat_full, "Росстат полный.csv")


##### Объединение всех данных #####
# опять МАЙ ИСЧЕЗ (проблема в Domclik)
Final_data <- full_join(Sber_full,Rosstat_full) 

unique(Final_data$Месяц)[5] == unique(Domclik$Месяц)[5] # FALSE Май по-разному пишется
unique(Final_data$Месяц)[1] == unique(Domclik$Месяц)[1] 
unique(Final_data$Месяц)[3] == unique(Domclik$Месяц)[3]
unique(Final_data$Месяц)[10] == unique(Domclik$Месяц)[10] #а все остальные месяцы одинаковые


#заменяю написание май в Domclik как в других
Domclik$Месяц <- gsub(Domclik$Месяц[5], Final_data$Месяц[5], Domclik$Месяц) # май = level 5
unique(Final_data$Месяц)[5] == unique(Domclik$Месяц)[5] # теперь TRUE

Final_data <- full_join(Sber_full,Rosstat_full) %>% full_join(Domclik) #надо вот так последовательно соединять, иначе не оч


colnames(Final_data)
column_order1 <- c("Регион", "Год", "Месяц", "Квартал", "Индекс.БП", "Месячный Индекс.А", "Индекс.Т" , 
                   "Средняя заработная плата", "Уровень безработицы (%)", "Число активных абонентов доступа к интернету",
                   "Всего одобренных заявок", "Доля онлайн-заявок (%)", "Доля заявок в офисе банка (%)" ,
                   "Всего ипотечных сделок", "Доля сделок, первичка (%)", "Доля сделок, вторичка (%)")
Final_data <- Final_data[ ,column_order1]

setwd("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные")
write.csv(Final_data, "All_data.csv")


##### Пропущенные значения ##### 

is.na(Final_data)
sum(is.na(Final_data)) #801

with_na <- Final_data[!complete.cases(Final_data), ] #отбираем только строки, где есть NA, и все стобцы
View(with_na)

# индекс Месячный А полностью отсутствует у 10: Еврейская Автономная Область, Ненецкий Автономный Округ, 
#                                   Республика Алтай, Республика Ингушетия, Чеченская Республика, Чукотский Автономный Округ,
#                                   Ленинградская Область, Республика Крым, Севастополь, Республика Северная  Осетия - Алания

# индекс Т полностью отсуствует в Ненецкий Автономный Округ, Республика Крым, Севастополь
# северная осетия потеряла туризм!!! (решено)
#
# Индекс БП отсутствует у Ленинградская Область, Московская Область, Республика Крым, Севастополь, Республика Северная  Осетия - Алания

# Крым и Севастополь есть только в росстате

with_na[1008,]
Final_data[1009,1] == Final_data[1025,1]
Final_data[1009,1] == Final_data[1010,1]

# проверка "Республика Северная Осетия - Алания"
unique(tourists_Sber$Регион)[55] == unique(activity_Sber$Регион)[51] #TRUE
unique(transactions_Sber$Регион)[54] == unique(activity_Sber$Регион)[51] #TRUE
# в данных Сбера "Республика Северная Осетия - Алания" пишется одинаково
# проверка домклик
unique(loan_applications$Регион)[56] == unique(activity_Sber$Регион)[51] #TRUE
unique(mortgage_deals$Регион)[56] == unique(activity_Sber$Регион)[51] #TRUE
# сбер и домклик одинаковые

unique(wages_long$Регион)[57] == unique(activity_Sber$Регион)[51] #FALSE
unique(internet$Регион)[56] == unique(activity_Sber$Регион)[51] #TRUE
unique(unemployment$Регион)[57] == unique(activity_Sber$Регион)[51] #TRUE

unique(Rosstat_full$Регион)[57] == unique(activity_Sber$Регион)[51] #FALSE

# в росстате Республика Северная Осетия - Алания дублируется. нужно менять в wages_long 

wages_long$Месяц[5]

wages_long$Регион <- gsub("Республика Северная  Осетия - Алания", "Республика Северная Осетия - Алания", wages_long$Регион)
unique(wages_long$Регион)[57] == unique(activity_Sber$Регион)[51] #TRUE

# проблема с Республика Северная  Осетия - Алания решена (перезаписала Final_data)

setwd("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные")
write.csv(Final_data, "All_data.csv")

# В активности сбера Отсутствует 7: "Еврейская Автономная Область","Ленинградская Область", "Ненецкий Автономный Округ", "Республика Алтай", 
#"Республика Ингушетия", "Чеченская Республика","Чукотский Автономный Округ"

# в СБЕР нет Московской и Ленинградской областей -> на 2 


data <- read.csv("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные/All_data.csv")

sum(is.na(data)) #561 NA
sum(!complete.cases(data)) #144 строк с хотя бы одним NA

library(VIM)
aggr(data, oma = c(10,3,1,3), cex.axis = 0.4)
matrixplot(data, oma = c(10,3,1,3), cex.axis = 0.3)

#удаляю крым и севастополь
data <- data[-c(997:1020),]

write.csv(data, "All_data.csv")
# я удалила крым и севастополь, потому что у них очень много пропущенных значений
# но пока оставила другие регионы, так как у них могут быть пропущенные значения только в одной из переменных




#================Часть 2: разведывательный анализ данных==========================================================

data <- read.csv("/Users/viktoriazajceva/Desktop/R/20 Проект/Данные/All_data.csv")
data <- data[, -c(1,2)] # почему-то создаются столбцы с нумерацией в начала

##### Выбор регионов, лидирующих по потребительской активности, безналичным платежам и внутреннему туризму ##### 

##### Описательная статистика для Сбер ##### 

library(psych)
par(oma = c(0, 0, 0, 0))
# безналичные платежи
describe(data$Индекс.БП)
hist(data$Индекс.БП) #распределение не нормальное

# месячная активность
describe(data$Месячный.Индекс.А)
hist(data$Месячный.Индекс.А) #распределение примерно нормальное

# внутренний туризм
describe(data$Индекс.Т) #распределение примерно нормальное
hist(data$Индекс.Т)

##### 2. Динамика средних показателей сбера в 2020 ##### 
##### Туризм ##### 
t_descr <- describeBy(data$Индекс.Т, group = data$Месяц)
t_descr

names(t_descr)
# но так как в списке месяца располагаются по алфавиту, а нам нужно по порядку, нужно изменить порядок месяцев = порядок sublists внутри первоначального списка

month_order1 <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", 
                  "Ноябрь", "Декабрь")
t_descr <- t_descr[month_order1]
t_descr

# функция, которая будет брать 3 элемент из каждого sublist, т.е. среднее
t_descr[[12]][3]
# пустой вектор для функции
tourist_means <- c()

mean_function <- function(list, vector) {
  for (sublist in list) {
    vector <- c(vector, sublist[[3]])
  }
  return(vector)
}

tourist_means <- mean_function(t_descr, tourist_means)
tourist_means # вектор со средними значениями изменение количества внутренних туристов по отношению к аналогичному месяцу прошлого года, в процентах по месяцам (январь-декабрь)

##### Месячная активность ##### 

activ_descr <- describeBy(data$Месячный.Индекс.А, group = data$Месяц)
activ_descr
activ_descr <- activ_descr[month_order1]
activ_descr

activity_means <- c()
activity_means <- mean_function(activ_descr, activity_means)
activity_means # вектор со средними значениями индексов потребительских значений по месяцам 

#####  Доля безналичных платежей в торговом обороте ##### 

trans_descr <- describeBy(data$Индекс.БП, group = data$Месяц)
trans_descr
trans_descr <- trans_descr[month_order1]
trans_descr

transaction_means <- c()
transaction_means <- mean_function(trans_descr, transaction_means)
transaction_means # вектор со средними значениями долей безналичных платежей в торговом обороте по месяцам


sber_means <- cbind.data.frame(activity_means, transaction_means, tourist_means)
sber_means

sber_means$month <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", 
                      "Ноябрь", "Декабрь")

sber_means

library(tidyverse)

# Reshape the data into long format
sber_means_long <- pivot_longer(sber_means, 
                                cols = c(activity_means, transaction_means, tourist_means), 
                                names_to = "Metric", 
                                values_to = "Value")

# ggplot выстроит месяца по алфавиту, надо установить порядок

sber_means_long$month <- factor(sber_means_long$month, levels = month_order1)

# Plot the data
ggplot(sber_means_long, aes(x = month, y = Value, color = Metric, group = Metric)) +
  geom_line() +
  labs(x = "Месяц", y = "Показатель", 
       title = "Динамика показателей СберИндекс в 2020 году по месяцам",
       color = "Показатель") + #название легенды
  scale_color_manual(values = c("blue", "red", "green"),
                     labels = c("Индекс потребительской активности", "Доля безналичных платежей в торговом обороте", "Изменение количества внутренних туристов"))


##### 3. 12 box plots #####  

# Convert Месяц to a factor with custom levels
data$Месяц <- factor(data$Месяц, levels = month_order1)

# Plot with reordered facets
sber_boxplots <- ggplot(data, aes(x = Месяц)) +
  geom_boxplot(aes(y = Месячный.Индекс.А, x = "Месячный.Индекс.А"), fill = "blue") +
  geom_boxplot(aes(y = Индекс.БП, x = "Индекс.БП"), fill = "red") +
  geom_boxplot(aes(y = Индекс.Т, x = "Индекс.Т"), fill = "green") +
  facet_wrap(~ Месяц, scales = "free_x", ncol = 4) +
  labs(x = "", y = "", title = "Показатели СберИндекс по месяцам в 2020 году")

##### 4.Нетипичные наблюдения ##### 
#
bp <- boxplot(data$Индекс.БП ~ data$Месяц, cex.labels = 0.2)
bp
outliers_bp <- bp$out

bp_outlier_indices <- which(data$Индекс.БП %in% outliers_bp)
bp_outlier_indices

bp_outliers_data <- data[bp_outlier_indices, ]
bp_outliers_data #самые низкие индексы безналичных платежелй в Чечне, Северной осетии, Ингушении, Дагестане, КБР
# самые высокие в Ненецком АО


#
act <- boxplot(data$Месячный.Индекс.А ~ data$Месяц, cex.labels = 0.2)
act
outliers_act <- act$out

act_outlier_indices <- which(data$Месячный.Индекс.А %in% outliers_act)
act_outlier_indices

act_outliers_data <- data[act_outlier_indices, ]
act_outliers_data # Не вижу очевидных объяснений

# для туризма не делаю

##### 5. Топ-30 регионов по потребительской активности ##### 

activity_by_reg <- describeBy(data$Месячный.Индекс.А, group = data$Регион)
activity_by_reg

# функция, которая будет брать 3 элемент из каждого sublist, т.е. среднее


act_mean_by_reg <- data.frame()

mean_function_dataframe <- function(list, dataframe) {
  for (sublist in list) {
    mean_value <- sublist[[3]]
    dataframe <- rbind(dataframe, mean_value)
  }
  return(dataframe)
}

act_mean_by_reg <- mean_function_dataframe(activity_by_reg, act_mean_by_reg)
act_mean_by_reg
act_mean_by_reg$Регион
act_mean_by_reg$Регион <- sort(unique(data$Регион)) # describeBy дает список сортированный по алфавиту

colnames(act_mean_by_reg)[1] <- "Индекс.А"
act_mean_by_reg <- act_mean_by_reg %>% arrange(desc(Индекс.А))
top30_activity <- head(act_mean_by_reg, 30)

##### 6. Топ-30 регионов по безналичных платежам ##### 

bp_by_reg <- describeBy(data$Индекс.БП, group = data$Регион)
bp_by_reg
bp_mean_by_reg <- data.frame()

mean_function_dataframe <- function(list, dataframe) {
  for (sublist in list) {
    mean_value <- sublist[[3]]
    dataframe <- rbind(dataframe, mean_value)
  }
  return(dataframe)
}

bp_mean_by_reg <- mean_function_dataframe(bp_by_reg, bp_mean_by_reg)


bp_mean_by_reg$Регион <- sort(unique(data$Регион))

colnames(bp_mean_by_reg)[1] <- "Индекс.БП"
bp_mean_by_reg <- bp_mean_by_reg %>% arrange(desc(Индекс.БП))
top30_bp <- head(bp_mean_by_reg, 30)
# (конкретно с bp можно только один раз прогнать код, потом нужно удалить bp_mean_by_reg, чтобы не было дублирования)

##### 7. Регионы, которые одновременно лидируют по потребительской активности и доле безналичных платежей. ####
# Другими словами, найдите регионы, общие для списков, полученных в пунктах 5 и 6. 

common_top <- intersect(top30_bp$Регион, top30_activity$Регион)

# 13
# "Камчатский Край"         "Тюменская Область"       "Томская Область"         "Калининградская Область"
# "Кировская Область"       "Хабаровский Край"        "Удмуртская Республика"   "Свердловская Область"   
# "Москва"                  "Иркутская Область"       "Нижегородская Область"   "Кемеровская Область"    
# "Республика Башкортостан"

##### 8. Выберите строки, соответствующие регионам из предыдущего пункта, и сохраните их в отдельный датафрейм. #####

common_rows <- top30_activity$Регион %in% common_top
common_rows
subset_top30_activity <- top30_activity[common_rows, ]
subset_top30_activity

common_rows2 <- top30_bp$Регион %in% common_top
subset_top30_bp <- top30_bp[common_rows2, ]
subset_top30_bp

top_activ_and_bp <- full_join(subset_top30_activity, subset_top30_bp, by = "Регион")

top_activ_and_bp <- top_activ_and_bp[ , c("Регион", "Индекс.А", "Индекс.БП")]
top_activ_and_bp <- top_activ_and_bp %>% arrange(Регион)


# подсчет средних индексов туризма, не нужно делать
tour_by_reg <- describeBy(data$Индекс.Т, group = data$Регион)
tour_by_reg
tour_mean_by_reg <- data.frame()

mean_function_dataframe <- function(list, dataframe) {
  for (sublist in list) {
    mean_value <- sublist[[3]]
    dataframe <- rbind(dataframe, mean_value)
  }
  return(dataframe)
}

tour_mean_by_reg <- mean_function_dataframe(tour_by_reg, tour_mean_by_reg)
tour_mean_by_reg

tour_mean_by_reg$Регион <- sort(unique(data$Регион))

colnames(tour_mean_by_reg)[1] <- "Индекс.Туризм"

common_rows3 <- tour_mean_by_reg$Регион %in% common_top
common_rows3
subset_tourism <- tour_mean_by_reg[common_rows3, ]
subset_tourism # средниие за год индексы туризма в регионах, 
# которые одновременно лидируют по потребительской активности и доле безналичных платежей


##### 9. Регионы с индексом туризма выше нуля в 5 месяцах и более ##### 

# Определите, в каких регионах среди отобранных на предыдущих шагах, индекс
# внутреннего туризма принимал значения выше 0 не менее чем в течение 5
# месяцев (не обязательно подряд). 

sum(data$Индекс.Т>0, na.rm = TRUE) # все случаи когда туризм > 0, na.rm = TRUE т.к. видимо есть пропущенные значения
# is the result >= 5 ? - check for each region

counts_greater_than_zero_by_group <- aggregate(data$Индекс.Т > 0 ~ Регион, data = data, FUN = sum, na.rm = TRUE)
counts_greater_than_zero_by_group

reg_with_tour_above_zero <- data.frame()

# Loop through each row of counts_greater_than_zero_by_group
for (i in seq_len(nrow(counts_greater_than_zero_by_group))) {
  count_value <- counts_greater_than_zero_by_group[i, 2]
  
  if (count_value >= 5) {
    reg_with_tour_above_zero <- rbind(reg_with_tour_above_zero, counts_greater_than_zero_by_group[i, ])
  }
}

reg_with_tour_above_zero #48 регионов, где индекс туризма был выше нуля 5 и более раз

##### 10. Регионы, которые одновременно лидируют по потребительской активности и доле безналичных платежей, и с индексом туризма был выше нуля в 5 раз ##### 

common_rows4 <- reg_with_tour_above_zero$Регион %in% common_top
common_rows4
top_reg_tourism_above_zero_5t <- reg_with_tour_above_zero[common_rows4, ]
top_reg_tourism_above_zero_5t
# из 13 регионов, которые одновременно лидируют по потребительской активности и доле безналичных платежей,
# только у 4 регионов индекс туризма был выше нуля в 5 месяцах и более за год:

#"Калининградская Область", "Камчатский Край","Кировская Область", "Нижегородская Область" 

# предполагается, что я дальше анализирую данные по месяцам в 4 регионах, полученных в пункте 10?

##### 11. Анализ по месяцам для топовых регионов ##### 
# теперь нужно в data пройтись для топовых регионов по месяцам
top_only <- data %>% filter(Регион %in% c('Кировская Область', 'Калининградская Область', 'Камчатский Край', 'Нижегородская Область'))
# When filtering rows based on multiple values in a column using filter() from the dplyr package, you should use the %in% operator instead of ==
colnames(top_only)[5] <- "Безналичные_платежи"
colnames(top_only)[6] <- "Активность"
colnames(top_only)[7] <- "Туризм"
# месяцы, в которые потребители этих регионов наиболее активны;

median(top_only$Активность) #71

top_only %>%
  filter(Активность > 71) %>%
  arrange(desc(Активность)) %>%
  group_by(Регион) %>%
  summarize(Месяцы_ordered = list(Месяц)) %>% # list для того чтобы сохранить порядок по убыванию
  mutate(Месяцы_ordered = sapply(Месяцы_ordered, toString)) # обратно в вектор, через запятую

# 1 Калининградская Область Сентябрь, Август, Июль, Октябрь, Июнь, Февраль, Ноябрь, Март
# 2 Камчатский Край         Июль, Сентябрь, Октябрь                             
# 3 Кировская Область       Сентябрь, Июль, Август, Октябрь, Июнь, Февраль     
# 4 Нижегородская Область   Сентябрь, Октябрь, Март, Июль, Февраль 


# месяцы, в которые потребители этих регионов чаще всего совершают безналичные платежи;

median(top_only$Безналичные_платежи) #58.25

top_only %>%
  filter(Безналичные_платежи > 58.25) %>%
  arrange(desc(Безналичные_платежи)) %>%
  group_by(Регион) %>%
  summarize(Месяцы_ordered = list(Месяц)) %>% # list для того чтобы сохранить порядок по убыванию
  mutate(Месяцы_ordered = sapply(Месяцы_ordered, toString))

#   Регион                  Месяцы_ordered                                                                    
# 1 Калининградская Область Ноябрь, Май, Октябрь, Декабрь, Январь, Август, Сентябрь, Март                     
# 2 Камчатский Край         Ноябрь, Январь, Октябрь, Декабрь, Август, Март, Май, Июнь, Июль, Сентябрь, Февраль
# 3 Кировская Область       Ноябрь, Январь, Декабрь, Октябрь                                                  
# 4 Нижегородская Область   Ноябрь 


# месяцы, в которые эти регионы чаще всего посещают внутренние туристы.

median(top_only$Туризм) # -4.92, но я возьму 0

top_only %>%
  filter(Туризм > 0) %>%
  arrange(desc(Туризм)) %>%
  group_by(Регион) %>%
  summarize(Месяцы_ordered = list(Месяц)) %>% # list для того чтобы сохранить порядок по убыванию
  mutate(Месяцы_ordered = sapply(Месяцы_ordered, toString))

# 1 Калининградская Область Август, Сентябрь, Февраль, Июль, Январь, Октябрь, Ноябрь, Декабрь
# 2 Камчатский Край         Август, Сентябрь, Декабрь, Март, Февраль                         
# 3 Кировская Область       Февраль, Сентябрь, Август, Январь, Март                          
# 4 Нижегородская Область   Февраль, Январь, Март, Август, Сентябрь 


#================ Поиск взаимосвязей ================

##### 1.2.3. Индекс безналичных платежей и Индекс внутреннего туризма ##### 

ggplot(top_only, aes(x = Безналичные_платежи, y = Туризм)) +
  geom_point() +
  labs(title = "Cвязь между безналичными платежами и внутренним туризмом",
       x = "Индекс безналичных платежей",
       y = "Индекс внутреннего туризма")

cor(top_only$Безналичные_платежи,top_only$Туризм) #0.032, линейной связи нет
cor(top_only$Безналичные_платежи,top_only$Туризм, method = "spearman") # 0.0147 нелинейной связи тоже нет


##### 4. Кредит и ипотека ##### 


top_only$Всего.ипотечных.сделок[top_only$Всего.ипотечных.сделок == "50 - 100"] <- "50 - 500"
top_only$Всего.ипотечных.сделок[top_only$Всего.ипотечных.сделок == "5"] <- "50 - 500"

unique(top_only$Всего.ипотечных.сделок)

# Verify the change

factor_credit <- factor(unique(top_only$Всего.одобренных.заявок), levels = c("100 - 500", "500 - 1 000", "1 000 - 5 000"), ordered = TRUE)
str(factor_credit)

factor_ipotek <- factor(unique(top_only$Всего.ипотечных.сделок), levels = c("50 - 500", "500 - 1 000", "> 1 000"), ordered = TRUE)
str(factor_ipotek)

cor(as.numeric(factor_credit), as.numeric(factor_ipotek, method = "spearman"))
# 0.5 - положительная умеренная связь

##### 5. Безналичные платежи и онлайн-заявки на кредит ##### 

cor(top_only$Безналичные_платежи, top_only$Доля.онлайн.заявок....)
# 0.322 положительная слабая связь

##### 6. Вывод ##### 
# Корреляционный анализ выявил, что в тех регионах, где жители чаще берут кредиты, число ипотечных сделок также выше, что характеризуется средней силой связи.
# Корреляционный анализ выявил слабую положительную связь между долей безналичных платежей и числом онлайн-заявок на кредит.


##### 7. Сезон ##### 
top_only <- top_only %>%
  mutate(Сезон = case_when(
    Месяц %in% c("Январь", "Февраль", "Декабрь") ~ "Зима",
    Месяц %in% c("Апрель", "Май", "Март") ~ "Весна",
    Месяц %in% c("Июль", "Август", "Июнь") ~ "Лето",
    Месяц %in% c("Октябрь", "Ноябрь", "Сентябрь") ~ "Осень",
  ))


##### 8. Ипотечные сделки по сезонам ##### 
# возвращаю исходный датафрейм, так как меняла ипотеку для корреляции
top_only <- data %>% filter(Регион %in% c('Кировская Область', 'Калининградская Область', 'Камчатский Край', 'Нижегородская Область'))

colnames(top_only)[5] <- "Безналичные_платежи"
colnames(top_only)[6] <- "Активность"
colnames(top_only)[7] <- "Туризм"
# добавить сезон

# столбиковую диаграмму, которая бы иллюстрировала соотношение
# количества ипотечных сделок, заключённых в разное время года, для каждой
# категории числа ипотечных сделок (10–50, 50–100, 100–500, 500–1000, больше 1000).

unique(top_only$Всего.ипотечных.сделок) # нет 10-50 категории

# фактор для упорядочивания столбцов

top_only <- top_only %>% 
  mutate(Всего.ипотечных.сделок = factor(Всего.ипотечных.сделок, 
                      levels = c("50 - 100", "100 - 500", "500 - 1 000", "> 1 000"),
                      ordered = TRUE))

top_only$Сезон <- factor(top_only$Сезон, levels = c("Зима", "Весна", "Лето", "Осень"),
                         ordered = TRUE)

ggplot(top_only, aes(x = Всего.ипотечных.сделок, fill = Сезон)) + geom_bar() +
  facet_wrap(~Сезон) +
  scale_fill_manual(values = c("Весна" = "lightpink", "Лето" = "chartreuse3", "Осень" = "darkorange", "Зима" = "lightsteelblue1")) +
  labs(title = "Количество ипотечных сделок в разное время года",
       y = "",
       x = "Количество ипотечных сделок") +
  theme_minimal()

# еще такой вариант, но я не буду его использовать

ggplot(top_only, aes(x = Всего.ипотечных.сделок, fill = Сезон)) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("Весна" = "lightpink", "Лето" = "chartreuse3", "Осень" = "darkorange", "Зима" = "lightsteelblue1")) +
  theme_minimal() +
  labs(title = "Количество ипотечных сделок в разное время года",
       y = "",
       x = "Количество ипотечных сделок")

# больше всего сделок заключается осенью

##### 9.10. Онлайн-заявки на ипотеку в разное время года ##### 

ggplot(top_only, aes(x = Сезон, y = Доля.онлайн.заявок...., fill = Сезон)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("Весна" = "lightpink", "Лето" = "chartreuse3", "Осень" = "darkorange", "Зима" = "lightsteelblue1")) +
  theme_minimal() +
  labs(x = "Время года", y = "", title = "Доли онлайн-заявок на ипотеку в разное время года")

# судя по медиане, больше все онлйан-заявок осенью. скорее всего, это результат того, что осенью в целом заключается наибольшее количество сделок.
# люди могут чаще брать кредиты осенью, так как это период после сезона отпусков и между новогодней порой
# также в 2020 году к осени большинство ковидных ограничений было снято и людям стало проще принимать большие решения

# весенний широкий разброс может объясняться тем, что из-за введения в марте ковидных ограничений люди отложили взятие кредита из-за неопределенности, 
# а если брали кредит, то онлайн, потому что многие отделения банков были закрыты

cor(top_only$Доля.онлайн.заявок...., as.numeric(top_only$Сезон), method = "spearman")
# 0.22 - слабая положительная корреляция, т.е. возможно люди чаще берут кредит ближе к концу года


##### 11.12. Ипотека на вторичное жилье в разное время года ##### 

ggplot(top_only, aes(x = Сезон, y = Доля.сделок..вторичка...., fill = Сезон)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("Весна" = "lightpink", "Лето" = "chartreuse3", "Осень" = "darkorange", "Зима" = "lightsteelblue1")) +
  theme_minimal() +
  labs(x = "Время года", y = "", title = "Доли онлайн-заявок на ипотеку в разное время года")
# вторичку берут чаще зимой или весной

cor(top_only$Доля.сделок..вторичка...., as.numeric(top_only$Сезон), method = "spearman")

# слабая отрицательная корреляция - возможно люди заключают больше сделок на вторичное жилье в начале года

# зимой и весной строительство новых домов замедлено из-за холодов и не появляется новых строек и предложений,
# поэтому люди покупают готовое вторичное жилье



