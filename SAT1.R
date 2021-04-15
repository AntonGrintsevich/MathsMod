#Очистить рабочую память
rm(list=ls())
#Проверим текущую рабочую директорию
getwd()
library(tidyverse)
library(dplyr)
#Считаем данные по первой метеостанции (файл "all_saratov_data.csv" в рабочей директории)
met1=read.csv("all_saratov_data.csv",header=TRUE,sep=",")
#проверим структуру
str(met1)
#Считаем данные по второй метеостанции (файл "all_saratov_data2.csv" в рабочей директории)
met2=read.csv("all_saratov_data2.csv",header=TRUE,sep=",")
str(met2)
#загрузим библиотеку для работы с датами
library(lubridate)
#Отфильтруем данные обоих станций по 23 годам, предшествующим 2004 (1981-2003)
met1f=filter(met1,year(as.Date(date))<=2003,year(as.Date(date))>=1981)
met2f=filter(met2,year(as.Date(date))<=2003,year(as.Date(date))>=1981)
#Проверим, что размерности после фильтрации совпадают
dim(met1)
dim(met2)
dim(met1f)
dim(met2f)
#Построим таблицу где каждой дате соответствует средняя температура по данным 2-х станций
temp2st=data.frame(met1f$date,0.5*(met1f$tavg+met2f$tavg))
#Переименуем заголовки столбцов
names(temp2st)=c("date","tavg")
head(temp2st)
#Для удобства представим элементы даты (год,мес,день) в трех отдельных столбцах числового формата
yr=year(as.Date(temp2st$date))
mon=month(as.Date(temp2st$date))
day=mday(as.Date(temp2st$date))
temp2st = cbind(temp2st,yr,mon,day)
head(temp2st)
#Строим вектор, где оставляем значения ср.температуры большие 8 град (в нашей размерности >80), меньше обнуляем
sat8=case_when(temp2st$tavg>=80~temp2st$tavg,temp2st$tavg<80~0,TRUE~0)
#Добавляем его в качестве столбца sat8 (будет использован для рассчета средней активной температуры)
temp2st = cbind(temp2st,sat8)
#Строим вектор, где дню с температурой больше 8 град соответствует 1, меньше 0
dveg=case_when(temp2st$tavg>=80~1,temp2st$tavg<80~0,TRUE~0)
#Добавляем его в качестве столбца dveg (будет использован для подсчета дней вегетации)
temp2st = cbind(temp2st,dveg)
head(temp2st)
#Нам также пригодится вектор с количеством дней в каждом месяце
day_in_mon = c(31,28,31,30,31,30,31,31,30,31,30,31)
#Обнулим данные в столбцах sat8 и dveg, которые соответствуют датам ранее 15 апреля
#т.к. раньше посев не проводится
temp2st$sat8[temp2st$mon<=3]=0
temp2st$sat8[temp2st$mon==4 & temp2st$day<=15]=0
temp2st$dveg[temp2st$mon<=3]=0
temp2st$dveg[temp2st$mon==4 & temp2st$day<=15]=0
#Обнулим в столбце dveg все дни вегитации, если их количество с начала года превысило 4 мес (120 дней)
for (yy in 1981:2003) {
numveg=0
for (mm in 4:12) {
for (dd in 1:day_in_mon[mm]) {
if (temp2st$dveg[temp2st$yr==yy & temp2st$mon==mm & temp2st$day==dd] == 1) numveg=numveg+1
if  (numveg>=120) {temp2st$dveg[temp2st$yr==yy & temp2st$mon==mm & temp2st$day>dd]=0
temp2st$dveg[temp2st$yr==yy & temp2st$mon>mm]=0}
}
}
}
#Проверим, сколько осталось ненулевых дней вегетации в произвольном году
sum(temp2st$dveg[temp2st$yr==1997])
#Определим нулевые вектора месячных значений активных температур и дней вегетации
sat = rep(0,times=12)
di = rep(0,times=12)
#Посчитаем по месяцам значения САТ и отн.дней вегетации
for (mm in 1:12) {
sat[mm] = sum(temp2st$sat8[temp2st$mon==mm])/23
di[mm] = sum(temp2st$dveg[temp2st$mon==mm])/23/day_in_mon[mm]
}
di
#значения САТ приведем к градусам цельсия и округлим до целых
sat=as.integer(0.1*sat)
sat
#Для рассчета урожайности нам потребуются табличные значения
afi = c(0,0,0,32.11,26.31,25.64,23.2,18.73,16.3,13.83,0,0)
bfi = c(0,0,0,11.3,9.26,9.03,8.16,6.59,5.73,4.87,0,0)
#по расчетной формуле урожайности
sum=0
for (i in 1:12) {
sum=sum+(afi[i]+bfi[i]*sat[i])*di[i]*300/(1600*2.2*75)}
#Расчет урожая сделан в тоннах (без *1000000) 
yield=sum
yield

