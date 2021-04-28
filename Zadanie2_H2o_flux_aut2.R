#Гринцевич Ю.А., ПАЭ 121
#Вариант 5
#Задание 2
# Cоздайте модель множественной линейной регрессии потоков 
# паров воды за  осенний период 2013 года по данным измерений
# методом турбулентной пульсации.

#Очистить рабочую память
rm(list=ls())
#Проверка рабочей директории
getwd()
#Загрузка необходимых библиотек
library(tidyverse)
library(tidyr)
library(stringr)
library(dplyr)
library(rnoaa)
library(lubridate)
library(tibble)
library(readr)
#Чтениу данных из файла, пропускаем первую строку, заменяем текстовые 'NA',
# пустые и сгенерированные "-9999" значения на NA, игнорируем строки с "[" 
eddydat = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"),
comment=c("["))
#Посмотрим на данные
glimpse(eddydat)
#Удаление первой строки и ненужного пустого столбца "roll"
eddydat = eddydat[-1, ]
eddydat = select(eddydat, -(roll))
#Изменение специальных символов в названии стобцов на допустимые для переменных названия
names(eddydat) = names(eddydat) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_") %>%
  str_replace_all("[.]","_")
#Отфильтруем данные за осень
datf=filter(eddydat,month(as.Date(date))<=11,month(as.Date(date))>=9)
#посмотрим как уменьшилась размерность
dim(eddydat)
dim(datf)
#Преобразуем переменные типа char в факторы
datf = datf %>% mutate_if(is.character, as.factor)
#Преобразуем переменные в числовой тип numeric
datf_numeric = datf[,sapply(datf,is.numeric)]
dim(datf_numeric)
#уберем все NA, иначе корелляции не расчитаются
datf_numeric=na.exclude(datf_numeric)
#окончательная размерность данных
dim(datf_numeric)
#Расчитаем коэффициенты корреляции
corel = cor(datf_numeric)
corel=data.frame(corel)
#Корелляции с исследуемой переменной потоков водяного пара находятся в столбце
corel$h2o_flux
#Посчитаем коэфф детерминации зависимой переменной
R2=corel$h2o_flux^2
names(R2)=names(corel)
#выбрем только значимые коэффициенты, в которых коэф детерминации более 0,16
R2_main=R2[R2>0.16] %>% na.exclude()
R2_main
#Проведем предварительный анализ значимых переменных
#Значимых переменных достаточно много - 27 штук
#Попробуем их сократить
#Из сильно коррелирующих между собой переменных будем выбирать одну,
# чтобы избежать эффекта мультиколлинеарности
#Найдем взаимные корреляции значимых переменных
cor_R2_main = cor(datf_numeric[names(R2_main)])
#Посмотрим по строкам, какие переменные сильно коррелируют (коэф кор > 0.9)
for (i in 1:27) {
ctemp=cor_R2_main[i,]
ctemp=ctemp[abs(ctemp)>0.9]
print(names(R2_main)[i])
print(ctemp)
}
#Из групп сильно коррелирующих переменных выбираем по одной (выбор субъективный)
#Для анализа остается 10 переменных 
param_rest=c("h2o_flux","qc_h2o_flux","co2_molar_density","RH","u_var","v_var","w_div_ts_cov",
        "w_div_h2o_cov","h2o_signal_strength_7200","flowrate")
cor(datf_numeric[param_rest])
#Посмотрим на оставшиеся данные
plot(datf_numeric[param_rest])
#Видим, что одна значимая переменная w_div_h2o_cov сильно коррелирует (коеф=0.999)
#c потоком водяного пара h2o_flux. Между ними практически линейная зависимость
#Это позволяет нам построить простую линейную регрессию, хорошо прогнозирующую
#поток пара
#Проверим как работает одномерная модель
mod1 = lm(data = datf_numeric, h2o_flux~ w_div_h2o_cov)
summary(mod1)
anova(mod1)
#Коэф детерминации по переменной w_div_h2o_cov равен 0.9982
# Построим график наблюдаемых значений от предсказанных значений
plot(mod1$fitted.values, datf_numeric$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="red")
#наблюдаем неплохую аппроксимацию
## Построим графиик нормального распределения:
plot(mod1,2)
# в центральной части данные распределены нормально, в хвостах отличаются
# Построим график зависимости остатков от наблюдаемых значений 
plot(datf_numeric$h2o_flux,mod1$residuals)
# зададим модель, связывающую остатки и H2O
mo1=lm(mod1$residuals~datf_numeric$h2o_flux)
abline(a=mo1$coefficients[1],b=mo1$coefficients[2],col="red")
# зависимость остатков от наблюдаемых значений слабая
# доверительный интервал
confint(mod1)

# Теперь построим модель множественной регрессии по всем оставшимся значимым переменным 
mod2 = lm(data = datf_numeric, h2o_flux~ qc_h2o_flux+co2_molar_density+RH
     +u_var+v_var+w_div_ts_cov+w_div_h2o_cov+h2o_signal_strength_7200+flowrate)
summary(mod2)
#коэффициент детерминации 0,999
anova(mod2)
#все переменные значимы

# Построим график наблюдаемых значений от предсказанных значений
plot(mod2$fitted.values, datf_numeric$h2o_flux)
abline(a=0, b=1, col="red")
#хорошая аппроксимация
# Построим графиик нормального распределения:
plot(mod2,2)
# остатки стали лучше распределены нормально, в хвостах отличаются
# Построим график зависимости остатков от наблюдаемых значений 
plot(datf_numeric$h2o_flux,mod2$residuals)
# зададим модель, связывающую остатки и H2O
mo2=lm(mod2$residuals~datf_numeric$h2o_flux)
abline(a=mo2$coefficients[1],b=mo2$coefficients[2],col="red")
# наблюдается слабая зависимость остатков от наблюдаемых значений


# Построим аналогичную модель при этом будем искать зависимость между переменными 
# второго порядка 
mod3 = lm(data = datf_numeric, h2o_flux~ (qc_h2o_flux+co2_molar_density+RH
          +u_var+v_var+w_div_ts_cov+w_div_h2o_cov+h2o_signal_strength_7200+flowrate)^2)
summary(mod3)
#коэф детерминации = 0,9998
anova(mod3)
#Имеется ряд не значимых переменных
# Построим график наблюдаемых значений от предсказанных значений
plot(mod3$fitted.values, datf_numeric$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="red")
#аппроксимация стала почти идеальной
# Построим графиик нормального распределения:
plot(mod3,2)
# остатки стали лучше распределены нормально, по-прежнему отличия в хвостах
# Построим график зависимости остатков от наблюдаемых значений 
plot(datf_numeric$h2o_flux,mod3$residuals)
# зададим модель, связывающую остатки и H2O
mo3=lm(mod3$residuals~datf_numeric$h2o_flux)
abline(a=mo3$coefficients[1],b=mo3$coefficients[2],col="red")
# наблюдается слабая зависимость остатков от наблюдаемых значений

#Построим модель, аналогичную модели 3, но убрав незначимые переменные 
mod4 = lm(data = datf_numeric, h2o_flux~ (qc_h2o_flux+co2_molar_density+RH
  +u_var+v_var+w_div_ts_cov+w_div_h2o_cov+h2o_signal_strength_7200+flowrate)^2
  -qc_h2o_flux:co2_molar_density-qc_h2o_flux:w_div_h2o_cov-qc_h2o_flux:h2o_signal_strength_7200
  -co2_molar_density:h2o_signal_strength_7200-RH:v_var-RH:w_div_h2o_cov
  -v_var:h2o_signal_strength_7200-w_div_ts_cov:h2o_signal_strength_7200)
summary(mod4)
#коэф детерминации = 0,9998
anova(mod4)
#имеется еще ряд незначимых переменных

#Построим модель, дополнительно убрав незначимые переменные, выявленные в модели 4 
mod5 = lm(data = datf_numeric, h2o_flux~ (qc_h2o_flux+co2_molar_density+RH
        +u_var+v_var+w_div_ts_cov+w_div_h2o_cov+h2o_signal_strength_7200+flowrate)^2
        -qc_h2o_flux:co2_molar_density-qc_h2o_flux:w_div_h2o_cov-qc_h2o_flux:h2o_signal_strength_7200
          -co2_molar_density:h2o_signal_strength_7200-RH:v_var-RH:w_div_h2o_cov
          -v_var:h2o_signal_strength_7200-w_div_ts_cov:h2o_signal_strength_7200
          -co2_molar_density:RH-w_div_h2o_cov:h2o_signal_strength_7200)
summary(mod5)
#коэф детерминации = 0,9998
anova(mod5)
#Больше незначимых переменных не наблюдаем
# Построим график наблюдаемых значений от предсказанных значений
plot(mod5$fitted.values, datf_numeric$h2o_flux)
abline(a=0, b=1, col="red")
#аппроксимация остается очень хорошей
# Построим график нормального распределения:
plot(mod5,2)
# остатки в центре распределены нормально, по-прежнему отличия в хвостах
# Построим график зависимости остатков от наблюдаемых значений 
plot(datf_numeric$h2o_flux,mod5$residuals)
# аппроксимируем
mo5=lm(mod5$residuals~datf_numeric$h2o_flux)
abline(a=mo5$coefficients[1],b=mo5$coefficients[2],col="red")
# наблюдается слабая зависимость остатков от наблюдаемых значений

#Вывод: при наличии среди параметров сильнокоррелирующей переменной с коэф корреляции
#близким к 1, можно обойтись одномерной линейной регрессией по этой переменной (Модель 1)
#построение моделей множественной регрессии (Модели 2-5) улучшает точность модели, но не настолько
#сильно, насколько увеличивается сложность модели
#Из всех моделей множественной регрессии (2-5) наиболее точна модель 3, т.к. имеет
#минимальное значение информационного критерия Акаике (AIC)
AIC(mod2)
AIC(mod3)
AIC(mod4)
AIC(mod5)
#минимальное значение Байесовского информационного критерия (BIC)
BIC(mod2)
BIC(mod3)
BIC(mod4)
BIC(mod5)


#Построим модель 6 без сильнокоррелирующей переменной w_div_h2o_cov
mod6 = lm(data = datf_numeric, h2o_flux~ qc_h2o_flux+co2_molar_density+RH
          +u_var+v_var+w_div_ts_cov+h2o_signal_strength_7200+flowrate)
summary(mod6)
#коэффициент детерминации 0,7801
anova(mod6)
#все переменные значимы
# Построим график наблюдаемых значений от предсказанных значений
plot(mod6$fitted.values, datf_numeric$h2o_flux)
abline(a=0, b=1, col="red")
#не очень хорошая аппроксимация
# Построим графиик нормального распределения:
plot(mod6,2)
# остатки в центре распределены нормально, по-прежнему отличия в хвостах
# Построим график зависимости остатков от наблюдаемых значений 
plot(datf_numeric$h2o_flux,mod6$residuals)
mo6=lm(mod6$residuals~datf_numeric$h2o_flux)
abline(a=mo6$coefficients[1],b=mo6$coefficients[2],col="red")
# наблюдается зависимость остатков от наблюдаемых значений

#Попробуем улучшить модель 6 за счет учета зависимостей 2-го порядка
mod7 = lm(data = datf_numeric, h2o_flux~ (qc_h2o_flux+co2_molar_density+RH
          +u_var+v_var+w_div_ts_cov+h2o_signal_strength_7200+flowrate)^2)
summary(mod7)
#коэффициент детерминации 0,8566
anova(mod7)
#Имеется ряд незначимых переменных
# Построим график наблюдаемых значений от предсказанных значений
plot(mod7$fitted.values, datf_numeric$h2o_flux)
abline(a=0, b=1, col="red")
#не очень хорошая аппроксимация
# Построим графиик нормального распределения:
plot(mod7,2)
# остатки в центре распределены нормально, по-прежнему отличия в хвостах
# Построим график зависимости остатков от наблюдаемых значений 
plot(datf_numeric$h2o_flux,mod7$residuals)
mo7=lm(mod7$residuals~datf_numeric$h2o_flux)
abline(a=mo7$coefficients[1],b=mo7$coefficients[2],col="red")
# сохраняется зависимость остатков от наблюдаемых значений

#Уберем из модели 7 незначимые переменные 2-го порядка
mod8 = lm(data = datf_numeric, h2o_flux~ (qc_h2o_flux+co2_molar_density+RH
        +u_var+v_var+w_div_ts_cov+h2o_signal_strength_7200+flowrate)^2
        -qc_h2o_flux:u_var-qc_h2o_flux:v_var-qc_h2o_flux:w_div_ts_cov
        -qc_h2o_flux:flowrate-co2_molar_density:RH-co2_molar_density:v_var
        -co2_molar_density:w_div_ts_cov-co2_molar_density:h2o_signal_strength_7200
        -u_var:flowrate-w_div_ts_cov:flowrate-h2o_signal_strength_7200:flowrate)
summary(mod8)
#коэффициент детерминации 0,8246
anova(mod8)
#Имеется ряд незначимых переменных

#Построим модель, дополнительно убрав незначимые переменные, выявленные в модели 8
mod9 = lm(data = datf_numeric, h2o_flux~ (qc_h2o_flux+co2_molar_density+RH
            +u_var+v_var+w_div_ts_cov+h2o_signal_strength_7200+flowrate)^2
          -qc_h2o_flux:u_var-qc_h2o_flux:v_var-qc_h2o_flux:w_div_ts_cov
          -qc_h2o_flux:flowrate-co2_molar_density:RH-co2_molar_density:v_var
          -co2_molar_density:w_div_ts_cov-co2_molar_density:h2o_signal_strength_7200
          -u_var:flowrate-w_div_ts_cov:flowrate-h2o_signal_strength_7200:flowrate
          -qc_h2o_flux:RH-RH:flowrate-u_var:v_var-u_var:h2o_signal_strength_7200
          -v_var:h2o_signal_strength_7200-v_var:flowrate-w_div_ts_cov:h2o_signal_strength_7200)
summary(mod9)
#коэффициент детерминации 0,8232
anova(mod9)
#Больше незначимых переменных не наблюдаем
# Построим график наблюдаемых значений от предсказанных значений
plot(mod9$fitted.values, datf_numeric$h2o_flux)
abline(a=0, b=1, col="red")
# Построим графиик нормального распределения:
plot(mod9,2)
# остатки в центре распределены нормально, по-прежнему отличия в хвостах
# Построим график зависимости остатков от наблюдаемых значений 
plot(datf_numeric$h2o_flux,mod9$residuals)
mo9=lm(mod9$residuals~datf_numeric$h2o_flux)
abline(a=mo9$coefficients[1],b=mo9$coefficients[2],col="red")
# сохраняется зависимость остатков от наблюдаемых значений

#Из моделей 6-9 множественной регрессии, наиболее точную аппроксимацию дает модель 7
#Модель 7 дает максимальный Adj-R2 (скорректированный на кол-во переменных R-квадрат) = 0,8526
#Минимальное значение информационного критерия Акаике (AIC)
AIC(mod6)
AIC(mod7)
AIC(mod8)
AIC(mod9)
#Минимальное значение Байесовского информационного критерия (BIC)
BIC(mod6)
BIC(mod7)
BIC(mod8)
BIC(mod9)
#что также показывает наибольшую точность модели 7
