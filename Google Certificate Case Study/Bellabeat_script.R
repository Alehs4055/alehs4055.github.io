#Librerias
library(tidyverse)
library(rmarkdown)
library(janitor)
library(lubridate)

#Datos ocupados
#Está parte debe cambiar dependiendo de dónde almacenes los datos
daily_activity <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\dailyActivity_merged.csv")
sleep_day_merged <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\sleepDay_merged.csv")

#Carga de datos
daily_activity <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\dailyActivity_merged.csv")
daily_calories <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\dailyCalories_merged.csv")
daily_steps_merged <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\dailySteps_merged.csv")
daily_intensities <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\dailyIntensities_merged.csv")
heartrate_seconds <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\heartrate_seconds_merged.csv")
hourly_calories <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\hourlyCalories_merged.csv")
hourly_intensities <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\hourlyIntensities_merged.csv")
hourly_steps <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\hourlySteps_merged.csv")
minute_calories_narrow <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\minuteCaloriesNarrow_merged.csv")
minute_calories_Wide <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\minuteCaloriesWide_merged.csv")
minute_intensities_narrow <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\minuteIntensitiesNarrow_merged.csv")
minute_intensities_wide <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\minuteIntensitiesWide_merged.csv")
minute_METs_narrow <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\minuteMETsNarrow_merged.csv")
minute_sleep_merged <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\minuteSleep_merged.csv")
minute_steps_narrow <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\minuteStepsNarrow_merged.csv")
minute_steps_wide <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\minuteStepsWide_merged.csv")
sleep_day_merged <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\sleepDay_merged.csv")
weight_logInfo_merged <- read_csv("C:\\Users\\magno\\OneDrive\\Escritorio\\Curso Análisis de datos\\Modulo 8\\Fitabase Data 4.12.16-5.12.16\\weightLogInfo_merged.csv")

#Exploración de datos
head(daily_activity)
str(daily_activity)

head(sleep_day_merged)
str(sleep_day_merged)

summary(daily_activity)
summary(sleep_day_merged)

#Limpieza de datos
daily_activity <- clean_names(daily_activity)
sleep_day_merged <- clean_names(sleep_day_merged)

daily_activity$activity_date <- mdy(daily_activity$activity_date)
sleep_day_merged$sleep_day <- mdy_hms(sleep_day_merged$sleep_day)
sleep_day_merged$sleep_day <- as.Date(sleep_day_merged$sleep_day)

get_dupes(daily_activity, id, activity_date)
get_dupes(sleep_day_merged, id, sleep_day)
sleep_day_merged <- sleep_day_merged %>% 
  distinct()


#Transformación de datos
sleep_day_merged <- mutate(sleep_day_merged, time_to_fall_asleep=total_time_in_bed - total_minutes_asleep)
sleep_day_merged <- mutate(sleep_day_merged, total_hours_asleep=round(total_minutes_asleep/60, 2), nsmall=4)

#Gráficas

#Minutos para quedarse dormido
ggplot(data=sleep_day_merged) +
geom_freqpoly(mapping = aes(x=time_to_fall_asleep))+
  labs(title = "Tiempo en quedarse dormidos", x="Minutos para quedarse dormido",y="Número de registro")

summary(sleep_day_merged$time_to_fall_asleep)

#Horas de sueño
ggplot(data=sleep_day_merged) +
  geom_freqpoly(mapping = aes(x=total_hours_asleep))+
  labs(title = "Horas de sueño", x="Número de horas de sueño",y="Número de registros")

summary(sleep_day_merged$total_hours_asleep)


#Calorias contra tipos de actividad
ggplot(data = daily_activity)+
  geom_point(mapping = aes(x=very_active_minutes, y=calories), color="blue")+
  geom_smooth(mapping = aes(x=very_active_minutes, y=calories),method=lm, color="black")+
  labs(title="Relación de calorías en función de los minutos en una actividad física intensa", x="Minutos",y="Calorías", caption=paste("La línea negra representa correlación entre las variables, que es igual a",cor(x=daily_activity$very_active_minutes, y=daily_activity$calories)))
cor(x=daily_activity$very_active_minutes, y=daily_activity$calories)
summary(daily_activity$very_active_minutes)

ggplot(data = daily_activity)+
  geom_point(mapping = aes(x=fairly_active_minutes, y=calories), color="red")+
  geom_smooth(mapping = aes(x=fairly_active_minutes, y=calories),method=lm, color="black")+
  labs(title="Relación de calorías en función de los minutos en una actividad física moderada", x="Minutos",y="Calorías", caption=paste("La línea negra representa correlación entre las variables, que es igual a",cor(x=daily_activity$fairly_active_minutes, y=daily_activity$calories)))
cor(x=daily_activity$fairly_active_minutes, y=daily_activity$calories)
summary(daily_activity$fairly_active_minutes)

ggplot(data = daily_activity)+
  geom_point(mapping = aes(x=lightly_active_minutes, y=calories), color="orange")+
  geom_smooth(mapping = aes(x=lightly_active_minutes, y=calories),method=lm, color="black")+
  labs(title="Relación de calorías en función de los minutos en una actividad físca ligera", x="Minutos",y="Calorías", caption=paste("La línea negra representa correlación entre las variables, que es igual a",cor(x=daily_activity$lightly_active_minutes, y=daily_activity$calories)))
cor(x=daily_activity$lightly_active_minutes, y=daily_activity$calories)
summary(daily_activity$lightly_active_minutes)

ggplot(data = daily_activity)+  
  geom_point(mapping = aes(x=sedentary_minutes, y=calories), color="green")+
  geom_smooth(mapping = aes(x=sedentary_minutes, y=calories),method=lm, color="black")+
  labs(title="Relación de calorías en función de los minutos sedentario", x="Minutos",y="Calorías", caption=paste("La línea negra representa correlación entre las variables, que es igual a",cor(x=daily_activity$sedentary_minutes, y=daily_activity$calories)))
cor(x=daily_activity$sedentary_minutes, y=daily_activity$calories)
summary(daily_activity$sedentary_minutes)

#Relación de las intensidades de las actividades respecto a las calorías

ggplot(data = daily_activity)+
  geom_smooth(mapping = aes(x=very_active_minutes, y=calories, color="Intensa"),method=lm)+
  geom_smooth(mapping = aes(x=fairly_active_minutes, y=calories, color="Moderada"),method=lm)+
  geom_smooth(mapping = aes(x=lightly_active_minutes, y=calories, color="Ligera"),method=lm)+
  geom_smooth(mapping = aes(x=sedentary_minutes, y=calories, color="Sedentaria"),method=lm)+
  labs(title="Relación de calorías en función de los minutos en distintas actividades físicas", x="Minutos",y="Calorías",color="Legend")+
  coord_cartesian(xlim = c(0, 300),ylim = c(1000, 3500))+
  scale_color_manual(name="Intensidad de la actividad",
                     breaks=c("Intensa","Moderada","Ligera","Sedentaria"),
                     values=c("Intensa"="blue","Moderada"="red","Ligera"="orange","Sedentaria"="green"))

#Pruebas

colors <- c("Intensa"="blue","Moderada"="red","Ligera"="orange","Sedentaria"="green")
ggplot(data = daily_activity)+
  geom_smooth(mapping = aes(x=very_active_minutes, y=calories, color="Intensa"),method=lm)+
  geom_smooth(mapping = aes(x=fairly_active_minutes, y=calories, color="Moderada"),method=lm)+
  geom_smooth(mapping = aes(x=lightly_active_minutes, y=calories, color="Ligera"),method=lm)+
  geom_smooth(mapping = aes(x=sedentary_minutes, y=calories, color="Sedentaria"),method=lm)+
  labs(title="Relación de calorías en función de los minutos en distintas actividades físicas", x="Minutos",y="Calorías",color="Legend")+
  coord_cartesian(xlim = c(0, 300),ylim = c(1000, 3500))+
  scale_color_manual(name="Intensidad de la actividad",
                     breaks=c("Intensa","Moderada","Ligera","Sedentaria"),
                     values=c("Intensa"="blue","Moderada"="red","Ligera"="orange","Sedentaria"="green"))

ggplot(data = daily_activity)+
  geom_smooth(mapping = aes(x=calories, y=very_active_minutes), color="Blue")

ggplot(data = daily_activity) +
  geom_contour(mapping = aes(x=))

ggplot(data=sleep_day_merged) +
  geom_freqpoly(mapping = aes(x=fall_asleep_time))

ggplot(data=daily_activity) +
  geom_point(mapping = aes(x=total_steps, y=sedentary_minutes))

ggplot(data=daily_activity) +
  geom_smooth(mapping = aes(x=total_steps, y=sedentary_minutes))

ggplot(data=sleep_day_merged) +
  geom_point(mapping=aes(x=total_minutes_asleep,y=total_time_in_bed))

ggplot(data=sleep_day_merged) +
  geom_smooth(mapping=aes(x=total_minutes_asleep,y=total_time_in_bed))

ggplot(data=sleep_day_merged) +
  geom_area(mapping=aes(x=total_minutes_asleep,y=total_time_in_bed))

ggplot(data=sleep_day_merged) +
  geom_freqpoly(mapping = aes(x=total_minutes_asleep, color="blue"))+
  geom_freqpoly(mapping = aes(x=total_time_in_bed, color="red"))