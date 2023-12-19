library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(micromap)
library(randomForest)
library(caret)
data("USstates")
###CLEANING###

#Import Data
df_flights <- read.csv("flights.csv")
df_airport_origin <-read.csv("airports.csv")
df_airport_destination <- read.csv("airports.csv")
df_airlines <- read.csv("airlines.csv")

#Differentiate Origin and Destination Airports  
colnames(df_airport_origin) <- paste0("ORIGIN_", colnames(df_airport_origin))
colnames(df_airport_destination) <- paste0("DESTINATION_", colnames(df_airport_destination))
  
#Merge Data Frames
df <- left_join(df_flights, df_airport_origin, by = c("ORIGIN_AIRPORT" = "ORIGIN_IATA_CODE"))
df <- left_join(df, df_airport_destination, by = c("DESTINATION_AIRPORT" = "DESTINATION_IATA_CODE"))
df <- left_join(df, df_airlines, by = c("AIRLINE" = "IATA_CODE"))
  
#Drop Duplicate Columns and Rename
df <- select(df, -AIRLINE, -ORIGIN_AIRPORT, -DESTINATION_AIRPORT)
colnames(df)[colnames(df) == "ORIGIN_AIRPORT.y"] = "ORIGIN_AIRPORT"
colnames(df)[colnames(df) == "DESTINATION_AIRPORT.y"] = "DESTINATION_AIRPORT"
colnames(df)[colnames(df) == "AIRLINE.y"] = "AIRLINE"

#Drop Flights w/ No Departure Time
df <- df[!(is.na(df$DEPARTURE_TIME) | df$DEPARTURE_TIME == ""), ]

#Convert Month and Day Of Week
df$MONTH[df$MONTH == 1] <- "January"
df$DAY_OF_WEEK[df$DAY_OF_WEEK == 1] <- "Monday"
df$DAY_OF_WEEK[df$DAY_OF_WEEK == 2] <- "Tuesday"
df$DAY_OF_WEEK[df$DAY_OF_WEEK == 3] <- "Wednesday"
df$DAY_OF_WEEK[df$DAY_OF_WEEK == 4] <- "Thursday"
df$DAY_OF_WEEK[df$DAY_OF_WEEK == 5] <- "Friday"
df$DAY_OF_WEEK[df$DAY_OF_WEEK == 6] <- "Saturday"
df$DAY_OF_WEEK[df$DAY_OF_WEEK == 7] <- "Sunday"
  
#Add Columns For Additional Context
df <- df %>%
  mutate(DELAYED_1 = ifelse(df$DEPARTURE_DELAY < 60 & df$DEPARTURE_DELAY > 0, 1, 0)) %>%
  mutate(DELAYED_2 = ifelse(df$DEPARTURE_DELAY <= 120 & df$DEPARTURE_DELAY >= 60, 1, 0)) %>%
  mutate(DELAYED_3 = ifelse(df$DEPARTURE_DELAY <= 180 & df$DEPARTURE_DELAY > 120, 1, 0)) %>%
  mutate(DELAYED_4 = ifelse(df$DEPARTURE_DELAY > 180, 1, 0)) %>%
  mutate(is_DELAYED = ifelse(df$DEPARTURE_DELAY > 0, 1, 0)) %>%
  mutate(ON_TIME = ifelse(df$DEPARTURE_DELAY > 0, 0, 1)) %>%
  mutate(region = state.name[match(ORIGIN_STATE, state.abb)])

###GRAPHS###

#On-time vs Delayed
paste0("Flights in January 2015: ", format(nrow(df), big.mark=",", scientific=FALSE))
paste0("# of Those Delayed: ", format(sum(df$is_DELAYED), big.mark=",", scientific=FALSE), " (",round((sum(df$is_DELAYED)/nrow(df))*100, 2),"%)")

#Departure/Arrival Correlation
#FIGURE 1
pairs(df[c("DEPARTURE_DELAY", "ARRIVAL_DELAY")])
cor(df[c("DEPARTURE_DELAY", "ARRIVAL_DELAY")], use = "complete.obs")

#Delays by Day
month_delayed_1 <- df %>% 
  group_by(DAY) %>% 
  summarise(DELAYED_1 = sum(DELAYED_1))
month_delayed_2 <- df %>% 
  group_by(DAY) %>% 
  summarise(DELAYED_2 = sum(DELAYED_2))
month_delayed_3 <- df %>% 
  group_by(DAY) %>% 
  summarise(DELAYED_3 = sum(DELAYED_3))
month_delayed_4 <- df %>% 
  group_by(DAY) %>% 
  summarise(DELAYED_4 = sum(DELAYED_4))

month_delays <- month_delayed_1 %>% 
  left_join(month_delayed_2, by = "DAY")
month_delays <- month_delays %>% 
  left_join(month_delayed_3, by = "DAY")
month_delays <- month_delays %>% 
  left_join(month_delayed_4, by = "DAY")

#FIGURE 2
ggplot(month_delays, aes(x = DAY)) +
  geom_line(aes(y = DELAYED_1, color = "< 1 Hour") ) +
  geom_line(aes(y = DELAYED_2, color = "1-2 Hours") ) +
  geom_line(aes(y = DELAYED_3, color = "2-3 Hours") ) +
  geom_line(aes(y = DELAYED_4, color = "> 3 Hours") ) +
  labs(x = "Day of Month",
       y = "# of Delays",
       title = "# of Delays by Day in January") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma) +
  guides(color = guide_legend(title = "Length"))

#Delays by Day Of Week
day_delays <- df %>%
  select(DAY_OF_WEEK, DELAYED_1, DELAYED_2, DELAYED_3, DELAYED_4) %>%
  pivot_longer(-c(DAY_OF_WEEK), names_to = "Length", values_to = "Value")

day_delays <- day_delays %>% 
  group_by(Length, DAY_OF_WEEK) %>% 
  summarise(total = sum(Value))

day_delays$Length <- factor(day_delays$Length, levels = c("DELAYED_1", "DELAYED_2", "DELAYED_3", "DELAYED_4"))

#FIGURE 3
ggplot(day_delays, aes(fill = Length, y = total, x = reorder(DAY_OF_WEEK, -total))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Day of Week",
       y = "# of Delays",
       title = "# of Delays by Day of Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(labels = c("< 1 Hour", "1-2 Hours", "2-3 Hours", "> 3 Hours")) +
  scale_y_continuous(labels = comma)

#Delays by AIRLINE
airline_delay <- df %>%
  select(AIRLINE, DELAYED_1, DELAYED_2, DELAYED_3, DELAYED_4) %>%
  pivot_longer(-c(AIRLINE), names_to = "Length", values_to = "Value")

airline_delay <- airline_delay %>% 
  group_by(Length, AIRLINE) %>% 
  summarise(total = sum(Value))

airline_totals <- airline_delay %>% 
  group_by(AIRLINE) %>% 
  summarise(whole = sum(total))

airline_delay <- airline_delay %>% 
  left_join(airline_totals,by="AIRLINE") %>%
  mutate(percent = total/whole)

airline_delay$Length <- factor(airline_delay$Length, levels = c("DELAYED_1", "DELAYED_2", "DELAYED_3", "DELAYED_4"))

#FIGURE 4
ggplot(airline_delay, aes(fill = Length, y = percent, x = AIRLINE)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Airline",
       y = "Percent",
       title = "Distribution of Delay Length by Airline") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels = c("< 1 Hour", "1-2 Hours", "2-3 Hours", "> 3 Hours")) +
  coord_flip()

#Linked Micro Map
statePolys <- create_map_table(USstates, IDcolumn = "ST")

mapDF <- df %>% 
  select(region, ORIGIN_STATE, is_DELAYED, ON_TIME) %>% 
  group_by(ORIGIN_STATE, region) %>% 
  summarise(is_DELAYED = sum(is_DELAYED), ON_TIME = sum(ON_TIME))

mapDF <- mapDF[!(is.na(mapDF$region) | mapDF$region == ""), ]

#FIGURE 5
mmplot(stat.data = mapDF, map.data = statePolys,
       panel.types = c("map", "dot_legend", "labels", "dot", "dot"),
       panel.data = list(NA, NA, "region", "is_DELAYED", "ON_TIME"),
       map.link = c("ORIGIN_STATE", "ID"),
       ord.by = "is_DELAYED",
       grouping = 5,
       median.row = F,
       plot.height = 4,
       plot.width = 10,
       colors = c("red", "green", "blue", "orange", "yellow"),
       panel.att = list(list(1, header = "Selected (Highlighted)\nPrevious (Gray)", panel.width = .3),
                        list(2, panel.width = .4),
                        list(3, header = "State", align = "left", panel.width = .3, text.size = .9),
                        list(4, header = "# of Flights\n Delayed",
                             graph.bgcolor = "lightgray", point.size = 1,
                             xaxis.title = "# of Flights\n(Thousands)", xaxis.ticks = list(0, 5000, 10000, 15000, 20000, 25000), xaxis.labels = list("0", "5", "10", "15", "20", "25")),
                        list(5, header = "# of Flights\n On-Time",
                             graph.bgcolor = "lightgray", point.size = 1,
                             xaxis.title = "# of Flights\n(Thousands)", xaxis.ticks = list(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000), xaxis.labels = list("0", "5", "10", "15", "20", "25", "30", "35"))))

#Delays by Origin Airport
top_10 <- df %>% 
  group_by(ORIGIN_AIRPORT) %>% 
  summarise(total = sum(is_DELAYED)) %>%
  top_n(10)

top_10 <- as.list(top_10$ORIGIN_AIRPORT)

airport_delays <- df %>%
  select(ORIGIN_AIRPORT, DELAYED_1, DELAYED_2, DELAYED_3, DELAYED_4) %>%
  pivot_longer(-c(ORIGIN_AIRPORT), names_to = "Length", values_to = "Value")

airport_delays <- airport_delays %>% 
  group_by(Length, ORIGIN_AIRPORT) %>% 
  summarise(total = sum(Value)) %>%
  filter(ORIGIN_AIRPORT %in% top_10)

airport_delays$Length <- factor(airport_delays$Length, levels=c("DELAYED_1", "DELAYED_2", "DELAYED_3", "DELAYED_4"))

#FIGURE 6
ggplot(airport_delays, aes(fill = Length, y = total, x = reorder(ORIGIN_AIRPORT, total))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Airport",
       y = "# of Delays",
       title = "Top 10 Delayed Airports") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(labels = c("< 1 Hour", "1-2 Hours", "2-3 Hours", "> 3 Hours")) +
  scale_y_continuous(labels = comma) +
  coord_flip()

###MODEL###

#Random Forest
model <- df %>%
  select(is_DELAYED, ORIGIN_STATE, ORIGIN_AIRPORT, AIRLINE, DAY_OF_WEEK, DAY)

model <- model[!(is.na(model$ORIGIN_STATE) | model$ORIGIN_STATE == ""), ]

sample <- sample(c(TRUE, FALSE), nrow(model), replace = TRUE, prob = c(0.7, 0.3))
train  <- model[sample, ]
test   <- model[!sample, ]

train <- train[sample(nrow(train), 5000), ]
test <- test[sample(nrow(test), 5000), ]

train$is_DELAYED <- as.factor(train$is_DELAYED)

flights.rf <- randomForest(is_DELAYED ~ .,
                        data = train,
                        importance = TRUE)

#FIGURE 7
plot(flights.rf)

#FIGURE 8
varImpPlot(flights.rf)

print(flights.rf)

pred_DELAY <- predict(flights.rf, newdata = test)

confusionMatrix(table(pred_DELAY,test$is_DELAYED))