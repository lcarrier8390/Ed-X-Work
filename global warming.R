library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line()

p <- p + geom_hline(aes(yintercept = 0), col = "blue")

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

  average <- mean(temp_carbon$temp_anomaly)
  SD <- sd(temp_carbon$temp_anomaly)
  c(average = average, SD = SD)
  
  greenhouse_gases %>%
    ggplot(aes(year, concentration)) +
    geom_line() +
    facet_grid(rows = vars(gas), scales = "free") +
    geom_vline(aes(xintercept = 1850)) +
    ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
    ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")
  
  temp_carbon %>%
    ggplot(aes(year, carbon_emissions)) +
    geom_line()
  
  co2_time <- historic_co2 %>%
    ggplot(aes(year, co2, color = source)) +
    geom_line() +
    scale_x_continuous(limit = c(-3000, 2018))
  
  co2_time
