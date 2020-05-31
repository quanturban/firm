library(dplyr)
library(data.table)
library(ggplot2)
library(poweRlaw)
library(ggrepel)

setwd("~/downloads/firm/data")

dt <- fread("GFRD_2005-2015_v1.csv", header = TRUE, sep = ",")

dt$total <- dt$primary + dt$secondary + dt$tertiary

dt.prov <- dt %>%
  group_by(province) %>%
  summarise(number = length(total),
            total_min = min(total), 
            total_max = max(total),
            total_mean = mean(total),
            total_median = median(total),
            total_sd = sd(total))

dt.sum <- dt %>%
  group_by(year, operation) %>%
  summarise(first_sum = sum(primary),
            second_sum = sum(secondary),
            third_sum = sum(tertiary))
dt.sum$total <- dt.sum$first_sum + dt.sum$second_sum + dt.sum$third_sum


# birth
dt.birth <-  dt.sum[dt.sum$operation == 1,]
ggplot(data = dt.birth) + 
  geom_line(size = 1.5, alpha = 0.8, color = "#c7e9b4",  aes(x = year, y = first_sum/1000)) + 
  geom_line(size = 1.5, alpha = 0.8, color = "#41b6c4", aes(x = year, y = second_sum/1000)) +
  geom_line(size = 1.5, alpha = 0.8, color = "#225ea8", aes(x = year, y = third_sum/1000)) +
  geom_line(size = 1.5, alpha = 0.8, color = "#081d58", aes(x = year, y = total/1000)) +
  xlab("Year") + ylab("Number of firms (1,000)") +
  scale_x_continuous(breaks=c(2005, 2010, 2015)) +
  theme_classic(base_size = 16)
ggsave("firm_birth.pdf", width = 4.5, height = 4)

# death
dt.death <-  dt.sum[dt.sum$operation == 0,]
ggplot(data = dt.death) + 
  geom_line(size = 1.5, alpha = 0.8, color = "#c7e9b4",  aes(x = year, y = first_sum/1000)) + 
  geom_line(size = 1.5, alpha = 0.8, color = "#41b6c4", aes(x = year, y = second_sum/1000)) +
  geom_line(size = 1.5, alpha = 0.8, color = "#225ea8", aes(x = year, y = third_sum/1000)) +
  geom_line(size = 1.5, alpha = 0.8, color = "#081d58", aes(x = year, y = total/1000)) +
  xlab("Year") + ylab("Number of firms (1,000)") +
  scale_x_continuous(breaks=c(2005, 2010, 2015)) +
  theme_classic(base_size = 16) 
ggsave("firm_death.pdf", width = 4.5, height = 4)


# lognormal fit
tmp1 <- dt[dt$year == 2015 & dt$operation == 1, ]$total
tmp2 <- dt[dt$year == 2015 & dt$operation == 0, ]$total
m_ln1 <- dislnorm$new(tmp1)
est1 <- estimate_xmin(m_ln1)
m_ln1$setXmin(est1)
m_ln2 <- dislnorm$new(tmp2)
est2 <- estimate_xmin(m_ln2)
m_ln2$setXmin(est2)

plot.data1 <- plot(m_ln1, draw = F)
fit.data1 <- lines(m_ln1, draw = F)
plot.data2 <- plot(m_ln2, draw = F)
fit.data2 <- lines(m_ln2, draw = F)

ggplot() + 
  geom_point(data=plot.data1, aes(x=log10(x), y=log10(y)), alpha = 0.5, size = 2) + 
  geom_line(data=fit.data1, aes(x=log10(x), y=log10(y)), size = 1.5, alpha = 0.8, color="red") +
  geom_point(data=plot.data2, aes(x=log10(x), y=log10(y)), alpha = 0.8, size = 2, color="#41b6c4") + 
  geom_line(data=fit.data2, aes(x=log10(x), y=log10(y)), size = 1.5, alpha = 0.8, color="blue") +
  labs(x="log10(Num_of_firms)", y="log10(CDF)") + ylim(-6, 0) +
  theme_classic(base_size = 16)
ggsave("firm_CDF.pdf", width = 4.5, height = 4)


###
city.sum <- dt %>%
  filter(year == 2015 & operation == 1) %>%
  group_by(city) %>%
  summarise(first_sum = sum(primary),
            second_sum = sum(secondary),
            third_sum = sum(tertiary),
            total = sum(total))
write.table(city.sum, file = "GFRD_2015_city_sum.csv", sep = ",", row.names = F)



###
data <- read.table("firm_yearbook.csv", sep = ",", header = T)

summary(lm(log10(GDP10000)~log10(first_sum) + log10(second_sum) + log10(third_sum), data = data))
summary(lm(log10(GDP10000)~log10(firm_total), data = data))
summary(lm(log10(GDP10000)~log10(light_sum), data = data))


summary(lm(log10(Fiscal10000)~log10(first_sum) + log10(second_sum) + log10(third_sum), data = data))
summary(lm(log10(Fiscal10000)~log10(firm_total), data = data))
summary(lm(log10(Fiscal10000)~log10(light_sum), data = data))

summary(lm(log10(Employment)~log10(first_sum) + log10(second_sum) + log10(third_sum), data = data))
summary(lm(log10(Employment)~log10(firm_total), data = data))
summary(lm(log10(Employment)~log10(light_sum), data = data))


model1 <- summary(lm(log10(GDP10000)~log10(firm_total), data = data))
p <- ggplot(aes(x = log10(firm_total), y = log10(GDP10000)), data = data) +
  geom_point(color = "#225ea8", size = 2, alpha = 0.5) +
  geom_abline(intercept = model1$coefficients[1,1], 
              slope = model1$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10(Num_of_firms)") + ylab("log10(GDP)") + 
  theme_classic(base_size = 16)
p
ggsave("firm_gdp.pdf", width = 4.5, height = 4)


model2 <- summary(lm(log10(Fiscal10000)~log10(firm_total), data = data))
p <- ggplot(aes(x = log10(firm_total), y = log10(Fiscal10000)), data = data) +
  geom_point(color = "#41b6c4", size = 2, alpha = 0.5) +
  geom_abline(intercept = model2$coefficients[1,1], 
              slope = model2$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10(Num_of_firms)") + ylab("log10(FiscalRevenue)") + 
  theme_classic(base_size = 16)
p
ggsave("firm_fiscal.pdf", width = 4.5, height = 4)


model3 <- summary(lm(log10(Employment)~log10(firm_total), data = data))
p <- ggplot(aes(x = log10(firm_total), y = log10(Employment)), data = data) +
  geom_point(color = "#081d58", size = 2, alpha = 0.3) +
  geom_abline(intercept = model3$coefficients[1,1], 
              slope = model3$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10(Num_of_firms)") + ylab("log10(Employment)") + 
  theme_classic(base_size = 16)
p
ggsave("firm_employment.pdf", width = 4.5, height = 4)



###
model4 <- summary(lm(log10(GDP10000)~log10(light_sum), data = data))
p <- ggplot(aes(x = log10(light_sum), y = log10(GDP10000)), data = data) +
  geom_point(color = "#225ea8", size = 2, alpha = 0.5) +
  geom_abline(intercept = model4$coefficients[1,1], 
              slope = model4$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10(NTL)") + ylab("log10(GDP)") + 
  theme_classic(base_size = 16)
p
ggsave("NTL_gdp.pdf", width = 4.5, height = 4)


model5 <- summary(lm(log10(Fiscal10000)~log10(light_sum), data = data))
p <- ggplot(aes(x = log10(light_sum), y = log10(Fiscal10000)), data = data) +
  geom_point(color = "#41b6c4", size = 2, alpha = 0.5) +
  geom_abline(intercept = model5$coefficients[1,1], 
              slope = model5$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10(NTL)") + ylab("log10(FiscalRevenue)") + 
  theme_classic(base_size = 16)
p
ggsave("NTL_fiscal.pdf", width = 4.5, height = 4)


model6 <- summary(lm(log10(Employment)~log10(light_sum), data = data))
p <- ggplot(aes(x = log10(light_sum), y = log10(Employment)), data = data) +
  geom_point(color = "#081d58", size = 2, alpha = 0.3) +
  geom_abline(intercept = model6$coefficients[1,1], 
              slope = model6$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10(NTL)") + ylab("log10(Employment)") + 
  theme_classic(base_size = 16)
p
ggsave("NTL_employment.pdf", width = 4.5, height = 4)

