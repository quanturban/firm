library(dplyr)
library(data.table)
library(ggplot2)
library(poweRlaw)
library(RColorBrewer)

### Load data
setwd("~/downloads/firm")
dt <- fread("data/GED_v2/GED_2005-2015_v2.csv", header = T, sep = ",")


### Table 3 | Summary stat.
dt.prov <- dt %>%
  group_by(province) %>%
  summarise(total_sum = sum(establish_total),
            total_min = min(establish_total), 
            total_max = max(establish_total),
            total_mean = mean(establish_total),
            total_sd = sd(establish_total))


### Figure 2 
dt.sum <- dt %>%
  group_by(date) %>%
  summarise(esta_sum = sum(establish_total),
            esta_pri = sum(establish_prim),
            esta_sec = sum(establish_seco),
            esta_ter = sum(establish_tert),
            other = sum(other_total))

# ref fig. 3 of http://www.gov.cn/zhuanti/2017-10/27/content_5234848.htm
year <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
gov.firm <- c(NaN, 3400, 3400, 3600, 4150, 4800, 5500, 5400, 6800, 10000, 12200) * 365

# companies with the same registered and cancelled year (2005-2015)
cancel <- c(287604, 243379, 215454, 210854, 233354, 234097, 
            237824, 197923, 244691, 363668, 433250)
dt.sum$cancel <- cancel
dt.sum$gov.firm <- gov.firm
dt.sum$ratio <- (dt.sum$esta_sum) / dt.sum$gov.firm
dt.sum$ratio.adj <- (dt.sum$esta_sum - dt.sum$cancel) / dt.sum$gov.firm


# Fig. 2a Establishment
ggplot(data = dt.sum) + 
  geom_line(aes(x = date, y = esta_pri/10^6),
            size = 1.5, alpha = 0.8, color = "#c7e9b4") + 
  geom_line(aes(x = date, y = esta_sec/10^6), 
            size = 1.5, alpha = 0.8, color = "#41b6c4") +
  geom_line(aes(x = date, y = esta_ter/10^6),
            size = 1.5, alpha = 0.8, color = "#225ea8") +
  geom_line(aes(x = date, y = gov.firm/10^6),
            size = 1.5, alpha = 0.8, linetype = "dashed") +
  geom_line(aes(x = date, y = esta_sum/10^6),
            size = 1.5, alpha = 0.8) +
  xlab("Year") + ylab("Num. of establishments (million)") +
  scale_x_continuous(breaks=c(2005, 2010, 2015)) +
  theme_classic(base_size = 16)
ggsave("establishments.pdf", width = 4.5, height = 4)


# Fig. 2b Ratio
ggplot(data = dt.sum) + 
  geom_line(aes(x = date, y = ratio),
            size = 1.5, alpha = 0.8) + 
  geom_line(aes(x = date, y = ratio.adj),
            size = 1.5, alpha = 0.8, linetype = "dotted") + 
  xlab("Year") + ylab("Coverage ratio") +
  scale_x_continuous(breaks=c(2005, 2010, 2015)) +
  ylim(0.6, 1.4) +
  theme_classic(base_size = 16)
ggsave("ratio.pdf", width = 4.5, height = 4)


# Fig. 2c lognormal fit
tmp1 <- dt[dt$date == 2015]$establish_total
tmp2 <- dt[dt$date == 2010]$establish_total
tmp3 <- dt[dt$date == 2005]$establish_total

m_ln1 <- dislnorm$new(tmp1)
est1 <- estimate_xmin(m_ln1)
m_ln1$setXmin(est1)

m_ln2 <- dislnorm$new(tmp2)
est2 <- estimate_xmin(m_ln2)
m_ln2$setXmin(est2)

m_ln3 <- dislnorm$new(tmp3)
est3 <- estimate_xmin(m_ln3)
m_ln3$setXmin(est3)

plot.data1 <- plot(m_ln1, draw = F)
fit.data1 <- lines(m_ln1, draw = F)
plot.data2 <- plot(m_ln2, draw = F)
fit.data2 <- lines(m_ln2, draw = F)
plot.data3 <- plot(m_ln3, draw = F)
fit.data3 <- lines(m_ln3, draw = F)

ggplot() + 
  geom_point(data=plot.data1, aes(x=log10(x), y=log10(y)), alpha = 0.5, size = 2) + 
  geom_line(data=fit.data1, aes(x=log10(x), y=log10(y)), size = 1.5, color="red") +
  geom_point(data=plot.data3, aes(x=log10(x), y=log10(y)), alpha = 0.8, size = 2, color="#225ea8") + 
  geom_line(data=fit.data3, aes(x=log10(x), y=log10(y)), size = 1.5, color="#225ea8") +
  labs(x="log10(Num. of establishments)", y="log10(CDF)") + ylim(-6, 0) +
  theme_classic(base_size = 16)
ggsave("establishment_CDF.pdf", width = 4.5, height = 4)


### Figure 4 | Validation
# city summary stat
city.sum <- dt %>%
  filter(date == 2015) %>%
  group_by(city) %>%
  summarise(prim_sum = sum(establish_prim),
            seco_sum = sum(establish_seco),
            tert_sum = sum(establish_tert),
            total = sum(establish_total))
#write.table(city.sum, file = "GED_2015_city_v2.csv", sep = ",", row.names = F)

# read yearbook, NTL data
yb <- read.table("data/yearbook_light.csv", sep = ",", header = T)

# combine
yb.firm <- merge(yb, city.sum, by = "city")

# GDP, firm, NTL
summary(lm(log10(GDP10000)~log10(total), data = yb.firm)) #r2: 0.782
summary(lm(log10(GDP10000)~log10(prim_sum) + log10(seco_sum) + log10(tert_sum), data = yb.firm)) #r2: 0.812
summary(lm(log10(GDP10000)~log10(light_sum), data = yb.firm)) #r2: 0.749
summary(lm(log10(GDP10000)~log10(light_sum) + log10(total), data = yb.firm)) #r2: 0.837
summary(lm(log10(GDP10000)~log10(light_sum) + log10(prim_sum) + log10(seco_sum) + log10(tert_sum), data = yb.firm)) #r2: 0.847

# fiscal rev, firm, NTL
summary(lm(log10(Fiscal10000)~log10(total), data = yb.firm)) #r2: 0.817
summary(lm(log10(Fiscal10000)~log10(prim_sum) + log10(seco_sum) + log10(tert_sum), data = yb.firm)) #r2: 0.860
summary(lm(log10(Fiscal10000)~log10(light_sum), data = yb.firm)) #r2: 0.710
summary(lm(log10(Fiscal10000)~log10(light_sum) + log10(total), data = yb.firm)) #r2: 0.844
summary(lm(log10(Fiscal10000)~log10(light_sum) + log10(prim_sum) + log10(seco_sum) + log10(tert_sum), data = yb.firm)) #r2: 0.868

# employment, firm, NTL
summary(lm(log10(Employment)~log10(total), data = yb.firm)) #r2: 0.761
summary(lm(log10(Employment)~log10(prim_sum) + log10(seco_sum) + log10(tert_sum), data = yb.firm)) #r2: 0.787
summary(lm(log10(Employment)~log10(light_sum), data = yb.firm)) #r2: 0.708
summary(lm(log10(Employment)~log10(light_sum) + log10(total), data = yb.firm)) #r2: 0.805
summary(lm(log10(Employment)~log10(light_sum) + log10(prim_sum) + log10(seco_sum) + log10(tert_sum), data = yb.firm)) #r2: 0.813

#
model1 <- summary(lm(log10(GDP10000)~log10(total), data = yb.firm))
p <- ggplot(aes(x = log10(total), y = log10(GDP10000)), data = yb.firm) +
  geom_point(color = "#225ea8", size = 2, alpha = 0.5) +
  geom_abline(intercept = model1$coefficients[1,1], 
              slope = model1$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10Num_of_firms") + ylab("log10GDP (10^4 RMB)") + 
  theme_classic(base_size = 16)
p
ggsave("firm_gdp_v2.pdf", width = 4.5, height = 4)

#
model2 <- summary(lm(log10(Fiscal10000)~log10(total), data = yb.firm))
p <- ggplot(aes(x = log10(total), y = log10(Fiscal10000)), data = yb.firm) +
  geom_point(color = "#41b6c4", size = 2, alpha = 0.5) +
  geom_abline(intercept = model2$coefficients[1,1], 
              slope = model2$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10Num_of_firms") + ylab("log10Fiscal revenue (10^4 RMB)") + 
  theme_classic(base_size = 16)
p
ggsave("firm_fiscal_v2.pdf", width = 4.5, height = 4)

#
model3 <- summary(lm(log10(Employment)~log10(total), data = yb.firm))
p <- ggplot(aes(x = log10(total), y = log10(Employment)), data = yb.firm) +
  geom_point(color = "#081d58", size = 2, alpha = 0.3) +
  geom_abline(intercept = model3$coefficients[1,1], 
              slope = model3$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10Num_of_firms") + ylab("log10Employment") + 
  theme_classic(base_size = 16)
p
ggsave("firm_employment_v2.pdf", width = 4.5, height = 4)

#
model4 <- summary(lm(log10(GDP10000)~log10(light_sum), data = yb.firm))
p <- ggplot(aes(x = log10(light_sum), y = log10(GDP10000)), data = yb.firm) +
  geom_point(color = "#225ea8", size = 2, alpha = 0.5) +
  geom_abline(intercept = model4$coefficients[1,1], 
              slope = model4$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10NTL") + ylab("log10GDP (10^4 RMB)") + 
  theme_classic(base_size = 16)
p
ggsave("NTL_gdp_v2.pdf", width = 4.5, height = 4)

#
model5 <- summary(lm(log10(Fiscal10000)~log10(light_sum), data = yb.firm))
p <- ggplot(aes(x = log10(light_sum), y = log10(Fiscal10000)), data = yb.firm) +
  geom_point(color = "#41b6c4", size = 2, alpha = 0.5) +
  geom_abline(intercept = model5$coefficients[1,1], 
              slope = model5$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10NTL") + ylab("log10Fiscal revenue (10^4 RMB)") + 
  theme_classic(base_size = 16)
p
ggsave("NTL_fiscal_v2.pdf", width = 4.5, height = 4)

#
model6 <- summary(lm(log10(Employment)~log10(light_sum), data = yb.firm))
p <- ggplot(aes(x = log10(light_sum), y = log10(Employment)), data = yb.firm) +
  geom_point(color = "#081d58", size = 2, alpha = 0.3) +
  geom_abline(intercept = model6$coefficients[1,1], 
              slope = model6$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10NTL") + ylab("log10Employment") + 
  theme_classic(base_size = 16)
p
ggsave("NTL_employment_v2.pdf", width = 4.5, height = 4)


### Figure 5 Validation of scaling law
bj.popu <- read.table("data/Beijing_Popu_1km.csv", header = T, sep = ",")
bj.popu$idx <- paste0(bj.popu$lon, "_", bj.popu$lat)
bj.firm <- dt[dt$province == "北京市",]
bj.firm$idx <- paste0(bj.firm$lon, "_", bj.firm$lat)
bj.firm.sum <- bj.firm %>%
  group_by(idx) %>%
  summarise(firmtotal = sum(exist_total))

bj <- merge(bj.popu, bj.firm.sum, by = "idx")
bj1 <- bj[bj$day >= 1000 & bj$firmtotal > 1,]
summary(lm(log10(firmtotal)~log10(day), data = bj1))
# alpha = 1.16 (0.027)


# Map Population
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
ggplot(aes(x = lon, y = lat), data = bj1) + 
  geom_raster(aes(fill = day/1000)) +
  scale_fill_gradientn(colors = myPalette(100), values=seq(0, 100, length.out=100)/100) +
  labs(fill = "Popu (10^3)") + 
  xlim(116.2, 116.6) + ylim(39.75, 40.1) +
  theme_classic(base_size = 14) 
ggsave("beijing_day_popu.pdf", width = 5.5, height = 4)


# Map Firm
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
ggplot(aes(x = lon, y = lat), data = bj1) + 
  geom_raster(aes(fill = firmtotal/1000)) +
  scale_fill_gradientn(colors = myPalette(100), values=seq(0, 100, length.out=100)/100) +
  labs(fill = "Establishments (10^3)") + 
  xlim(116.2, 116.6) + ylim(39.75, 40.1) +
  theme_classic(base_size = 14) 
ggsave("beijing_firm.pdf", width = 5.5, height = 4)


# Regression
model.firm <- summary(lm(log10(firmtotal)~log10(day), data = bj1))
p <- ggplot(aes(x = log10(day), y = log10(firmtotal)), data = bj1) +
  geom_point(color = "#081d58", size = 1.5, alpha = 0.3) +
  geom_abline(intercept = model.firm$coefficients[1,1], 
              slope = model.firm$coefficients[2,1], 
              size = 1, alpha = 0.8) + 
  xlab("log10Population") + ylab("log10Establishments") + 
  theme_classic(base_size = 16)
p
ggsave("popu_firm.pdf", width = 4.5, height = 4)
