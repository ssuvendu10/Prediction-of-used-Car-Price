data<- read.csv(file="C://Users//DELL//Desktop//presidency university//proj_junior//CarPrice_sorted.csv", head=TRUE)

# Library load
library(tidyverse)
library(gridExtra)
library(DataExplorer)
library(car)
library(nortest)
library(lmtest)

str(data)

# Changing discretes column as a factor.

data$fueltype <- as.factor(data$fueltype)
data$aspiration <- as.factor(data$aspiration)
data$doornumber <- as.factor(data$doornumber)
data$carbody <- as.factor(data$carbody)
data$drivewheel <- as.factor(data$drivewheel)
data$enginelocation <- as.factor(data$enginelocation)
data$enginetype <- as.factor(data$enginetype)
data$fuelsystem <- as.factor(data$fuelsystem)
data$cylindernumber <- as.factor(data$cylindernumber)

# Dropping columns with no predictive value

data$car_ID <- NULL
data$CarName <- NULL

# Transforming feature cylindernumber from text to its numeric equivalent

levels(data$cylindernumber) <- c("8","5","4","6","3","12","2")
data$cylindernumber <-   as.numeric(as.character (data$cylindernumber))

summary(data)

# Missing Value

lapply(data,function(x) { sum(is.na(x))})

#EDA 
head(data,8)%>%kable()

# Fig Size funtion

fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}
# Correlation matrix, continuous variable

fig(14, 12)

plot_correlation(
  data,
  type = "c",
  cor_args = list("use" = "pairwise.complete.obs"),
  title = "Correlation matrix, continuous variable",
  theme_config = list(title = element_text(size=20),
                      axis.text.y = element_text(size = 15),
                      axis.text.x = element_text(hjust = 1, angle = 45,size = 15)))



# Collinearity check
cor(data$citympg, data$highwaympg, use = "complete.obs")

# Boxplot funtion by "price"
fig(18,16)
plot_boxplot(data, by = "price", nrow = 5L,
             geom_boxplot_args = list("outlier.color" = "red"),
             theme_config = list(text = element_text(size=18),
                                 axis.text.y = element_text(size = 15),
                                 axis.text.x = element_text(size = 15))
)

fig(17,8)

pl1 <- ggplot(data) +
  aes(x = reorder(enginelocation,price), y = price, fill = enginelocation) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "enginelocation") +
  theme_gray() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15)) 


pl2 <- ggplot(data) +
  aes(x = reorder(fueltype, price), y = price, fill = fueltype) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_gray() +
  labs(x = "fueltype") + 
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15)) 

grid.arrange(ncol = 2, nrow = 1,pl1,pl2)

pl1 <- ggplot(data) +
  aes(x = aspiration, y = price, fill = aspiration) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_gray() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15)) 

pl2 <- ggplot(data) +
  aes(x = doornumber, y = price, fill = doornumber) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_gray() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15)) 

grid.arrange(ncol = 2, nrow = 1,pl1,pl2)

pl1 <- ggplot(data) +
  aes(x = reorder(carbody,price), y = price, fill = carbody) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "carbody") + 
  theme_gray() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15)) 


pl2 <- ggplot(data) +
  aes(x = reorder(drivewheel,price), y = price, fill = drivewheel) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "drivewheel") +
  theme_gray() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15))  

grid.arrange(ncol = 2, nrow = 1,pl1,pl2)

pl1 <- ggplot(data) +
  aes(x = reorder(fuelsystem,price), y = price, fill = fuelsystem) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "fuelsystem")  +
  theme_gray() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15)) 

pl2 <- ggplot(data) +
  aes(x = reorder(enginetype,price), y = price, fill = enginetype) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "enginetype") +
  theme_gray() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15))  


grid.arrange(ncol = 2, nrow = 1,pl1,pl2)

#Boxplot of price

pl1 <- ggplot(data) +
  aes(x = "", y = price) +
  geom_boxplot(fill = "#00B9E3") +
  labs(x = "Price", y ="Value") + 
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15))


#Histogram of price

pl2 <- ggplot(data = data, aes(x = price)) +
  geom_histogram(bins = 50,aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#00B9E3") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(data$price),
                            sd = sd(data$price))) +
  ggtitle("Histogram with theorical normal dist. curve") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15))  

grid.arrange(ncol = 2, nrow = 1,pl1,pl2)

summary(data$price)

# Positive and negatively influencing features in the price variable

df <-
  subset(
    data,
    select = c(
      "wheelbase","carlength","carwidth", "carheight", "curbweight", "cylindernumber", "enginesize",
      "boreratio", "stroke", "compressionratio", "horsepower", "peakrpm", "price"
    )
  )

corr_car_data <-
  as.data.frame(t(cor(data$price, df, method = "pearson")))

corr_car_data$key <- rownames(corr_car_data)

ggplot(data = corr_car_data) +
  aes(x = reorder(key, V1),
      y = V1,
      fill = key) +
  geom_bar(stat = "identity") +
  ylab("Correlation") +
  xlab("Feature") +
  ggtitle("Numerical features with better predictive capacity") +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15))  +
  coord_flip()





# Scatter plot with better prediction of price value
fig(19,10)
df <-
  subset(
    data,
    select = c(
      "wheelbase","carlength","carwidth", "curbweight", "cylindernumber", "enginesize",
      "boreratio", "horsepower", "price"
    )
  )

plot_scatterplot(df, by ="price", ncol = 4L, nrow = 2L,
                 geom_point_args = list("stroke" = 0.1, "colour" = "blue"),
                 theme_config = list(text = element_text(size=18),
                                     axis.text.y = element_text(size = 15),
                                     axis.text.x = element_text(size = 15)))


# Logaritmic transformation of the variable price

data$TRF_price <- log10(data$price)

#Boxplot

pl1 <- ggplot(data) +
  aes(x = "", y = TRF_price) +
  geom_boxplot(fill = "#00B9E3") +
  labs(x = "Transformed Price", y ="Value") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15)) 

#Histogram

pl2 <- ggplot(data = data, aes(x = TRF_price)) +
  geom_histogram(bins = 50,aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#00B9E3") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(data$TRF_price),
                            sd = sd(data$TRF_price))) +
  ggtitle("Transformed Histogram with theorical normal dist. curve") +
  theme_bw() +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15)) 

grid.arrange(ncol = 2, nrow = 1,pl1,pl2)

# Q-Q plot comparison

par(mfrow=c(1,2))

qqnorm(data$price, pch = 19, col = "gray50",main = "Normal Q-Q Plot  price")
qqline(data$price)

qqnorm(data$TRF_price, pch = 19, col = "gray50", main = "Normal Q-Q Plot  TRF_Price")
qqline(data$TRF_price)


# Enginelocation

t1 <- data %>% filter(enginelocation == "front")
t2 <- data %>% filter(enginelocation == "rear")

t.test(t1$TRF_price,t2$TRF_price)



# fueltype

t1 <- data %>% filter(fueltype == "gas")
t2 <- data %>% filter(fueltype == "diesel")

t.test(t1$TRF_price,t2$TRF_price)

# aspiration

t1 <- data %>% filter(aspiration == "std")
t2 <- data %>% filter(aspiration == "turbo")

t.test(t1$TRF_price,t2$TRF_price)


# doornumber

t1 <- data %>% filter(doornumber == "four")
t2 <- data %>% filter(doornumber == "two")

t.test(t1$TRF_price,t2$TRF_price)


#ANOVA test

# fuelsystem

anova <- aov(data$TRF_price ~ data$fuelsystem)
summary(anova)

# enginetype

anova <- aov(data$TRF_price ~ data$enginetype)
summary(anova)

# drivewheel

anova <- aov(data$TRF_price ~ data$drivewheel)
summary(anova)



# carbody

anova <- aov(TRF_price ~ carbody, data = data)
summary(anova)

# Continuous variables under test

nombre_col <- c("symboling",  "wheelbase",
                "carlength",  "carwidth",
                "carheight",  "curbweight",
                "cylindernumber", "enginesize",
                "boreratio",  "stroke",
                "compressionratio", "horsepower", "peakrpm" )

# empty dataframe

df <-
  structure(
    list(
      Variable = character(),
      Intercept = numeric(),
      beta = numeric(),
      t_value = numeric(),
      p_value = numeric(),
      IC_2.5 = numeric(),
      IC_95 = numeric(),
      R_squared = numeric()
    ),
    class = "data.frame"
  )

# Extraction and tabulation of the most important values of the simple regression of each continuous variable.

for (i in nombre_col) {
  
  regresion <- lm(data$TRF_price ~ data[[i]])
  sumario <- summary(regresion)
  confianza <-  confint(regresion, level = 0.95)
  
  vector <-
    data.frame(
      Variable = i,
      Intercept = round(sumario$coefficients[1], 2), beta = round(sumario$coefficients[2], 4),
      t_value = round(sumario$coefficients[6], 2), p_value = sumario$coefficients[8],
      IC_2.5 = round(confianza[2, 1], 4), IC_95 = round(confianza[2, 2], 4),
      R_squared = sumario$r.squared
    )
  df <- rbind(df,vector)
}

# Features with no lineal relationship

df %>% filter(p_value > 0.05)

# Features with lineal relationship

df %>% filter(p_value < 0.05)

# Variable Carheight

regresion <- lm(data$TRF_price ~ data$carheight)
summary(regresion)
confint(regresion, level = 0.95)


# Remove discarded variable from model

desechadas <- c("symboling", "stroke", "carheight", "compressionratio", "peakrpm",
                "fueltype", "doornumber", "citympg", "highwaympg", "price")

# Data frame with significant variables for the model

CarPrice_predictoras <- data[ , !(names(data) %in% desechadas)]

# Linear model with accepted predictor variables

modelo <- lm(TRF_price ~., data = CarPrice_predictoras)
summary(modelo)

# Model Backward

modelo_backward <- step(object =modelo, direction = "backward", trace = 1)
summary(modelo_backward)


# Best lineal model until now.

modelo_backward$call


# Remove discarded variable from model
# New removed features: enginesize y boreratio

desechadas <- c("symboling", "stroke", "carheight", "compressionratio", "peakrpm", "fueltype", "doornumber","citympg", "highwaympg", "price", "enginesize", "boreratio")

# Data frame with significant variables for the model

CarPrice_predictoras <- data[ , !(names(data) %in% desechadas)]

# New backward model with less variables

modelo <- lm(TRF_price ~., data = CarPrice_predictoras)

modelo_backward2 <- step(object =modelo, direction = "backward", trace = 1)
(sumario2 <- summary(modelo_backward2))


#  Multicollinearity test

vif(modelo_backward2)


# Remove discarded variable from model
# New removed feature: enginetype

desechadas <- c("symboling", "stroke", "carheight", "compressionratio", "peakrpm", "fueltype",
                "doornumber", "citympg", "highwaympg", "price", "enginesize", "boreratio", "enginetype" )

# Data frame with significant variables for the model

CarPrice_predictoras <- data[ , !(names(data) %in% desechadas)]


# New backward model with less variables

modelo <- lm(TRF_price ~., data = CarPrice_predictoras)

modelo_backward3 <- step(object =modelo, direction = "backward", trace = 1)
(sumario3 <- summary(modelo_backward3))



#  Multicollinearity test

vif(modelo_backward3)


par(mfrow=c(2,2))
plot(modelo_backward3, col =c("#00B9E3"))

ggplot(data = modelo_backward3, aes(x = modelo_backward3$residuals)) +
  geom_histogram(bins = 50,aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#00B9E3") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(modelo_backward3$residuals),
                            sd = sd(modelo_backward3$residuals))) +
  ggtitle("Residuals histogram with theorical normal dist. curve") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15))


# Normality test

lillie.test(x = modelo_backward3$residuals)


# Homoscedasticity test

bptest(modelo_backward3)

# Detection and visualization of outliers

outlierTest (modelo_backward3)

par(mfrow=c(1,1))
influencePlot(modelo_backward3,col =c("#00B9E3") )

CarPrice_predictoras$regresion <- predict(modelo_backward3)           # Save vector of regression values
CarPrice_predictoras$residuos <- residuals(modelo_backward3)          # Save residuals
ggplot(CarPrice_predictoras, aes(x = regresion, y = TRF_price)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +       # regression line  
  geom_segment(aes(xend = regresion, yend = regresion), alpha = .2) + # vertical lines of residual
  geom_point(aes(color = abs(residuos), size = abs(residuos))) +      # size of points
  scale_color_continuous(low = "#00B9E3", high = "red") +             # color of the points mapped by residue size
  guides(color = FALSE, size = FALSE) +                               
  geom_point(aes(y = regresion), shape = 1) +
  ggtitle("Regression line obtained vs transformed of the price") +
  theme_gray() +
  theme(title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title = element_text(size=15))



