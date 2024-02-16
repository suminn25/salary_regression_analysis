#install.packages ('readxl')
library(readxl)
library(tidyverse)
library(car)
library(GGally)
library(caret)
library(leaps)
setwd("C:/Users/sum/OneDrive/바탕 화면")
pay <- read_excel ("C:/Users/sum/OneDrive/바탕 화면/3-1/통계자료분석실습/분석데이터1/pay.xlsx")
pay
pay1 <- as_tibble(pay)
pay1 %>% head(n=10)
pay2 <- pay1 %>%
  mutate(total = BasePay + Bonus) %>%
  select(-c(8, 9))
pay2 %>% head(n=10)
pay2$Gender <- as.factor(pay2$Gender)
pay2$Education <- as.factor(pay2$Education)
pay2$Dept <- as.factor(pay2$Dept)
p <- pay2 %>% select(where(is.numeric)|where(is.factor)) %>%
  relocate(total, .after = last_col()) #반응변수 마지막으로 배치(그래프때문)
p
str(p)

p %>%
  select(where(is.numeric)) %>%
  ggcorr(label=TRUE, label_round=2)

ggpairs(p, lower = list(continuous = "smooth"))
# 자료 분리
set.seed(1234)
tot <- createDataPartition(p$total, p=0.8,
                            list=FALSE)
train_t <- p %>% slice(tot)
test_t <- p %>% slice(-tot)
fit_null <- lm(total ~ 1, train_t)
fit_full <- lm(total ~ ., train_t)
# 변수 선택
MASS::stepAIC(fit_null,
              scope=list(upper=fit_full, lower=fit_null),
              trace=FALSE)
MASS::stepAIC(fit_null, scope = list(lower = fit_null, upper = fit_full),
             k = log(nrow(train_t)),
             trace = FALSE)
fits <- regsubsets(total ~., train_t)
plot(fits)
plot(fits, scale="adjr2")
fit1 <- MASS::stepAIC(fit_null,
                      scope=list(upper=fit_full, lower=fit_null),
                      trace=FALSE)
summary(fit1)
p_1 <- p %>%
  filter(!(Education == "High School")) %>%
  filter(!(Dept == "Operations"))
set.seed(1234)
tot <- createDataPartition(p$total, p=0.8,
                           list=FALSE)
train_t <- p_1 %>% slice(tot)
test_t <- p_1 %>% slice(-tot)
fit_null <- lm(total ~ 1, train_t)
fit_full <- lm(total ~ ., train_t)
MASS::stepAIC(fit_null,
              scope=list(upper=fit_full, lower=fit_null),
              trace=FALSE)
fit2 <- MASS::stepAIC(fit_null,
                      scope=list(upper=fit_full, lower=fit_null),
                      trace=FALSE)
summary(fit2)
p_2 <- p_1 %>%
  filter(!(Dept == "Engineering"))
train_t <- p_2 %>% slice(tot)
test_t <- p_2 %>% slice(-tot)
fit_null <- lm(total ~ 1, train_t)
fit_full <- lm(total ~ ., train_t)
MASS::stepAIC(fit_null,
              scope=list(upper=fit_full, lower=fit_null),
              trace=FALSE)
fit3 <- MASS::stepAIC(fit_null,
                      scope=list(upper=fit_full, lower=fit_null),
                      trace=FALSE)
summary(fit3)
fit4 <- MASS::stepAIC(fit_null, scope = list(lower = fit_null, upper = fit_full),
                      k = log(nrow(train_t)),
                      trace = FALSE)
summary(fit4)
p_1 <- p %>%
  filter(!(Education == "High School"))
set.seed(1234)
tot <- createDataPartition(p$total, p=0.8,
                           list=FALSE)
train_t <- p_1 %>% slice(tot)
test_t <- p_1 %>% slice(-tot)
fit_null <- lm(total ~ 1, train_t)
fit_full <- lm(total ~ ., train_t)
fit5 <-  MASS::stepAIC(fit_null, scope = list(lower = fit_null, upper = fit_full),
                       k = log(nrow(train_t)),
                       trace = FALSE)
summary(fit5)
par(mfrow=c(2, 2))
plot(fit5, pch=20, labels.id = rownames(pay))
par(mfrow=c(1, 1))
car::crPlots(fit5)
vif(fit5)
influencePlot(fit5)
pred_t <- predict(fit5, newdata=test_t)
defaultSummary(data.frame(obs=test_t$total,
                          pred=pred_t))
summary(fit5)$sigma
summary(fit5)$r.squared

pred_t <- predict(fit5, newdata = test_t)
X_tr <- model.matrix(total ~., train_t)[,-1]
X_te <- model.matrix(total ~., test_t)[,-1]


library(glmnet)
cvfit_la <- cv.glmnet(X_tr, train_t$total)
cvfit_la
coef(cvfit_la)
pred_la <- predict(cvfit_la, newx = X_te)
defaultSummary(data.frame(obs=test_t$total,
                          pred=as.numeric(pred_la)))
ft <- cbind(test_t, pred=pred_t) %>%
  rownames_to_column(var="JopTitle")
ft %>%
  ggplot(aes(x=total, y=pred)) +
  geom_point() +
  geom_abline(aes(intercept=0, slope=1)) +
  geom_text(data=slice_max(ft, abs(total-pred), n=1),
            aes(label=JopTitle), nudge_y=0.3) +
  labs(y="Predicted values", x="Observed values")


