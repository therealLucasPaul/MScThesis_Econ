tstat <- 3.5
n <- 20
df <- n*2 - 2
pval <- 2 * (1-pt(abs(tstat), df))


y <- c(3.14, 1.3, 1.5)
mu_mle <- mean(y)
lambda_mle <- (length(y)*mu_mle^2)/(sum((y-mu_mle)^2/y))
print(mu_mle)

library(ggplot2)

set.seed(12345)
x <- as.data.frame(matrix(rnorm(50, 10, 2),nrow=5,ncol=10))
beta <- as.data.frame(c(0,0,0,3,0,0,0,0,0,2))
y <-  as.matrix(x) %*%  as.matrix(beta) + rnorm(5, 2, 4)
data <- cbind(y,x)
lm(y~., data = data)

cv_model<- glmnet::cv.glmnet(as.matrix(x), as.matrix(y), alpha = 1)
best_lambda <- cv_model$lambda.min
coef(glmnet::glmnet(x, y, alpha = 0, lambda = 10))

y
x 

ggplot(data.frame(Y=y, X=x), aes(X,Y))+
  geom_point(size = 2, col = 'red')+
  geom_smooth(method = "lm", formula = y ~ poly(x, 8), aes(color = "7"), se = F)+
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), aes(color = "4"), se = F)+
  labs(color = "Polynomial Degree") +
  scale_color_brewer(palette = "Set1") +
  ggtitle("Sparse Data Setting with Polynomial Fit") +
  theme_classic()+
  theme(panel.grid.major = element_line())

options(scipen = 10)
colnames(ytrain) = "Y"
dataset = cbind(ytrain, xtrain)
mod1 <- lm(Y ~., data = dataset)
summary(mod1)


