data <- read.csv("cvr_sample.csv")

# 線形モデル
lm.formula <- "cvr ~ score + f_holiday + f_sunday + f_monday + f_tuesday + f_wednesday + f_thirsday + f_1_word + f_2_word"
lm.model <- lm(lm.formula, data = data)
lm.est <- fitted(lm.model) * data$click #コンバージョンのフィット値



# 一般化線形モデル
glm.formula <- "cbind(conversion, click - conversion) ~ 
                score + f_holiday + f_sunday + f_monday + f_tuesday + f_wednesday + 
                f_thirsday + f_1_word + f_2_word"


glm.model <- glm(glm.formula, family = binomial, data = data)
glm.est <- fitted(glm.model) * data$click #コンバージョンのフィット値



# 一般化線形混合モデル
glmm.formula <- "cbind(conversion, click - conversion) ~ (1 | criterion_id) + 
                 score + f_holiday + f_sunday + f_monday + f_tuesday + f_wednesday + 
                 f_thirsday + f_1_word + f_2_word"
glmm.model <- glmer(glmm.formula, family = binomial, data = data)
glmm.est <- fitted(glmm.model) * data$click #コンバージョンのフィット値


#'
#'グラフ表示
#'
myplot <- function(formula, title) {
  plot(formula, ylim=c(-0.2, 1.5), lwd=3,  
       xlab="コンバージョンの実績値", 
       ylab="コンバージョンのフィット値", 
       main=title)
  grid(nx = 20, ny = 10, col = "gray", lty = "dotted",
       lwd = par("lwd"), equilogs = TRUE)
  abline(h=0, v=0,col="cyan")
  abline(0,1,"blue",lty=2)
}

par(mfrow=c(1,3))
actual <- data$conversion
myplot(lm.est ~ actual, "線形モデルによるコンバージョンのフィット値と実績値")
myplot(glm.est ~ actual, "一般化線形モデルによるコンバージョンのフィット値と実績値")
myplot(glmm.est ~ actual, "一般化線形混合モデルによるコンバージョンのフィット値と実績値")
