## 计算阿尔法多样性
install.packages("dplyr")
install.packages("tidyr")
install.packages("vegan")
# 添加一列都为1的个体数量，以方便求和
# 计算物种-多度矩阵
library(dplyr)
library(tidyr)
sp <- read.csv("物种信息.csv", header = T)

sp$abund=1
data <- sp
spe=tapply(data$abund,list(data$plot,data$species),sum)

#为了后面计算的方便，将NA的地方改为0。但是不是真的0,因此物种数据不适合做线性模型
spe[is.na(spe)]=0
sum(spe)

write.csv(spe, "物种-多度矩阵.csv")
head(spe)

# 使用vegen包计算生物多样性指数
library(vegan)
N0 <- rowSums(spe > 0)               # 物种丰富度
H <- diversity(spe)                    # Shannon熵指数
N1 <- exp(H)                        # Shannon多样性指数
N2 <- diversity(spe, "inv")              # Simpson多样性指数
J <- H/log(N0)                        # Pielou均匀度
E1 <- N1/N0                          # Shannon均匀度 (Hill比率)
E2 <- N2/N0                          # Simpson均匀度 (Hill比率)


abun <- data.frame(spe)
## 将数据行名改为第一列，命名为plot
abun <- tibble::rownames_to_column(abun,"plot")
head(abun)
## 将宽数据转化为长数据
sp_abun <- abun %>% pivot_longer(-plot, names_to = "variable", values_to = "value")
## 命名
names(sp_abun) <- c("plot", "sp", "num")
head(sp_abun)
sp_abun
write.csv(sp_abun, "物种多度数据.csv")

str(sp)
unique(sp$species)
sp_dis <- table(sp$species)    ##计算物种个体数，所有物种

write.csv(sp_dis, "物种个体数.csv")

