library(ggplot2)
library(dplyr)
library(rmarkdown)


get_mouse_data <- function(){
  if (file.exists("Data_Cortex_Nuclear.xls")){
    return(readxl::read_xls("Data_Cortex_Nuclear.xls"))
  }
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00342/Data_Cortex_Nuclear.xls", 
                       destfile = paste0(getwd(),"/Data_Cortex_Nuclear.xls"), method = "wininet", mode = "wb")
  return(readxl::read_xls("Data_Cortex_Nuclear.xls"))
  
}


mddf <- get_mouse_data() #mouse_down_data_frame(mddf)

str(mddf)
control_group <- mddf[mddf$Genotype == "Control",]
str(control_group)
unique(mddf$class)

length(unique(sub('_[0-9]*','',mddf$MouseID)))

ncol(mddf)
colnames(mddf)
table(mddf$class)/15

sum(is.na(mddf))

mddf[is.na(mddf)]

hahaha <- data.frame(c(1,1),c(1,1))

sum(hahaha == 0)

sum(mddf[,2:78] == 0)
prot <- mddf[,2:78]

prot_wo_na <- prot[!is.na(prot)]
sum(prot_wo_na == 0)

mddf$MouseID

which.min(as.vector(prot))

mddf$RRP1_N[which.min(mddf$RRP1_N)] <- abs(mddf$RRP1_N[which.min(mddf$RRP1_N)])

mean(mddf$RRP1_N) - 3 * sd(mddf$RRP1_N)

sum(mddf[,2:78],na.rm = T)

mddf[which.min(mddf$RRP1_N),c("MouseID","RRP1_N","class")]

mean(mddf[mddf$class == "c-SC-m",]$RRP1_N)

mddf()
min(mddf[2:78], na.rm = T)

min(mddf$RRP1_N)

sum(prot < 0, na.rm = T)

mean(mddf$RRP1_N)
median(mddf$RRP1_N)

boxplot(mddf[2:78])

as.vector(mddf)

mddf$class <- as.factor(mddf$class)

ggplot(mddf, aes(x = class, y = BDNF_N))+
  geom_violin()+
  geom_boxplot(width = 0.2)+
  ggtitle("Уровень экспрессии белка BDNF_N в зависимости от класса мыши")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 0))


ggplot(mddf, aes(x = BDNF_N))+
  geom_histogram()+
  facet_wrap(~ class)

ggplot(mddf, aes(sample = BDNF_N), na.rm = T)+
  geom_qq()+
  geom_qq_line()+
  facet_wrap(~class)


mddf %>%
  group_by(class)%>%
  summarise(shapiro_stat = shapiro.test(BDNF_N)$statistic,
            shapiro_p_value = shapiro.test(BDNF_N)$p.value)

  
aaa <- shapiro.test(mddf$BDNF_N)

aaa$statistic

t.test(mddf$BDNF_N)


ks.test(mddf$BDNF_N, "pnorm", mean = mean(mddf$BDNF_N), sd = sd(mddf$BDNF_N), na.rm = T)

ks.

str(mddf)




BDNF_aov <- aov(BDNF_N ~ class, data = mddf)

summary(BDNF_aov)

BDNF_tukey <- TukeyHSD(BDNF_aov)
rownames(BDNF_tukey$class)

plot(TukeyHSD(BDNF_aov))


kruskal.test(BDNF_N ~ class, data = mddf)

pairwise.wilcox.test(mddf$BDNF_N, mddf$class, p.adjust.method = "bonf")

mddf_copy <- mddf

pairwise.wilcox.test(mddf_copy$BDNF_N, mddf_copy$class, p.adjust.method = "bonf")



mddf_copy <- na.replace(mddf_copy)
rm(hahaha)

sum(is.na(mddf_copy))

mddf_copy_prot <- mddf_copy[,c(2:78,82)]

erbb4_lm <- lm(ERBB4_N ~ .+., mddf_copy_prot)

summary(erbb4_lm)

corr.test(mddf_copy_prot)





















