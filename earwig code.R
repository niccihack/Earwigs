#06/25/17
#N. Hack
#Earwig stats and graphs


#load packages
packages <- c('readxl','dplyr','car','ggplot2','combinat','multcomp','lsmeans')
lapply(packages, require, character.only = T)

#Data manipulation
HackIyengar_CohabitationData_Recoded062417 <- read_excel("C:/Users/manic/Dropbox/Home computer/Earwigs/HackIyengar-CohabitationData-Recoded062417.xlsx")
trials <- HackIyengar_CohabitationData_Recoded062417[,1:7]

#Statistics
trials$cohabitation = as.factor(trials$cohabitation)
trials$category = as.factor(trials$category)
trials$size = as.factor(trials$size)
trials$shelters = as.factor(trials$shelters)
trials$sex = as.factor(trials$sex)
trials$time = as.factor(trials$time)
tglm = glm(cohabitation~size+shelters+sex+time, family = binomial, data = trials)
sumtglm = summary(tglm)
capture.output(sumtglm, file = 'earwig_fullmodel.txt')
cat = glm(cohabitation~category-1, family = binomial, data = trials)
sumcat = summary(cat)
capture.output(sumcat, file = 'earwigs_category.txt')
posthoccat = summary(glht(cat, mcp(category = 'Tukey')))
capture.output(posthoccat, file = 'posthoc_category.txt')


#loop to find significant interactions
vars <- data.frame(c('size','sex','shelters','time'))
comb <- combn(1:4,2)#make table of all combinations
# allocate list first: 
# https://stackoverflow.com/questions/5599896/how-do-i-store-arrays-of-statistical-models
glmList <- vector(mode="list", length=ncol(comb))
for (k in 1:ncol(comb)){
  par1 = comb[2,k] %>% vars[.,1] %>% as.character(.)#taking index of combination table from variable list
  par2 = comb[1,k] %>% vars[.,1] %>% as.character(.)#needs to be character for dplyr::select to work
  # var1 = dplyr::select(trials, contains(par1))
  # var2 = dplyr::select(trials, contains(par2))
  # To do a model in a loop, you make the formula first as a string and run it through the formula function. 
  # https://stackoverflow.com/questions/8121542/r-specifying-variable-name-in-function-parameter-for-a-function-of-general-uni
  frm<-paste('cohabitation~',par1,'*', par2, sep="")
  myglm = glm(formula(frm), family = binomial, data = trials)#does not work for some reason!!!
  sumglm = summary(myglm)
  glmList[[k]] <- sumglm
}
capture.output(glmList, file = 'earwig.glms.txt')

#final model determined from loop
fullglm = glm(cohabitation~size+time*shelters+sex*shelters, family = binomial, data = trials)
sumfullglm = summary(fullglm)
capture.output(sumfullglm, file = 'earwigs_finalglm.txt')
car::Anova(fullglm, type =2)#If you want to use likelihood-ratio chisquared distribution instead of normal distribution in glm
posthocsexfull = summary(glht(fullglm, mcp(sex = 'Tukey')))
posthocsex = summary(glht(tglm,mcp(sex = 'Tukey')))
posthocsexint= lsmeans(fullglm, pairwise~sex*shelters, adjust = 'Tukey')
capture.output(posthocsexint, file = 'earwigs.fullGLM.Tukey-sex.txt')
posthoctime= summary(glht(tglm, mcp(time = 'Tukey')))
posthoctimeint= lsmeans(fullglm, pairwise~time*shelters, adjust = 'Tukey')
posthocsize = summary(glht(fullglm, mcp(size = 'Tukey')))




##using original data
OG <- read_excel("C:/Users/manic/Dropbox/Home computer/Earwigs/earwig-code/NicciHack-DynamicDuoData-072313(Blank).xlsx")
OG <- OG[,1:12]
OG$TrialType = as.factor(OG$TrialType)
OG$Cohabitation12h = as.factor(OG$Cohabitation12h)
OG$Cohabitation24h = as.factor(OG$Cohabitation24h)
OG$Locationat12 = as.factor(OG$Locationat12)
OG$Locationat24 = as.factor(OG$Locationat24)
OG$size = as.factor(OG$size)
OG$shelter = as.factor(OG$shelter)
OG$Sex = as.factor(OG$Sex)
difsize = dplyr::filter(OG, shelter == 'shelter' & size == 'different')
size = dplyr::filter(difsize, EarwigID == 'L' | EarwigID == 'S')
MF = dplyr::filter(OG, shelter == 'shelter' & TrialType == 'MF')
FF = dplyr::filter(OG, shelter == 'shelter' & TrialType == 'FF')
sizeglm = glm(Locationat12~EarwigID+TrialType, family = binomial, data = size)
summary(sizeglm)
MFglm = glm(Locationat24~Sex, family = binomial, data = MF)
summary(MFglm)
ggplot(MF, aes(x = Sex, fill = Locationat24))+
  geom_bar()+
  theme_classic()


#graphing
ggplot(trials, aes(x = category, fill = cohabitation))+
  geom_bar()+
  theme_classic()

d2 <- trials %>% 
  group_by(size,cohabitation) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = size, y = perc*100, fill = cohabitation))+
  geom_bar(stat="identity")+
  theme_classic()

ggplot(trials, aes(x = size, y = sex, color= cohabitation))+
  geom_jitter()+
  theme_classic()

ggplot(trials, aes(x = size, y =  cohabitation))+
  geom_jitter()+
  theme_classic()

ggplot(trials, aes(x = time, y = shelters, color = cohabitation))+
  geom_jitter()+
  theme_classic()

ggplot(trials, aes(x = sex, y = shelters, color = cohabitation))+
  geom_jitter()+
  theme_classic()


