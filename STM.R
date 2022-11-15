# szükséges csomagok betöltése
install.packages("stm")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tm")
install.packages("igpraph")

library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(ggplot2)
library(tidyverse)
library(tm)

#adatbázis beolvasás és szükséges változók létrehozása

data <- read.csv("STM/stm.csv", encoding = "UTF-8")
processed <- textProcessor(data$sigbig_1104_2, metadata=data, lowercase = FALSE, removestopwords = FALSE, removenumbers = FALSE, removepunctuation = FALSE, stem = FALSE)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

meta <- out$meta


kResult2 <- searchK(out$documents, out$vocab, K=c(17:22), prevalence=~Oldal, data=meta)
plot(kResult2)



poliblogPrevFit2 <- stm(out$documents, out$vocab, K=20, max.em.its=10, data=meta, prevalence=~Oldal_sz, init.type="Spectral", 
                        seed=84)

# létrejött topikok vizualizációja
plot(poliblogPrevFit2, type="summary", xlim=c(0,.4))

plot(poliblogPrevFit2, type="labels", topics = 1:4)
plot(poliblogPrevFit2, type="labels", topics = 5:9)
plot(poliblogPrevFit2, type="labels", topics = 10:13)
plot(poliblogPrevFit2, type="labels", topics = 14:17)
plot(poliblogPrevFit2, type="labels", topics = 18:20)







prep2 <- estimateEffect(1:20 ~ Oldal_sz, poliblogPrevFit2, meta=out$meta, 
                        uncertainty="Global")

summary(prep2, topics =1)

# topic proportions

plot(prep2, covariate = "Oldal_sz", topics = c(1:20),
     model = poliblogPrevFit2, method = "difference",
     cov.value1 = "Ellenzeki", cov.value2 = "Kormanyparti",
     xlab = "Inkább Ellenzéki ... Inkább Kormánypárti",
     main = "Oldalhoz tartozás hatása",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Topic1', 'Topic2', 'Topic3', 'Topic4', 
                       'Topic5', 'Topic6', 'Topic7', 'Topic8', 
                       'Topic9', 'Topic10', 'Topic11', 'Topic12', 
                       'Topic13', 'Topic14', 'Topic15', 'Topic16',
                       'Topic17', 'Topic18', 'Topic19','Topic20'))

poliblogContent <- stm(out$documents, out$vocab, K = 20, prevalence =~ Oldal_sz,
                       content =~ Oldal_sz, max.em.its = 10, data = out$meta,
                       init.type = "Spectral")


plot(poliblogContent, type = "perspectives", topics = 1)

plot(poliblogContent, type = "perspectives", topics = c(12, 20))



# topic proportions

# korreláció vizsgálat
mod.out.corr2 <- topicCorr(poliblogPrevFit2)
plot(mod.out.corr2)


topic1_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 1, n = 30)
topic2_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 2, n = 10)
topic3_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 3, n = 25)
topic4_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 4, n = 10)
topic5_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 5, n = 10)
topic6_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 6, n = 10)
topic7_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 7, n = 10)
topic8_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 8, n = 10)
topic9_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 9, n = 10)
topic10_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 10, n = 20)
topic11_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 11, n = 10)
topic12_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 12, n = 10)
topic13_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 13, n = 10)
topic14_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 14, n = 10)
topic15_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 15, n = 10)
topic16_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 16, n = 10)
topic17_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 17, n = 10)
topic18_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 18, n = 10)
topic19_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 19, n = 20)
topic20_szoveg <- findThoughts(poliblogPrevFit2, texts = data$nolink2, topics = 20, n = 10)


topic1_szoveg
topic2_szoveg
topic3_szoveg
topic4_szoveg
topic5_szoveg
topic6_szoveg
topic7_szoveg
topic8_szoveg
topic9_szoveg
topic10_szoveg
topic11_szoveg
topic12_szoveg
topic13_szoveg
topic14_szoveg
topic15_szoveg
topic16_szoveg
topic17_szoveg
topic18_szoveg
topic19_szoveg
topic20_szoveg
### topik prevalencia értékek

## Put labels in a vector
labels <- c("topic 1", "topic 2", "topic 3", "topic 4", "topic 5", "topic 6", "topic 7", "topic 8", "topic 9", "topic 10", "topic 11", "topic 12", "topic 13", "topic 14", "topic 15", "topic 16", "topic 17", "topic 18", "topic 19", "topic 20")

## Extract theta from the stm-model
df <- data.frame(labels)
proportion <- as.data.frame(colSums(poliblogPrevFit2$theta/nrow(poliblogPrevFit2$theta)))
df <- cbind(df, proportion)
colnames(df) <- c("Labels", "Probability")

## Sort the dataframe
df <- df[order(-proportion), ] 
df$Labels <- factor(df$Labels, levels = rev(df$Labels))
df$Probability <- as.numeric(df$Probability)
df$Probability <- round(df$Probability, 4)

## Plot graph
ggplot(df, aes(x = Labels, y = Probability)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = c(0, 0.15), limits = c(0, 0.15), expand = c(0, 0)) + #change breaks and limits as you need
  coord_flip() + 
  geom_text(aes(label = scales::percent(Probability)), #Scale in percent
            hjust = -0.25, size = 4,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) + 
  theme(panel.border = element_blank())

out$meta$Oldal_sz <- as.factor(out$meta$rating)
prep3 <- estimateEffect(1:20 ~ Oldal_sz, poliblogPrevFit2,
                       meta = out$meta, uncertainty = "Global")
summary(prep3, topics=1:20)


szovegek <- data.frame(topic1_szoveg)

warnings()
