library(tidyverse)
library(lubridate)
library(cluster)
library(gplots)
options(digits = 2, scipen = 100)

# Read data and mark missing data
metData <- read_csv("VTPO_MetData.txt",
                    na = c("M",""),
                    trim_ws = TRUE)
                    #stringsAsFactors = FALSE)

metData$presentwx <- gsub("- RA","RA",metData$presentwx)
metData$presentwx <- gsub("-RA","RA",metData$presentwx)
metData$presentwx <- gsub("[+]RA","RA",metData$presentwx)
metData$presentwx <- gsub("-DZ","RA",metData$presentwx)
metData$presentwx <- gsub("[+]DZ","RA",metData$presentwx)
metData$presentwx <- gsub("-TSRA BR","TS RA",metData$presentwx)
metData$presentwx <- gsub("-TSRA","TS RA",metData$presentwx)
metData$presentwx <- gsub("[+]TSRA","TS RA",metData$presentwx)
metData$presentwx <- gsub("FG","BR",metData$presentwx)
metData$presentwx <- gsub("HZ","BR",metData$presentwx)
metData$presentwx <- gsub("MIBR","BR",metData$presentwx)
metData$presentwx <- gsub("^SH$","RA",metData$presentwx)
metData$presentwx <- gsub("^SHRA$","RA",metData$presentwx)
metData$presentwx <- gsub("^TS$","TS RA",metData$presentwx)
metData$presentwx <- gsub("^TSRA$","TS RA",metData$presentwx)
metData$presentwx <- gsub("^TS VCSH$","TS RA",metData$presentwx)
metData$presentwx <- gsub("^VCSH$","VCRA",metData$presentwx)
metData$presentwx <- gsub("DZ","RA",metData$presentwx)
metData$presentwx <- gsub("BR RA","RA BR",metData$presentwx)

metData$presentwx[is.na(metData$presentwx)] <- "Normal"

metData %>%
  select(-mslp, -p01m) %>% 
  mutate(date = date(valid)) %>% 
  filter(!is.na(tmpc) & !is.na(skyl1)) %>% 
  select(-skyl1, -skyl2, -skyl3) -> metData1

metData1 %>% distinct() %>% 
  mutate(one = 1) %>% 
  spread(skyc1, one, sep = "_", fill = 0) %>% 
  mutate(one = 1) %>% 
  spread(skyc2, one, sep = "_", fill = 0) %>% 
  mutate(one = 1) %>% 
  spread(skyc3, one, sep = "_", fill = 0)  %>% 
  select(-skyc2_NA, -skyc3_NA)  %>% 
  filter(!is.na(relh), !is.na(drct), 
         !is.na(sknt), !is.na(vsby)) -> metData2

metData2 %>% 
  mutate(WX_BR = ifelse(grepl("BR", presentwx), 1, 0)) %>% 
  mutate(WX_NA = ifelse(grepl("Normal", presentwx), 1, 0)) %>% 
  mutate(WX_RA = ifelse(grepl("RA", presentwx), 1, 0)) %>% 
  mutate(WX_TS = ifelse(grepl("TS", presentwx), 1, 0)) %>% 
  select(-presentwx, -station) -> metData3
  
metData3 %>% 
  group_by(date) %>% 
  summarise(max_temp = max(tmpc), 
            min_temp = min(tmpc),
            mid_temp = median(tmpc),
            max_relh = max(relh),
            min_relh = min(relh),
            mid_relh = median(relh),
            max_sknt = max(sknt),
            min_sknt = min(sknt),
            mid_sknt = median(sknt),
            skyc1_BKN = mean(skyc1_BKN),
            skyc1_FEW = mean(skyc1_FEW),
            skyc1_SCT = mean(skyc1_SCT),
            skyc2_BKN = mean(skyc2_BKN),
            skyc2_FEW = mean(skyc2_FEW),
            skyc2_OVC = mean(skyc2_OVC),
            skyc2_SCT = mean(skyc2_SCT),
            skyc3_BKN = mean(skyc3_BKN),
            skyc3_FEW = mean(skyc3_FEW),
            skyc3_OVC = mean(skyc3_OVC),
            skyc3_SCT = mean(skyc3_SCT),
            WX_BR = mean(WX_BR),
            WX_NA = mean(WX_NA),
            WX_RA = mean(WX_RA),
            WX_TS = mean(WX_TS)) -> metData4

d <- daisy(metData4[,-1], metric = "gower")

fit <- hclust(d, method = "complete")

library(dendextend)
dend <- as.dendrogram(fit)
dend <- color_branches(dend, k = 5)
plot(dend)

cl <- cutree(fit, k = 5)
metData4 %>% 
  mutate(cluster = cl) -> metData5

centroid <- NULL
for(i in 1:5) {
  metData5 %>% 
    filter(cluster == i) -> metData5.1
  centroid[[i]] <- sapply(metData5.1, mean)
}

m <- t(matrix(unlist(centroid),ncol=5))
colnames(m) <- names(metData5)
df <- data.frame(m[,-1])
heatmap(m[,c(-1,-26)],scale = "column", col = redgreen(50))
write.csv(metData5,"output.csv")
