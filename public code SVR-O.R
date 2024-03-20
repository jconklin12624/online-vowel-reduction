#sample code for 
#"Examining recording quality from two methods of remote data collection in a study of vowel reduction"
#submitted to the Journal of the Association for Laboratory Phonology

#explanation of variable names
#widedata (a data file containing the full dataset with data paired according to experiment design, 
#i.e., "rot ~ carrot"): 
#subject: a random identifier for the speaker
#word_1: the first target word in the pair for this row
#word_2: the second target word in the pair for this row
#F1_1: the first formant value of the target vowel in word 1 in normalized units
#F2_1: the second formant value of the target vowel in word 1 in normalized units
#Duration_1: the duration of the target vowel in word 1 in milliseconds
#F1_2: the first formant value of the target vowel in word 2 in normalized units
#F2_2: the second formant value of the target vowel in word 2 in normalized units
#Duration_2: the duration of the target vowel in word 2 in milliseconds
#vowel: the identity of the target vowel (the same for word 1 and word 2)
#ED: Euclidean distance between the target vowels in words 1 and 2
#dur_ratio2:  duration ratio comparing target vowels in words 1 and 2
#DurationExclude: a binary variable identifying words that should not be included in the duration analysis
  #0 indicates that a word should be excluded when considering duration
  #1 indicates that a word should be included when considering duration
  #the duration of excluded words should not be considered accurate; more information is available in the manuscript
#extraction_method: the type of script used to extract acoustic information from annotated files
  #values are "automatic" and "hand" (i.e., hand corrected)
#recording_method: in person, recorded online in a lossy format via Gorilla, or recorded offline in a lossless format via smartphone

#fulldata: a data file containing the full data set in long format
#subject: a random identifier for the speaker
#word: the target word
#F1: the value of the first formant in Hertz
#F2: the value of the second formant in Hertz
#duration: the duration of the target vowel in milliseconds
#recording_method: in person, recorded online in a lossy format via Gorilla, or recorded offline in a lossless format via smartphone
#extraction_method: the type of script used to extract acoustic information from annotated files
  #values are "automatic" and "supervised" (i.e., hand corrected)
#dur_excl: a binary variable identifying words that should not be included in the duration analysis
  #0 indicates that a word should be excluded when considering duration
  #1 indicates that a word should be included when considering duration
  #the duration of excluded words should not be considered accurate; more information is available in the manuscript
#F1normed: the value of the first formant in normalized units
#F2normed: the value of the second formant in normalized units
#stress: a binary variable indicating if the target vowel in the target word was stressed or unstressed
#vowel: the identity of the target vowel

#pillai_summary_vert: a dataset containing Pillai scores for each speaker and vowel
#subject: a random identifier for the speaker
#recording_method: in person, recorded online in a lossy format via Gorilla, or recorded offline in a lossless format via smartphone
#extraction_method: the type of script used to extract acoustic information from annotated files
  #values are "automatic" and "supervised" (i.e., hand corrected)
#pillai: the Pillai score for that particular vowel produced by that speaker
#vowel: the identity of the target vowel
#n: the number of utterances informing the Pillai score

#fetch data and load libraries

library(tidyverse)
library(lme4)
library(lmerTest)
library(cowplot)

widedata <- read.csv(file.choose(), header = TRUE)
fulldata <- read.csv(file.choose(), header = TRUE)
pillai_summary_vert <- read.csv(file.choose(), header = TRUE)


#make in-person the default recording method

widedata$recording_method[widedata$recording_method == "In-Person"] <- "ainperson"
unique(widedata$recording_method)

fulldata$recording_method[fulldata$recording_method == "inperson"] <- "ainperson"
unique(fulldata$recording_method)

###############################
#f1

#recode extraction method for clearer plot labels

fulldata$extraction_method[fulldata$extraction_method == "hand"] <- "Supervised"
fulldata$extraction_method[fulldata$extraction_method == "auto"] <- "Automatic"
unique(fulldata$extraction_method)

#model

f1model <- lmer(formula=F1normed ~ 1 + extraction_method*recording_method + recording_method*vowel*stress + (1|word) + (1+vowel|subject), data = fulldata)
summary(f1model)

#plot - Figure 1

f1plot <- ggplot(fulldata, aes(x=factor(vowel), y=F1normed, fill=factor(recording_method)))+
  geom_boxplot() +
  theme_classic() + 
  guides(fill=guide_legend(title=NULL)) +
  facet_grid(rows =vars(stress), cols = vars(extraction_method)) + 
  scale_fill_brewer(name = NULL, labels=c("gorilla" = "Gorilla", "ainperson" = "In Person", "uploaded" = "Smartphone"), palette = "Accent") +
  scale_x_discrete(labels=c("/\U0251/", "/\U00E6/", "/\U025B/", "/\U026A/", "/\U028C/")) + 
  theme(text = element_text(size=32), legend.position = c(0.85, 0.5)) +
  panel_border(color = "black", size = 1, linetype = 1, remove = FALSE) +
  labs(title = NULL, x=NULL, y = "Normalized F1")
f1plot
ggsave("Fig1.tiff", width = 3500, height = 3500, units = "px", dpi = 300)

#pairwise comparisons

auto_a_str <- subset(fulldata, extraction_method == "Automatic" & vowel == "/ɑ/" & stress == "Stressed")
auto_v_str <- subset(fulldata, extraction_method == "Automatic" & vowel == "/ʌ/" & stress == "Stressed")
auto_ae_str <- subset(fulldata, extraction_method == "Automatic" & vowel == "/æ/" & stress == "Stressed")
auto_i_str <- subset(fulldata, extraction_method == "Automatic" & vowel == "/ɪ/" & stress == "Stressed")
auto_e_str <- subset(fulldata, extraction_method == "Automatic" & vowel == "/ɛ/" & stress == "Stressed")
hand_a_str <- subset(fulldata, extraction_method == "Supervised" & vowel == "/ɑ/" & stress == "Stressed")
hand_v_str <- subset(fulldata, extraction_method == "Supervised" & vowel == "/ʌ/" & stress == "Stressed")
hand_ae_str <- subset(fulldata, extraction_method == "Supervised" & vowel == "/æ/" & stress == "Stressed")
hand_i_str <- subset(fulldata, extraction_method == "Supervised" & vowel == "/ɪ/" & stress == "Stressed")
hand_e_str <- subset(fulldata, extraction_method == "Supervised" & vowel == "/ɛ/" & stress == "Stressed")
auto_a_unstr <- subset(fulldata, extraction_method == "Automatic" & vowel == "/ɑ/" & stress == "Unstressed")
auto_v_unstr <- subset(fulldata, extraction_method == "Automatic" & vowel == "/ʌ/" & stress == "Unstressed")
auto_ae_unstr <- subset(fulldata, extraction_method == "Automatic" & vowel == "/æ/" & stress == "Unstressed")
auto_i_unstr <- subset(fulldata, extraction_method == "Automatic" & vowel == "/ɪ/" & stress == "Unstressed")
auto_e_unstr <- subset(fulldata, extraction_method == "Automatic" & vowel == "/ɛ/" & stress == "Unstressed")
hand_a_unstr <- subset(fulldata, extraction_method == "Supervised" & vowel == "/ɑ/" & stress == "Unstressed")
hand_v_unstr <- subset(fulldata, extraction_method == "Supervised" & vowel == "/ʌ/" & stress == "Unstressed")
hand_ae_unstr <- subset(fulldata, extraction_method == "Supervised" & vowel == "/æ/" & stress == "Unstressed")
hand_i_unstr <- subset(fulldata, extraction_method == "Supervised" & vowel == "/ɪ/" & stress == "Unstressed")
hand_e_unstr <- subset(fulldata, extraction_method == "Supervised" & vowel == "/ɛ/" & stress == "Unstressed")

pairwise.t.test(auto_a_str$F1normed, auto_a_str$recording_method, p.adj="bonf") 
pairwise.t.test(auto_v_str$F1normed, auto_v_str$recording_method, p.adj="bonf")
pairwise.t.test(auto_ae_str$F1normed, auto_ae_str$recording_method, p.adj="bonf")
pairwise.t.test(auto_i_str$F1normed, auto_i_str$recording_method, p.adj="bonf")
pairwise.t.test(auto_e_str$F1normed, auto_e_str$recording_method, p.adj="bonf")
pairwise.t.test(hand_a_str$F1normed, hand_a_str$recording_method, p.adj="bonf")
pairwise.t.test(hand_v_str$F1normed, hand_v_str$recording_method, p.adj="bonf")
pairwise.t.test(hand_ae_str$F1normed, hand_ae_str$recording_method, p.adj="bonf")
pairwise.t.test(hand_i_str$F1normed, hand_i_str$recording_method, p.adj="bonf")
pairwise.t.test(hand_e_str$F1normed, hand_e_str$recording_method, p.adj="bonf")            
pairwise.t.test(auto_a_unstr$F1normed, auto_a_unstr$recording_method, p.adj="bonf") 
pairwise.t.test(auto_v_unstr$F1normed, auto_v_unstr$recording_method, p.adj="bonf")
pairwise.t.test(auto_ae_unstr$F1normed, auto_ae_unstr$recording_method, p.adj="bonf")
pairwise.t.test(auto_i_unstr$F1normed, auto_i_unstr$recording_method, p.adj="bonf")
pairwise.t.test(auto_e_unstr$F1normed, auto_e_unstr$recording_method, p.adj="bonf")
pairwise.t.test(hand_a_unstr$F1normed, hand_a_unstr$recording_method, p.adj="bonf")
pairwise.t.test(hand_v_unstr$F1normed, hand_v_unstr$recording_method, p.adj="bonf")
pairwise.t.test(hand_ae_unstr$F1normed, hand_ae_unstr$recording_method, p.adj="bonf")
pairwise.t.test(hand_i_unstr$F1normed, hand_i_unstr$recording_method, p.adj="bonf")
pairwise.t.test(hand_e_unstr$F1normed, hand_e_unstr$recording_method, p.adj="bonf")  

#compute difference in means

f1sum <- fulldata %>%
  group_by(extraction_method, vowel, stress, recording_method) %>%
  summarise(meanf1 = mean(F1normed))

f1sum_wide <- spread(f1sum, recording_method, meanf1)
f1sum_wide

f1sum_wide <- f1sum_wide %>%
  mutate(GorInp = gorilla - ainperson, 
         SmaInp = uploaded - ainperson, 
         SmaGor = uploaded - gorilla)


#################################
#f2

f2model <- lmer(formula=F2normed ~ 1 + extraction_method*recording_method*vowel*stress + (1|word) + (1+vowel|subject), data = fulldata)
summary(f2model)

#make a boxplot - Figure 2

f2plot <- ggplot(fulldata, aes(x=factor(vowel), y=F2normed, fill=factor(recording_method)))+
  geom_boxplot() +
  theme_classic() + 
  guides(fill=guide_legend(title=NULL)) +
  facet_grid(rows =vars(stress), cols = vars(extraction_method)) + 
  scale_fill_brewer(name = NULL, labels=c("gorilla" = "Gorilla", "ainperson" = "In Person", "uploaded" = "Smartphone"), palette = "Accent") +
  scale_x_discrete(labels=c("/\U0251/", "/\U00E6/", "/\U025B/", "/\U026A/", "/\U028C/")) + 
  theme(text = element_text(size=32), legend.position = c(0.85, 0.1)) +
  panel_border(color = "black", size = 1, linetype = 1, remove = FALSE) +
  labs(title = NULL, x=NULL, y = "Normalized F2")
f2plot
ggsave("Fig2.tiff", width = 3500, height = 3500, units = "px", dpi = 300)

################################
#six panel vowel plot - Figure 3

# New facet label names for recording method variable
rec.labs <- c("In Person", "Gorilla", "Smartphone")
names(rec.labs) <- c("ainperson", "gorilla", "uploaded")

#remove slashes from vowel variable for clearer visuals on crowded plot

relabelling <- c("/ʌ/" = "ʌ", "/ɪ/" = "ɪ", "/ɛ/" = "ɛ", "/æ/" = "æ", "/ɑ/" = "ɑ" )
fulldata$vowel2 <- relabelling[as.character(fulldata$vowel)]

#summarize data in order to label means

dfsum3 <- fulldata %>%                                        
  group_by(vowel2, stress, recording_method, extraction_method) %>%     
  summarise(meanf1 = mean(F1normed),              
            sdf1 = sd(F1normed), 
            sef1 = sd(F1normed)/sqrt(n()), 
            meanf2 = mean(F2normed),              
            sdf2 = sd(F2normed), 
            sef2 = sd(F2normed)/sqrt(n()))

#create and save plot

vowelplot <- ggplot(fulldata, aes(x = F2normed, y = F1normed, linetype=stress, color = vowel2, label = vowel2)) + 
  # geom_point(alpha = 0.2) + 
  stat_ellipse(level = 0.67) + 
  geom_text(data = dfsum3, aes(x = meanf2, y = meanf1), size=7) + 
  scale_x_reverse() + scale_y_reverse() + 
  scale_color_discrete(breaks = c("ɑ", "æ", "ɛ", "ɪ", "ʌ")) +
  facet_grid(rows =vars(extraction_method), cols = vars(recording_method), labeller = labeller(recording_method = rec.labs)) + 
  labs(x="Normalized F2", y="Normalized F1", linetype=NULL) + 
  guides(color = "none") + 
  theme_classic() +
  theme(text = element_text(size=32), legend.text=element_text(size=20),
        legend.background = element_blank(), legend.position = c(0.1, 0.05)) +
  panel_border(color = "black", size = 1, linetype = 1, remove = FALSE) 
vowelplot
ggsave("Fig3.tiff", width = 3500, height = 3500, units = "px", dpi = 300)

######################################

#plot Euclidean distance by condition

widedata$extraction_method[widedata$extraction_method == "Hand"] <- "Supervised"
widedata$extraction_method[widedata$extraction_method == "Auto"] <- "Automatic"
unique(widedata$extraction_method)

#create plot - Figure 4

eudplot <- ggplot(widedata, aes(x=factor(vowel), y=ED, fill=factor(recording_method)))+
  geom_boxplot() +
  theme_classic() + 
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_brewer(name = NULL, labels=c("Gorilla" = "Gorilla", "ainperson" = "In Person", "Uploaded" = "Smartphone"), palette = "Accent") +
  scale_x_discrete(labels=c("/\U0251/", "/\U00E6/", "/\U025B/", "/\U026A/", "/\U028C/")) + 
  facet_wrap(~extraction_method) + 
  theme(text = element_text(size=32), legend.position = c(0.9, 0.9)) +
  panel_border(color = "black", size = 1, linetype = 1, remove = FALSE) +
  labs(title = NULL, x=NULL, y = "Euclidean Distance")
eudplot
ggsave("Fig4.tiff", width = 4500, height = 3000, units = "px", dpi = 300)

#EuD model

eudmodel <- lmer(formula=ED ~ 1 + vowel + recording_method + extraction_method + vowel*recording_method + (1|word_1) + (1|subject), data = widedata)
summary(eudmodel)

####################################

#pillai scores

pillaimdl <- lmer(formula=pillai ~ 1 + vowel*recording_method + (1+vowel|subject), data = pillai_summary_vert)
summary(pillaimdl) 

#rename variable values

pillai_summary_vert$extraction_method[pillai_summary_vert$extraction_method == "Hand"] <- "Supervised"
pillai_summary_vert$extraction_method[pillai_summary_vert$extraction_method == "Auto"] <- "Automatic"
unique(pillai_summary_vert$extraction_method)

#make plot - Figure 5

pillaiplot <- ggplot(pillai_summary_vert, aes(x=factor(vowel), y=pillai, fill=factor(recording_method)))+
  geom_boxplot() +
  theme_classic() + 
  guides(fill=guide_legend(title=NULL)) +
  facet_wrap(~extraction_method) + 
  scale_fill_brewer(name = NULL, labels=c("gorilla" = "Gorilla", "ainperson" = "In Person", "uploaded" = "Smartphone"), palette = "Accent") +
  scale_x_discrete(labels=c("/\U0251/", "/\U00E6/", "/\U025B/", "/\U026A/", "/\U028C/")) + 
  theme(text = element_text(size=32), legend.position = c(0.1, 0.1)) +
  panel_border(color = "black", size = 1, linetype = 1, remove = FALSE) +
  labs(title = NULL, x=NULL, y = "Pillai Score for Each Speaker")
pillaiplot
ggsave("Fig5.tiff", width = 4500, height = 2500, units = "px", dpi = 300)

########################################################
#duration ratio

#In the widedata data frame, the DurationExclude variable identifies the items
#that were excluded from the durational analysis (either for having an /r/ or 
#for having been annotated without reference to vowel duration due to unclear 
#consonant-vowel boundaries in the spectrogram). 1 indicates that an item should be
#included in the temporal analysis; 0 indicates it should be excluded. 

#subset data to exclude /r/s and /l/s using our newly created variable

widedatadur <- subset(widedata, DurationExclude == 1)

#summarize data

durationsum <- widedatadur %>%                                      
  group_by(vowel, extraction_method, recording_method) %>%                        
  summarise(meandur = mean(dur_ratio2),             
            sddur = sd(dur_ratio2), 
            sedur = sd(dur_ratio2)/sqrt(n()))

#recode extraction method for clearer plot labels

durationsum$extraction_method[durationsum$extraction_method == "Hand"] <- "Supervised"
durationsum$extraction_method[durationsum$extraction_method == "Auto"] <- "Automatic"
unique(durationsum$extraction_method)

#make plot - Figure 6

#recording_method2 <- c("In Person", "Gorilla", "Uploaded")

durplot <- ggplot(widedatadur, aes(x=factor(vowel), y=dur_ratio2, fill=factor(recording_method)))+
  geom_boxplot() +
  theme_classic() + 
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_brewer(name = NULL, labels=c("ainperson" = "In Person", "Gorilla" = "Gorilla", "Uploaded" = "Smartphone"), palette = "Accent") +
  scale_x_discrete(labels=c("/\U0251/", "/\U00E6/", "/\U025B/", "/\U026A/", "/\U028C/")) + 
  facet_wrap(~extraction_method) +
  theme(text = element_text(size=32), legend.position = c(0.1, 0.9)) +
  panel_border(color = "black", size = 1, linetype = 1, remove = FALSE) +
  labs(title = NULL, x=NULL, y = "Duration Ratio")
durplot
ggsave("Fig6.tiff", width = 4500, height = 2500, units = "px", dpi = 300)

#duration ratio

durmodel <- lmer(formula=dur_ratio2 ~ 1 + vowel*recording_method + (1|word_1) + (1+vowel|subject), data = widedatadur)
summary(durmodel)