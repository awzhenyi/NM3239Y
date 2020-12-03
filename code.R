library(dplyr)
library(tidyr)
library(textclean)
library(stringr)
library(tidytext)
library(textdata)
library(sentimentr)
library(ggplot2)
library(reshape2)

#filter out tweets from news source (cna reporters, chinesepop, yahoo, tocsg, businesstimes, news, mothership)
#clean text
news = "tocsg|news|cna|businesstimes|chinesepop|yahoosg|mothershipsg|asiaonecom|todayonline"
language = "[\U4E00-\U9FFF\U3000-\U303F]"
clean = function(df) {
  df = mutate_all(df, tolower)
  df = df %>%
    filter(!grepl(news, User)) %>%
    filter(!grepl(language, Text))
  df$Text = gsub("(http[^ ]*)|(www\\.[^ ]*)", "", df$Text)
  df$Text = gsub("ge2020", "", df$ Text)
  df$Text = replace_symbol(df$Text)
  df$Text = replace_internet_slang(df$Text)
  df$Text = replace_contraction(df$Text)
  df$Text = replace_emoticon(df$Text)
  df$Text = replace_number(df$Text)
  df$Text = gsub('[[:punct:]]', '', df$Text)
  return(df)
}

#get sentiment score and weighted sentiment score
get_sentiment_score = function(df) {
  require(sentimentr)
  df$sentiment = ''
  df$sentiment_score = 0
  df$weighted_sentiment_score = 0
  for(i in 1:nrow(df)) {
    df$sentiment_score[[i]] = sentiment(df$Text[[i]])$sentiment
    score = df$sentiment_score[[i]] 
    if(score > 0) {
      df$sentiment[[i]] = 'positive'
    } else if (score < 0) {
      df$sentiment[[i]] = 'negative'
    } else {
      df$sentiment[[i]] = 'neutral'
    }
    df$weighted_sentiment_score[[i]] = score + 
      (score*0.05*as.numeric(df$Favorites[[i]])) + 
      (score*0.1*as.numeric(df$Retweets[[i]]))
  }
  return(df)
}

#summarise total number of +ve/-ve/neutral tweets
summary_stats = function(df) {
  positive = length(which(df$sentiment == 'positive'))
  negative = length(which(df$sentiment == 'negative'))
  neutral = length(which(df$sentiment == 'neutral'))
  total = positive + negative + neutral
  count = c(positive, negative, neutral)
  proportion = c(positive/total, negative/total, neutral/total)
  general_sentiment = c("positive", "negative", "neutral")
  df1 = data.frame(general_sentiment, count, proportion)
  return(df1)
}

#summarise total sentiment score
summary_stats2 = function(df) {
  total_ss = sum(df$sentiment_score)
  weighted_total_ss = sum(df$weighted_sentiment_score)
  df1 = data.frame(total_ss, weighted_total_ss)
  return(df1)
}
#importing data and running all the functions for every candidate
paul_tambyah = read.csv("paul tambyah.csv", encoding = "UTF-8")
paul_tambyah_clean = clean(paul_tambyah)
paul_tambyah_sentiment = get_sentiment_score(paul_tambyah_clean)
paul_tambyah_summary = summary_stats(paul_tambyah_sentiment)
paul_tambyah_summary2 = summary_stats2(paul_tambyah_sentiment)

nicole_seah = read.csv("nicole seah.csv", encoding = "UTF-8")
nicole_seah_clean = clean(nicole_seah)
nicole_seah_sentiment = get_sentiment_score(nicole_seah_clean)
nicole_summary = summary_stats(nicole_seah_sentiment)
nicole_summary2 = summary_stats2(nicole_seah_sentiment)

jamus = read.csv("jamus lim.csv", encoding = "UTF-8")
jamus_clean = clean(jamus)
jamus_sentiment = get_sentiment_score(jamus_clean)
jamus_summary = summary_stats(jamus_sentiment)
jamus_summary2 = summary_stats2(jamus_sentiment)

csj = read.csv("soon juan.csv", encoding = "UTF-8")
csj_clean = clean(csj)
csj_sentiment = get_sentiment_score(csj_clean)
csj_summary = summary_stats(csj_sentiment)
csj_summary2 = summary_stats2(csj_sentiment)

raeesah = read.csv("raeesah.csv", encoding = "UTF-8")
raeesah_clean = clean(raeesah)
raeesah_sentiment = get_sentiment_score(raeesah_clean)
raeesah_summary = summary_stats(raeesah_sentiment)
raeesah_summary2 = summary_stats2(raeesah_sentiment)

joteo = read.csv("josephine teo.csv", encoding = "UTF-8")
joteo_clean = clean(joteo)
joteo_sentiment = get_sentiment_score(joteo_clean)
joteo_summary = summary_stats(joteo_sentiment)
joteo_summary2 = summary_stats2(joteo_sentiment)

hsk = read.csv("swee keat.csv", encoding = "UTF-8")
hsk_clean = clean(hsk)
hsk_sentiment = get_sentiment_score(hsk_clean)
hsk_summary = summary_stats(hsk_sentiment)
hsk_summary2 = summary_stats2(hsk_sentiment)

lim_tean = read.csv("lim tean.csv", encoding = "UTF-8")
lim_tean_clean = clean(lim_tean)
lim_tean_sentiment = get_sentiment_score(lim_tean_clean)
lim_tean_summary = summary_stats(lim_tean_sentiment)
lim_tean_summary2 = summary_stats2(lim_tean_sentiment)

murali = read.csv("murali pillai.csv", encoding = "UTF-8")
murali_clean = suppressWarnings(clean(murali))
murali_sentiment = get_sentiment_score(murali_clean)
murali_summary = summary_stats(murali_sentiment)
murali_summary2 = summary_stats2(murali_sentiment)

eng_hwa = read.csv("eng hwa.csv", encoding = "UTF-8")
eng_hwa_clean = clean(eng_hwa)
eng_hwa_sentiment = get_sentiment_score(eng_hwa_clean)
eng_hwa_summary = summary_stats(eng_hwa_sentiment)
eng_hwa_summary2 = summary_stats2(eng_hwa_sentiment)

pritam = read.csv("pritam singh.csv", encoding = "UTF-8")
pritam_clean = clean(pritam)
pritam_sentiment = get_sentiment_score(pritam_clean)
pritam_summary = summary_stats(pritam_sentiment)
pritam_summary2 = summary_stats2(pritam_sentiment)

cheng_bock = read.csv("cheng bock.csv", encoding = "UTF-8")
cheng_bock_clean = clean(cheng_bock)
cheng_bock_sentiment = get_sentiment_score(cheng_bock_clean)
cheng_bock_summary = summary_stats(cheng_bock_sentiment)
cheng_bock_summary2 = summary_stats2(cheng_bock_sentiment)

chee_meng = read.csv("chee meng.csv", encoding = "UTF-8")
chee_meng_clean = clean(chee_meng)
chee_meng_sentiment = get_sentiment_score(chee_meng_clean)
chee_meng_summary = summary_stats(chee_meng_sentiment)
chee_meng_summary2 = summary_stats2(chee_meng_sentiment)

iswaran = read.csv("s iswaran.csv", encoding = "UTF-8")
iswaran_clean = clean(iswaran)
iswaran_sentiment = get_sentiment_score(iswaran_clean)
iswaran_summary = summary_stats(iswaran_sentiment)
iswaran_summary2 = summary_stats2(iswaran_sentiment)

victor = read.csv("victor lye.csv", encoding = "UTF-8")
victor_clean = clean(victor)
victor_sentiment = get_sentiment_score(victor_clean)
victor_summary = summary_stats(victor_sentiment)
victor_summary2 = summary_stats2(victor_sentiment)

sylvia = read.csv("sylvia lim.csv", encoding = "UTF-8")
sylvia_clean = clean(sylvia)
sylvia_sentiment = get_sentiment_score(sylvia_clean)
sylvia_summary = summary_stats(sylvia_sentiment)
sylvia_summary2 = summary_stats2(sylvia_sentiment)

#merging dfs to visualise
oppositions.df = do.call("rbind", list(nicole_summary2, pritam_summary2, cheng_bock_summary2,
                                       jamus_summary2,raeesah_summary2,
                                       sylvia_summary2,csj_summary2,paul_tambyah_summary2,lim_tean_summary2))
names = c("Nicole Seah", "Pritam Singh", "Tan Cheng Bock", "Jamus Lim", "Raeesah Khan", 
          "Sylvia Lim", "Chee Soon Juan", "Paul Tambyah","Lim Tean") 
oppositions.df = cbind(names, oppositions.df)
type = c("WP","WP","PSP",'WP',"WP","WP","SDP","SDP","PV")
oppositions.df = cbind(oppositions.df,type)
#visualisation 1, bar chart for oppositions candidates and their sentiment score
base_plot = ggplot(data = oppositions.df, aes(x=reorder(names,total_ss), y = total_ss, fill=type)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Names", y = "Total Sentiment Score") +
  scale_fill_manual("Party", values = c("WP" = "skyblue1", "PSP" = "red", "SDP" = "red3", "PV" = "orchid1")) +
  theme_bw() +
  theme(axis.text=element_text(size = 8))

print(base_plot)

base_plot0 = ggplot(data = oppositions.df, aes(x=reorder(names,weighted_total_ss), y = weighted_total_ss, fill=type)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Names", y = "Total Weighted Sentiment Score") +
  scale_fill_manual("Party", values = c("WP" = "skyblue1", "PSP" = "red", "SDP" = "red3", "PV" = "orchid1")) +
  theme_bw() +
  theme(axis.text=element_text(size = 8))
print(base_plot0)


#Visualisation 2 (total engagement rate from number of tweets, retweets and favorites of oppositions and its pap opponents)

#function to capture average engagement rates for 2 candidates contesting under the same area 
duo_engagement = function(df, df1) {
  total_tweets = (nrow(df) + nrow(df1))/ 2 
  total_favourites = (sum(as.numeric(df$Favorites)) + sum(as.numeric(df1$Favorites)))/2
  total_retweets = (sum(as.numeric(df$Retweets)) + sum(as.numeric(df1$Retweets)))/2
  final_df = data.frame("Num_Tweets" = total_tweets,
                        "Total_Favourites" = total_favourites,
                        "Total_Retweets" = total_retweets)
  return(final_df)
}
#for only one contestant
engagement = function(df) {
  total_tweets = nrow(df)
  total_favourites = sum(as.numeric(df$Favorites))
  total_retweets  = sum(as.numeric(df$Retweets))
  final_df = data.frame("Num_Tweets" = total_tweets,
                        "Total_Favourites" = total_favourites,
                        "Total_Retweets" = total_retweets)
  return(final_df)
}
sk_oppo = duo_engagement(jamus_clean, raeesah_clean)
sk_pap = engagement(chee_meng_clean)

aljunied_oppo = duo_engagement(pritam_clean, sylvia_clean)
aljunied_pap = engagement(victor_clean)

ec_oppo = engagement(nicole_seah_clean)
ec_pap = engagement(hsk_clean)

wc_oppo = engagement(cheng_bock_clean)
wc_pap = engagement(iswaran_clean)

bb_oppo = engagement(csj_clean)
bb_pap = engagement(murali_clean)

bp_oppo = engagement(paul_tambyah_clean)
bp_pap = engagement(eng_hwa_clean)

jb_oppo = engagement(lim_tean_clean)
jb_pap =  engagement(joteo_clean)

engagement.df = do.call("rbind", list(sk_oppo,sk_pap,aljunied_oppo,aljunied_pap,ec_oppo,ec_pap,
                                      wc_oppo, wc_pap, bb_oppo, bb_pap, bp_oppo,bp_pap,jb_oppo,
                                      jb_pap))
area = c("Sengkang","Sengkang","Aljunied","Aljunied","East Coast", "East Coast", "West Coast","West Coast",
         "Bukit Batok","Bukit Batok", "Bukit Panjang", "Bukit Panjang", "Jalan Besar", "Jalan Besar")
allegiance = c("Opposition","PAP","Opposition","PAP","Opposition","PAP","Opposition","PAP",
               "Opposition","PAP","Opposition","PAP","Opposition","PAP")
engagement.df = cbind(area, engagement.df)
engagement.df = cbind(engagement.df, allegiance)
reshaped = gather(engagement.df, key = Type,value = Number, Num_Tweets,Total_Favourites,Total_Retweets)
base_plot2 = ggplot(reshaped, aes(x=Number, y=allegiance, fill = Type)) +
  geom_bar(stat='identity', width = 0.5) +
  facet_grid(area~.)+ 
  labs(x = 'Count', y = '') +
  scale_fill_manual(values = c("#d6d6d6", "#ff9f8f", "#ff3636")) +
  theme_bw()
print(base_plot2)

#visualisation 3 (r coefficient between engagement rates and actual vote share for oppositions)

#dropping even number rows which are pap results
oppo_engagement.df = engagement.df[seq(1,nrow(engagement.df),2),]
vote_share = c(52.13, 59.93, 46.59, 48.31,45.2,46.26,34.63)
oppo_engagement.df = cbind(oppo_engagement.df,vote_share)
for (i in 1:nrow(oppo_engagement.df)) {
  oppo_engagement.df$Total_Engagement[[i]] = oppo_engagement.df$Num_Tweets[[i]] +
    oppo_engagement.df$Total_Favourites[[i]] +
    oppo_engagement.df$Total_Retweets[[i]]
}
r.coeff = cor.test(oppo_engagement.df$Total_Engagement, oppo_engagement.df$vote_share)                
base_plot3 =ggplot(oppo_engagement.df, aes(x = Total_Engagement, y = vote_share)) + 
  geom_point(size = 4) +
  geom_text(label = oppo_engagement.df$area, vjust = 1.4, hjust = 0.4) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)+
  annotate(x=75000, y=52.2, label=paste0("r=",round(cor(oppo_engagement.df$Total_Engagement, oppo_engagement.df$vote_share),3)), geom="text") +
  labs(x="Total Engagement", y ='Vote Share (%)') +          
  theme_bw()
print(base_plot3)

#Visualisation 4, same as visualisation 3 except x-axis is now total sentiment score
duo_sentiment = function(df, df1) {
  ss = (df$total_ss + df1$total_ss)/2
  final_df = data.frame("Total_Sentiment_Score" = ss)
  return(final_df)
}

single_sentiment = function(df) {
  final_df = data.frame("Total_Sentiment_Score" = df$total_ss)
  return(final_df)
}

sk_oppo1 = duo_sentiment(jamus_summary2, raeesah_summary2)
sk_pap1 = single_sentiment(chee_meng_summary2)

aljunied_oppo1 = duo_sentiment(pritam_summary2, sylvia_summary2)
aljunied_pap1 = single_sentiment(victor_summary2)

ec_oppo1 = single_sentiment(nicole_summary2)
ec_pap1 = single_sentiment(hsk_summary2)

wc_oppo1 = single_sentiment(cheng_bock_summary2)
wc_pap1 = single_sentiment(iswaran_summary2)

bb_oppo1 = single_sentiment(csj_summary2)
bb_pap1 = single_sentiment(murali_summary2)

bp_oppo1 = single_sentiment(paul_tambyah_summary2)
bp_pap1 = single_sentiment(eng_hwa_summary2)

jb_oppo1 = single_sentiment(lim_tean_summary2)
jb_pap1 =  single_sentiment(joteo_summary2)

sentiment.df = do.call("rbind", list(sk_oppo1,sk_pap1,aljunied_oppo1,aljunied_pap1,ec_oppo1,ec_pap1,
                                     wc_oppo1, wc_pap1, bb_oppo1, bb_pap1, bp_oppo1,bp_pap1,jb_oppo1,
                                     jb_pap1))
sentiment.df = cbind(engagement.df, sentiment.df)
sentiment.df = sentiment.df[seq(1,nrow(sentiment.df),2),]
sentiment.df = cbind(sentiment.df, vote_share)

base_plot4 =ggplot(sentiment.df, aes(x = Total_Sentiment_Score, y = vote_share)) + 
  geom_point(size = 4) +
  geom_text(label = sentiment.df$area, vjust = 1.4, hjust = 0.4) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)+
  annotate(x=87, y=50, label=paste0("r=",round(cor(sentiment.df$Total_Sentiment_Score, sentiment.df$vote_share),3)), geom="text") +
  labs(x="Total Sentiment Score", y ='Vote Share (%)') +          
  theme_bw()
print(base_plot4)

#Visualisation 5, pie chart to show proportion of +ve/-ve/neutral tweets
base_plot5 = ggplot(pritam_summary,aes(x = "", y = proportion, fill = general_sentiment)) +
  geom_bar(width = 1, stat = 'identity', color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(proportion*100),"%")),position = position_stack(vjust=0.5))+
  scale_fill_manual(values = c("#F8766D","#619CFF","#00BA38")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Tweets on Pritam Singh") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust=0.5))

print(base_plot5)

