# Can Tweets predict the Singapore GE2020 results?

This is a data analytics/visualisation project which uses scraped tweets to see if there is any correlation between how favourably electoral contestants are viewed on social media (Twitter) and their eventual results in GE2020.

**Methodology**
* scraped tweet data from twitter using python's getoldtweets3
* some basic cleaning of data
* compute sentiment scores of the contestants based on the tweets using sentimentr
* find r-coefficient of sentiment score and resulting vote share on different electoral areas

**Other Questions Explored**
* if allegiance of party are related to popularity
* use of other popularity indicators such as retweets, favourites, proportion of positive tweets instead of sentiment score. Does it lead to similar findings?

