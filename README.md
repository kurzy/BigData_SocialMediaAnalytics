# BigData_SocialMediaAnalytics
This project collected, cleaned, and analysed social media data pertaining to the *Amazon Alexa/Echo* products, the [@Amazon](https://twitter.com/amazon?lang=en) Twitter page and the [Amazon](https://www.facebook.com/Amazon/) Facebook page during the summer of 2017.  

## Getting Started
The R script *data_analysis.r* contains the code for the project.  
Data was collected from Twitter and Facebook, and each of these platforms requires you to obtain your own API keys/secrets. You can choose to fill in your details and run the collection process again, or use the exsiting data contained within the .RData file.

## Execution
Make sure you have R installed on your machine. Download R from https://cran.r-project.org/.  
Make sure your R executable (e.g. C:\Program Files\R\R-xxxx\bin) is present in your PATH system environment variable.  
Run `R data_analysis.r`.  
The required R packages will be downloaded and installed for you when you run the script.  

## Analysis Results
### Friends vs Followers
![Friends vs Followers](https://github.com/kurzy/BigData_SocialMediaAnalytics/blob/master/img/FriendsVsFollowers.png)

### Semantic Network Graph
![Semantic Network Graph](https://github.com/kurzy/BigData_SocialMediaAnalytics/blob/master/img/semantic_net_gephi.PNG)

### Sentiment Analysis
![Sentiment Analysis](https://github.com/kurzy/BigData_SocialMediaAnalytics/blob/master/img/tw_sentiment.png)

### Sentiment Polarity Classification
![Sentiment Polarity](https://github.com/kurzy/BigData_SocialMediaAnalytics/blob/master/img/tw_polarity.png)

### Geographic Distribution (East)
![Geo East](https://github.com/kurzy/BigData_SocialMediaAnalytics/blob/master/img/world_map1.png)

### Geographic Distribution (West)
![Geo West](https://github.com/kurzy/BigData_SocialMediaAnalytics/blob/master/img/world_map2.png)
