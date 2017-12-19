SI 671 project Work:

 #apt-get install -y python3-numpy cmake zlib1g-dev libjpeg-dev libboost-all-dev gcc libsdl2-dev wget unzip git
#Much like the literary world, the studio creates the game, and the publisher helps market and distribute the game.


# For each publisher, show the most important/weighted comment? like in this paper:  https://web.stanford.edu/class/cs224n/reports/2735436.pdf


R-Code (need a list of all titles released.)

# Cshell:
#cd Desktop
#cd "UMich Classes and Clubs"
#cd "SI 671"
#cd Project
python3
# Python Code

from textblob import TextBlob
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf

# get the dates working!
rownames = ( "id", "structure", "post_date", "comm_date",          
 "num_comments", "subreddit", "upvote_prop", "post_score",         
 "author",  "user",  "comment_score",  "controversiality", "comment", "title", "post_text", "link",               
 "domain",  "URL",  "Publisher", "W1Sales",            
"W2Sales",  "W3Sales",  "W4Sales", "Platform",           
 "Game_Name", "Release_Date", "Year", "CDays_since_release", "PDays_since_release")


# Upload the data.
df = pd.read_csv('final_comment_data_good.txt', sep = ";", names = rownames, header = 0, encoding = 'ISO-8859-1', low_memory = False)
df.head()
df.dtypes


# remove comments that are too old or too young to be relevant:
# if this works, remove the two and apply to all:
df = df.drop(df[df['CDays_since_release']> 730].index)
df = df.drop(df[df['CDays_since_release']< -30].index)



# Remove stopwords

from nltk import word_tokenize
from nltk.corpus import stopwords
import string
stop = stopwords.words('english') + list(string.punctuation)
# filter out the stop words and punctuation


df['filtered title'] = 0
k = 0
for j in df['title']:
	df['filtered title'][k] = [i for i in word_tokenize(str(j).lower()) if i not in stop]
	k += 1

k = 0
df['filtered comment'] = 0
for j in df['comment']:
	df['filtered comment'][k] = [i for i in word_tokenize(str(j).lower()) if i not in stop]
	k += 1

k = 0
df['filtered post_text'] = 0
for j in df['post_text']:
	df['filtered post_text'][k] = [i for i in word_tokenize(str(j).lower()) if i not in stop]
	k += 1


# Adding Sentiment values:

#Polarity"
df['comment_pol'] = df['comment'].map(lambda x: TextBlob(str(x)).sentiment.polarity)
df['title_pol'] = df['title'].map(lambda x: TextBlob(str(x)).sentiment.polarity)

#Subjectivity
df['comment_sub'] = df['comment'].map(lambda x: TextBlob(str(x)).sentiment.subjectivity)
df['title_sub'] = df['title'].map(lambda x: TextBlob(str(x)).sentiment.subjectivity)

# Make Controversy a float
df['controversiality'] = df['controversiality'].astype(float)

# How strongly does the community agree with the comment?
df['score_size'] = df['comment_score'].abs()

# Make a z-score variable for comment_score averages 
df['comment_deviation'] = (df['comment_score'] - pd.DataFrame.mean(df['comment_score']))/pd.DataFrame.var(df['comment_score'])
# Useless

df['Publisher_mean_score'] = df['comment_score'].groupby(df['Publisher']).transform('mean')
df['Publisher_mean_W1sales'] = df['W1Sales'].groupby(df['Publisher']).transform('mean')
df['Publisher_mean_sub']= df['comment_sub'].groupby(df['Publisher']).transform('mean')
df['Publisher_mean_pol']= df['comment_pol'].groupby(df['Publisher']).transform('mean')


df['Publisher_var_score'] = df['comment_score'].groupby(df['Publisher']).transform('var')
df['Publisher_var_W1sales'] = df['W1Sales'].groupby(df['Publisher']).transform('var')
df['Publisher_var_sub']= df['comment_sub'].groupby(df['Publisher']).transform('var')
df['Publisher_var_pol']= df['comment_pol'].groupby(df['Publisher']).transform('var')

df['Game_mean_score'] = df['comment_score'].groupby(df['Game_Name']).transform('mean')
df['Game_var_score'] = df['comment_score'].groupby(df['Game_Name']).transform('var')



df["Pub_Cat"] = pd.Categorical(df["Publisher"]).codes
df["Game_Cat"] = pd.Categorical(df["Game_Name"]).codes

#df['title_weight_pol'] = df['title_pol']*df['post_score']
# save the pandas data frame as a file?
df['title_weight_sub'] = df['title_sub']*np.log(df['post_score'] + 1)


# Save this as a file for Johannes:
df.to_csv("pandas_vars.txt", sep = ";", header = True, index = False, encoding =  'ISO-8859-1')


##### Down from here:
df = pd.read_csv('pandas_vars.txt', sep = ";", header = 0,  encoding = 'ISO-8859-1', low_memory = False)



# Make dummy vars for being in top 30 that week:
df['W1onChart'] = np.where(df['W1Sales']>= 1, 1, 0)
df['W2onChart'] = np.where(df['W2Sales']>= 1, 1, 0)
df['W3onChart'] = np.where(df['W3Sales']>= 1, 1, 0)
df['W4onChart'] = np.where(df['W4Sales']>= 1, 1, 0)



import sklearn
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
X = df[['W3onChart', 'comment_score', 'comment_pol', 'title_pol', 'num_comments', 'W1onChart']]
y = df['W4onChart']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=42)
# Make train and test sets?


logistic = sklearn.linear_model.LogisticRegression(penalty = "l2", C = 1.0)
logistic.fit(X_train, y_train)
print(logistic.coef_)
pred = logistic.predict(X_test)
pd.crosstab(y_test, pred, 
	rownames=['Actual status'], colnames=['Predicted status'])
thing = sklearn.metrics.f1_score(y_test,pred, average = "weighted")
thing



# 83% 


# Doing simple regressions on the data...
import statsmodels.formula.api as sm
result = sm.ols(formula ="W1Sales ~ comment_pol + title_pol + comment_score + num_comments + upvote_prop", data = df, ).fit()
print(result.summary())

from statsmodels.regression.linear_model import WLS
comment_weight_pol + post_score + title_sub + comment_sub + title_pol +

# that seems to explain a LOT, actually!
# 0.31 Adj-R fit.
result2 = smf.wls(formula ="Sales ~   Publisher_mean_sub + Publisher_mean_score", data = df).fit()
print(result2.summary())



# Use Weighted least squares to do a thing

result2 = smf.wls(formula ="W1Sales ~ Publisher_mean_pol + Publisher_mean_score + Publisher_var_sub + num_comments + upvote_prop", data = df2).fit()
print(result2.summary())



X = df.groupby(["Publisher"]).mean()
X.columns[1] = ["Publisher", "comment_score", ]

X['Publisher'] = X.index
result3 = smf.wls(formula = "Sales ~ score_size + comment_pol", data = X).fit()
print(result3.summary())

testimonial = TextBlob("Textblob is amazingly simple to use. What great fun!")

testimonial.sentiment
testimonial.sentiment.polarity
testimonial.sentiment.subjectivity

testimonial.sentence

# make a column each for sentiment polarity and subjectivity
# Do a simple for loop
# Across the for loop, record the sentiment of each statement ()



# Make a new for loop?
# Topic modeling of a post/Topic modeling of a Game? (pre-post release?)
#