import pandas as pd
from snownlp import SnowNLP
data = pd.read_excel('ChatGPTData.xlsx')
def get_sentiment_cn(text):
    s = SnowNLP(text)
    return s.sentiments

data["sentiment"] =data.content.apply(get_sentiment_cn)