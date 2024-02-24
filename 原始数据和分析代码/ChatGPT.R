library(openxlsx)
library(tidyverse)

RowData <- list.files(pattern = "ChatGPT.*\\.csv$")|>
  map(read_csv)|>
  bind_rows()

PreData <- RowData|>
  select(bid,time=发布时间,content=微博正文)|>
  separate(time,sep = " ",into = c("month","hour"))|>
  separate(month,sep = "-",into = c("year","month","day"))|>
  separate(hour,sep = ":",into = c("hour","minute","second"))|>
  mutate(month=case_when(year%in%"2024"~"13",
                         T~month))|>
  select(bid,month,hour,content)

ChatData <- PreData|>
  group_by(month,content)|>
  distinct()|>
  ungroup()|>
  mutate(content=toupper(content))

SampleData <- ChatData[sample(nrow(ChatData),size=round(nrow(ChatData)*0.01)),]

####发文量可视化####

library(patchwork)

DataSum <- ChatData|>
  mutate(hour=case_when(
    hour%in%c("01","02","03","04")~"01:00~04:00",
    hour%in%c("05","06","07","08")~"05:00~08:00",
    hour%in%c("09","10","11","12")~"09:00~12:00",
    hour%in%c("13","14","15","16")~"13:00~16:00",
    hour%in%c("17","18","19","20")~"17:00~20:00",
    T~"21:00~24:00"),
    hour=factor(hour,levels=rev(c("01:00~04:00","05:00~08:00","09:00~12:00","13:00~16:00","17:00~20:00","21:00~24:00"))))|>
  group_by(month,hour)|>
  reframe(n=n())

#Res2
r1_1 <- ggplot(DataSum,aes(x=month, y=hour,
                           fill=n))+
  geom_raster()+
  # scale_color_manual(values = c("#e69f67","#8dab93","#e69f67"))+
  scale_fill_gradientn(colours =colorRampPalette(c("#faf8f0","#f2dbbc","#b6b991","#44958c","#35818f"))(100))+
  # coord_polar(start = 0)+
  # scale_size_continuous(limits =c(0,7980))+
  theme_minimal()+
  theme(legend.position='bottom')
# coord_polar(start = 0)
r1_2 <- DataSum|>
  group_by(month)|>
  reframe(n=sum(n))|>
  ggplot(aes(x=month, y=n))+
  geom_area(aes(group=1),fill="#35818f",alpha=0.2)+
  geom_point(size=2,color="#35818f")+
  geom_line(aes(group=1),color="#35818f",
            position = "identity",linewidth=1)+
  geom_text(aes(label=n), vjust = -5)+
  # scale_color_gradientn(colours =colorRampPalette(c("#f1dfad","#faf8f0","#74959a"))(100))+
  # scale_color_manual(values = c("#e69f67","#8dab93","#e69f67"))+
  theme_classic()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  labs(x=NULL)
  
r1_3 <- DataSum|>
  group_by(hour)|>
  reframe(n=sum(n))|>
  ggplot(aes(x=hour, y=n))+
  geom_area(aes(group=1),fill="#f2dbbc",alpha=0.2)+
  geom_point(size=2,color="#f2dbbc")+
  geom_line(aes(group=1),color="#f2dbbc",
            position = "identity",linewidth=1)+
  coord_flip()+
  theme_classic()+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  labs(x=NULL)


r1_2+ plot_spacer()+r1_1+r1_3+ plot_layout(widths = c(0.618*10, 1), heights = c(1,0.618*2))
#5*8




####分词####
library(jiebaR)

mixseg <- worker(write = "NOFILE")#https://github.com/qinwf/jiebaR/issues/51
dic <- c("好奔溃","英伟达","元宇宙","i冰宇宙","大语言模型","概念股","永远的神","yyds","怎么说呢","想死","想鼠","别死","笑死","笑死了","笑死我了","笑鼠","嘤嘤嘤","我超","维尔汀")# 专业词词库
new_user_word(mixseg, dic)
stopword <- c("我","的","如果","同时","这样","怎么","那么","真的","之后","正在","非常","不是","小苏","不会","所以","如何","因为","不少","其中","什么","他们","一些","这种","作为","以及","已经","这个","没有","自己","2022","可能","2024","一下","一直","对于","很多","还有","可以","人工智能","AI","还是","但是","这些","20","CHATGPT","GPT","亿元","2023","了","一个","我们","　","就是","10","你","好","是","不","啊","在","就","都","说","给","想","吧","有","也","很","看","要","还","他","和","吃","这","吗","那","她","去","没","被","把","又","会","到","对","人","跟","一","呜","1","找","妈","写","快","搞","做","上","来","大","等","呢","捏","让","呃","挺","里","再","小","多","着","用","发","过","完","能","睡","图","https","干","个","请","才","打","真","拿","得","穿","帮","叫","像","话","先","点","哦","太","男","下","当","改","号","从","只","子","逼","最","忘","但","听","com","新","骂","罢","钱","带","事","p","中","别","问","谁","拍","刷","更","嘛","包","www","服","送","嘞","揍","字","7","女","web","放","为","vd","比","辣","活","它","h","翻","cn","咋","与")
stopword2 <- c("我","的","如果","同时","这样","怎么","那么","真的","之后","正在","非常","不是","小苏","不会","所以","如何","因为","不少","其中","什么","他们","一些","这种","作为","以及","已经","这个","没有","自己","2022","可能","2024","一下","一直","对于","很多","还有","可以","还是","但是","这些","20","GPT","亿元","2023","了","一个","我们","　","就是","10","你","好","是","不","啊","在","就","都","说","给","想","吧","有","也","很","看","要","还","他","和","吃","这","吗","那","她","去","没","被","把","又","会","到","对","人","跟","一","呜","1","找","妈","写","快","搞","做","上","来","大","等","呢","捏","让","呃","挺","里","再","小","多","着","用","发","过","完","能","睡","图","https","干","个","请","才","打","真","拿","得","穿","帮","叫","像","话","先","点","哦","太","男","下","当","改","号","从","只","子","逼","最","忘","但","听","com","新","骂","罢","钱","带","事","p","中","别","问","谁","拍","刷","更","嘛","包","www","服","送","嘞","揍","字","7","女","web","放","为","vd","比","辣","活","它","h","翻","cn","咋","与")

##全部分词（全）##
SegmentAllAll <- segment(ChatData$content, mixseg)|>
  filter_segment(stopword2)|>
  freq()|>
  filter(nchar(char)>1)|>
  mutate(char=case_when(char%in%"OPENAI"~"OpenAI",char%in%"CHATGPT"~"ChatGPT",T~char))|>
  arrange(desc(freq))|>
  mutate(char=case_when(char%in%head(char,10)~char,T~"其他"))


##全部分词##
SegmentAll <- segment(ChatData$content, mixseg)|>
  filter_segment(stopword)|>
  freq()|>
  filter(nchar(char)>1)|>
  mutate(char=case_when(char%in%"OPENAI"~"OpenAI",T~char))|>
  arrange(desc(freq))

##月份分词##
SegmentMonth <- ChatData |>
  mutate(seg=map(content,~filter_segment(segment(.,mixseg),stopword)))|>
  group_by(month)|>
  summarise(freq=freq(unlist(seg)))|>
  filter(nchar(freq$char)>1)|>
  arrange(month,desc(freq$freq))

SegmentMonth <- tibble("month"=SegmentMonth$month,
                       "char"=unlist(SegmentMonth$freq[,1]),
                       "freq"=unlist(SegmentMonth$freq[,2]))

write.xlsx(SegmentMonth,"ChatGPTHotWordsAll.xlsx")
  
library(wordcloud2)
###词云可视化###
SegmentAll|>
  filter(char!="11")|>
  head(150)|>
  wordcloud2(fontFamily = 'MiSans VF',fontWeight = 330,color = rev(colorRampPalette(c("#faf8f0","#f2dbbc","#b6b991","#44958c","#35818f"))(150)))

library(treemap)
###前10可视化（全）###
SegmentAllElse <- SegmentAllAll|>group_by(char)|>reframe(n=sum(freq))|>filter(char!="其他")
treemap(SegmentAllElse,
                 index="char", #指定分组的列
                 vSize="n", #指定面积大小的列
                 vColor="n", #指定颜色深浅的列
                 type='value', #指定颜色填充数据的类型
                 format.legend = list(scientific = FALSE,
                                      big.mark = " "),
        palette=colorRampPalette(c("#faf8f0","#f2dbbc","#44958c","#35818f"))(10))

###时间变化可视化###

SegmentMonth2 <- SegmentMonth|>
  mutate(char=factor(char,levels=c("OPENAI","微软","华为","公司","板块","市场","董事会","用户","技术","科技","模型","数据","机器人")),
                       type=case_when(char%in%c("OPENAI","微软","华为")~"A",
                                      char%in%c("板块","市场","董事会","公司","用户")~"B",
                                      T~"C"))


SegmentMonthMax <- SegmentMonth2|>
  group_by(month)|>
  filter(char!="奥特曼")|>
  slice_head(n=5)|>
  ungroup()
#6*7
SegmentMonthMax|>
  ggplot(aes(x=char,y=month,size=freq))+
  geom_point(data=SegmentMonth2|>filter(char%in%unlist(select(SegmentMonthMax,char))),color="grey",alpha=0.2)+
  geom_point(aes(color=type),alpha=0.9)+
  guides(size=guide_legend(order = 1),
         fill=guide_legend(order = 2))+
  # scale_color_manual(values = c("#e66351","#fed072","#386793"))+
  theme_test()+
  theme(aspect.ratio=0.618/1.4)+
  scale_color_manual(values =c("#d7ceb0","#5b9c8d","#0b584f"))+
  theme(legend.position = 'right',
        legend.direction = 'vertical',
        legend.justification = c(0,0),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey"))+
  scale_y_discrete(limits=rev)

####供LDA用####
SegmentPreLDA <- ChatData |>
  mutate(seg=map(content,~filter_segment(segment(.,mixseg),stopword)))|>
  group_by(month,bid,content)|>
  reframe(
    char=freq(unlist(seg))[,1],
    freq=freq(unlist(seg))[,2]
    )|>
  filter(nchar(char)>1)

library(tidytext)
library(tm)
SegmentPreLDATFIDF <- SegmentPreLDA|>
  mutate(char=toupper(char))|>
  filter(!char%in%c("比如","马斯克","是否","然后","随着","只是","此外","12","11","14","30","13","15","100","奥特曼","而且","为什么","越来越","因此","50","NA"),
         !is.na(char))|>
  ungroup()|>
  bind_tf_idf(term = char,document = bid,n = freq)|>
  group_by(bid)|>
  top_n(10,tf_idf)
  
# |>arrange(month,desc(freq$freq))

SegmentLDAMatrix <- SegmentPreLDATFIDF|>
  cast_dtm(document = bid,
           term=char,
           value=freq)

library(topicmodels)
burnin = 1000
#迭代次数
iter = 1000
#保存记录的步长
keep = 20
#主题范围（从2到20，以步长2进行递增）
sequ <- seq(2, 20, 2)
#迭代进行试验
fitted_many <- lapply(sequ, function(k) LDA(SegmentLDAMatrix, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))
#抽取每个主题的对数似然估计值
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
#定义计算调和平均值的函数
harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}
#计算各个主题的调和平均数，将其最为模型的最大似然估计
#需加载程序包gmp、Rmpfr
library("gmp")
library("Rmpfr")
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
#画出主题数-似然估计曲线图，用于观察
plot(sequ, hm_many, type = "l")
# 计算最佳主题个数
sequ[which.max(hm_many)]



LDAmodel <- LDA(SegmentLDAMatrix, k = 4, control = list(seed = 1128))
LDAres <- tidy(LDAmodel)|>
  group_by(topic)|>
  top_n(5, beta) |>
  ungroup() |>
  arrange(topic, -beta)

LDAresMonth <- tidy(LDAmodel,matrix="gamma")|>
  group_by(document)|>
  top_n(n=1)|>
  ungroup()|>
  right_join(ChatData,by=c("document"="bid"))|>
  group_by(month,topic)|>
  reframe(n=n())|>
  drop_na()|>
  group_by(month)|>
  mutate(percent=n/sum(n))

LDAresMonthSample <- tidy(LDAmodel,matrix="gamma")|>
  group_by(document)|>
  top_n(n=1)|>
  ungroup()|>
  right_join(ChatData,by=c("document"="bid"))

ggplot(LDAresMonth, aes(x = month, y = percent, fill = as.character(topic))) +    
  geom_bar(position = "fill", stat = "identity",width = 0.618*1.25) +
  # geom_text(aes(label=pct), position = position_stack(vjust = 0.5),color="white") +
  theme_test() +
  labs(x="Crown functional traits",y="Proportion of total variance ")+
  theme(legend.position = "right",  # 图例位置
        legend.title = element_blank())+
  theme(aspect.ratio=1/(0.618*4))+
  scale_fill_manual(values=c("#f2dbbc","#44958c","#35818f","#0b584f"))

####情感分析可视化####

SentData <- read.xlsx("ChatGPTSentiment.xlsx")

SentDataPercent <- SentData|>
  mutate(sentiment=case_when(sentiment>0.5~"Pos",T~"Neg"))|>
  group_by(month,sentiment)|>
  summarise(n=n())|>
  mutate(percent=paste0(round(n/sum(n)*100,1),"%"))

p3_1 <- ggplot(SentDataPercent,aes(x = month, y =n) ) +
  geom_line(aes(group = month), color = "#E7E7E7", linewidth = 1) +
  geom_line(aes(group = sentiment, color = sentiment), linewidth = 1,alpha=0.5) +
  geom_point(aes(fill = sentiment),color="white",shape=21,size = 3.5,alpha=1) +
  geom_text(data=SentDataPercent|>filter(sentiment=="Pos"),aes(label=percent),vjust=-1,size=3)+
  scale_fill_manual(values=c("#f2dbbc", "#35818f")) +
  scale_color_manual(values=c("#f2dbbc", "#35818f")) +
  theme_classic()+
  theme(legend.position = "right",
        axis.title = element_blank()
  ) +
  theme(aspect.ratio=0.618/2)

p3_2 <- SentDataPercent|>
  group_by(sentiment)|>
  summarise(n=sum(n))|>
  mutate(percent=paste0(round(n/sum(n)*100,1),"%"))|>
  ggplot(aes(x=1,y=n,fill=sentiment))+
  geom_col(color="white",linewidth=0.8) +
  geom_text(aes(label=percent),position = position_stack(vjust = 0.5),size=4)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("#f2dbbc", "#35818f")) + 
  theme(
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
  )

SentDataSig <- SentData|>
  select(!content)|>
  group_by()


SentDataMeanSd <- SentDataSig|>group_by(month)|>
  reframe(ymin=quantile(sentiment,0.25),
          ymax=quantile(sentiment,0.75),
          mean=median(sentiment))
###成对秩和检验###
library(rstatix)
RankSum <- pairwise_wilcox_test (SentDataSig, sentiment ~ month, 
                      p.adjust.method = "bonf")
library(rcompanion)
RankSumABC <- RankSum|>
  mutate(group1=case_when(group1==10~"10A",T~group1),
         group2=case_when(group2==10~"10A",T~group2),
         group1=factor(group1,levels=c(6,7,5,4,1,2,3,8,9,12,13,"10A",11)))|>
  arrange(group1)|>
  unite("group",group1:group2,sep="-")

RankSumRes <- cldList(p.adj ~ group,data = RankSumABC,threshold = 0.05)|>
  mutate(Group=case_when(Group=="1A"~"10",T~Group),
         Group=as.numeric(Group))|>
  right_join(SentDataMeanSd,by=c("Group"="month"))


p3_3 <- ggplot(SentDataMeanSd,aes(x=month,fill=mean))+
  geom_point(aes(y=mean),shape=21,color="white",size=3, alpha=1) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax,color=mean), width=0,
                linewidth=1,
                position=position_dodge(0.05),
                alpha=0.8)+
  geom_hline(yintercept=0.5, linetype='dashed',color = "#E7E7E7", linewidth = 1)+
  geom_text(data=RankSumRes,aes(x=Group,y=mean,label=Letter),position = position_stack(vjust = 0.2))+
  scale_fill_gradientn(colours =colorRampPalette(c("#f2dbbc","#b6b991","#44958c","#35818f"))(100))+
  scale_color_gradientn(colours =colorRampPalette(c("#f2dbbc","#b6b991","#44958c","#35818f"))(100))+
  theme_classic()+
  # ylim(0,1.15)+
  theme(aspect.ratio=0.618/2)
p3_1/p3_3#A5
