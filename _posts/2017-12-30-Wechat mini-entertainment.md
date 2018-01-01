---
title: "Wechat mini-project for fun"
author: "REX(RUIZHE) ZHOU"
categories: analysis
date: "30/12/2018"
---

**Introduction**

Learned a little bit of `itchat` module built under python, and tried to play around with it to crawl the information of my
wechat contacts' information

**Requirement**

- `Python 3.6`
- module `itchat`
- module `matplotlib.pyplot`
- module `numpy`
- module `jieba`
- module `re`

**Output**

[<img src="/assets/wordcloud.png" class="fit image">]({{ "/assets/wordcloud.png" | "https://github.com/trexwithoutt/trexwithoutt.github.io/blob/master" }})


**Programming**

1. conduct interface with wechat account

```python
import itchat

itchat.login()

friends = itchat.get_friends(update=True)[0: ]
```

<img src="/sec1.png" class="fit image">


2. extract male/female friends information

```python
# Initialization
male = female = other = 0

for i in friends[1:]:
    sex = i['Sex']
    if sex == 1:
        male += 1
    elif sex == 2:
        female += 1
    else:
        other += 1

# overall count of friends
total = len(friends[1:])

# print result
print("Male: %.2f%%" % (float(male) / total * 100) + '\n' +
     "Female: %.2f%%" % (float(female) / total * 100) + '\n' +
     "Transgender? : %.2f%%"% (float(other) / total * 100)) 

################
##  Plotting  ##
################

sex_ratio = [float(male) / total * 100, float(female) / total * 100, float(other) / total * 100]

import matplotlib.pyplot as plt; plt.rcdefaults()
import numpy as np

objects = ('Male', 'Female', 'Other')
y_pos = np.arange(len(objects))
ratio = sex_ratio
 
plt.bar(y_pos, ratio, align='center', alpha=0.5)
plt.xticks(y_pos, objects)
plt.ylabel('Ratio')
plt.title('Wechat Sex Ratio Visualization')
plt.show()

```
<img src="/ratio.png" class="fit image">


[<img src="/assets/barplot.png" class="fit image">]({{ "/assets/barplot.png" | "https://github.com/trexwithoutt/trexwithoutt.github.io/blob/master" }})



3. friends' signature mining

```python
import re

siglist = []

for i in friends:
    signature = i['Signature'].strip().replace('span', '').replace('class', '').replace('emoji', '')
    rep = re.compile("1f\d+\w*|[<>/=]")
    signature = rep.sub('', signature)
    siglist.append(signature)
text = ''.join(siglist)


import jieba

wordlist = jieba.cut(text, cut_all = True)
word_space_split = ' '.join(wordlist)

# Plot
from wordcloud import WordCloud, ImageColorGenerator
import PIL.Image as Image
%matplotlib inline

coloring = np.array(Image.open("wechat.jpg"))

my_wordcloud = WordCloud(background_color="white", max_words=2000,
                         mask=coloring, max_font_size=60, random_state=42, scale=3,
                         font_path="coolfont.ttf").generate(word_space_split)

image_colors = ImageColorGenerator(coloring)
plt.imshow(my_wordcloud.recolor(color_func=image_colors))
plt.imshow(my_wordcloud)
plt.axis("off")
plt.show()
```
<img src="/sec2.png" class="fit image">

[<img src="/assets/wordcloud.png" class="fit image">]({{ "/assets/wordcloud.png" | "https://github.com/trexwithoutt/trexwithoutt.github.io/blob/master" }})

<img src="/logout.png" class="fit image">
