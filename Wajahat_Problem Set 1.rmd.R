---
title: "Canadians Over the Age of 50 May Be Less Likely to Have a Favourable Opinion of Canada's Federal Green Party"
author: "Wajahat Naqvi"
date: "September 27, 2020"
abstract: "I analyzed and compared two distributions and their summary statistics to find out which age group of Canadian voters is less likely to hold a favourable opinion of the Federal Green Party. My analysis says that Canadians over the age of 50 are less likely to give the party a high rating. This tells the party that it should consider giving special incentives to Canadians over 50 to secure their votes."
output: html_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
# install.packages("tidyverse")
library(tidyverse)
library(visdat)
library(cesR)
library(skimr)
```

# Introduction:

Research Question: Which age group of Canadian voters could be less likely to hold a favourable opinion of Canada's Federal Green Party?

Political parties should know which categories of voters are likely to hold a favourable opinion of them and which categories of voters are not likely to. This is important because political parties should know which categories of voters they should provide special incentives to in order to secure their votes.
I am particularly interested in finding out which categories of voters are less likely to hold a favourable opinion of the Federal Green Party in Canada. The party's mission is to build a more prosperous and sustainable future for all Canadians. This mission will appeal to certain categories of Canadian voters but will not appeal easily to other categories. The purpose of this analysis is to identify the age group to which voters who are unlikely to hold a favourable opinion of the Federal Green Party belong. 
In order to identify that age group, I created a new dataset. This dataset is a subset of the original dataset (that is, the dataset that corresponds to the survey code: “ces2019_phone”). This new dataset has two columns. The first column contains the ages of the respondents of a survey conducted in 2019 (the survey that has the survey code “ces2019_phone”) and the second column contains the respondents’ answers to the question “How do you feel about the FEDERAL Green Party?”. I used this dataset to create two histograms. The first histogram shows the distribution of the ages of respondents who had an unfavourable opinion of the Federal Green Party. The second histogram shows the distribution of the ages of respondents who had a favourable opinion of the Federal Green Party (that is, gave it a rating above 55, or, in other words, a high rating). 
The datasets I have chosen to analyze allow me to make insightful histograms that help me answer my research question.
To identify the respondents’ age groups that were less likely to hold a favourable opinion of the Federal Green Party, I analysed the shape of each distribution and compared the relevant key figures, for instance, I compared the median age of the respondents who said they did not hold a favourable opinion of the Federal Green Party to the median age of the respondents who said thay they hold a favourable opinion of the Federal Green Party. 
My analysis and comparisons revealed that it is possible that Canadian voters over 50 years of age are less likely to hold a favourable opinion of the Federal Green Party.
This is an important finding. Through this finding, the Federal Green Party knows that there is a chance that people over the age of 50 are less likely to hold a favourable opinion of it and that it should focus on providing special incentives to them in order to secure their votes.

```{r download, warning=FALSE, message=FALSE}
# install.packages("devtools")
# devtools::install_github("hodgettsp/cesR")

cesR::get_ces("ces2019_phone")
```
# About the dataset:

The dataset I analyze is survey data that was part of the 2019 Canadian Election Study. The Canadian Election Study is a large-scale survey of citizens conducted each election year. The dataset corresponds to the survey code "ces2019_phone" and has 4,021 rows and 278 columns. Every row corresponds to a unique respondent and has information on a variety of matters. Every row also has information on a respondent’s opinions of the various political parties in Canada. My research question is relevant given this dataset because to answer it, I have to analyze a couple of distributions obtained through the dataset.
The two columns in the original dataset of interest to me are “q18” and “age”. The former has information on every respondent's opinion of the Federal Green Party (that is, every respondent's answer to the question “How do you feel about the Federal Green Party?”). The opinion was recorded in the form of a number from 0 to 100. The latter has information on every respondent's age. 
About the column titled “age”:
Minimum value: 18 years (The minimum value in a set of values is the lowest value in the set of values)
1st quartile: 38 years (The first quartile of a set of values is the value below which 25% of the values in the set of values lie)
Median: 51 years (The median of a set of values is the middle value in the set of values)
Mean: 50.89 years (The mean of a set of values is the average of all the values in the set)
3rd quartile: 64 years (The third quartile of a set of values is value above which 25% of the values in the set of values lie)
Maximum value: 100 years (The maximum value in a set of values is the highest value in the set of values)
About the column titled “q18”:
Minimum value: -9 (if a respondent said that they did not know how they felt about the Federal Green Party, -9 was recorded as their opinion of the federal party)
1st quartile: 10 
Median: 45
Mean: 40.21
3rd quartile: 60 
Maximum value: 100
The response of every respondent who stated their opinion of the Federal Green Party was recorded in the form of a number between 0 and 100. The higher the number, the higher the opinion and the better they felt about the Federal Green Party. The opinion can be thought of as a rating. In this analysis, a rating above 55 is considered to be a high rating and a rating below 55 is considered to be a low rating. A high rating is assumed to convey that the respondent is likely to have a favourable opinion of the Federal Green Party and a low rating is assumed to convey that the respondent is likely to not have a favourable opinion of the Federal Green Party.

```{r}
head(ces2019_phone)
```

```{r}
skimr::skim(ces2019_phone)

class(ces2019_phone$q18)

ces2019_phone <- 
  ces2019_phone %>% 
  select(age, q18)

ces2019_phone <-
  ces2019_phone %>%
  filter(q18>-5)
```

# Important Information To Note Before Reading the Analysis Below:
  The histograms below are based on dataset A. Dataset A consists of a subset of the data in the dataset ces2019_phone (survey code: "ces2019_phone"). It has two columns and 574 rows. The two columns in this dataset are "q18" and "age". Column "q18" contains respondents' answers to question “How do you feel about the FEDERAL Green Party?”. Every opinion was recorded in the form of a number between 0 and 100. Presumably, the higher the number a respondent gives, the more they like the Federal Green Party. Hence, every number can be thought as a rating. Only stated opinions of the Federal Green Party are part of Dataset A. Column "age" tells us the respondents' ages when the survey (survey code: "ces2019_phone") was taken. Each row in dataset A corresponds to a particular respondent of the survey (survey code: "ces2019_phone"). Dataset A does not contain the opinions of respondents aged over 34 years when the survey (survey code: "ces2019_phone") was taken but it does have data of respondents who were aged below 34 years when the survey (survey code: "ces2019_phone") was taken.

```{r}
cesR::get_ces("ces2019_phone")

ces2019_phone <-
  ces2019_phone %>%
  filter(q18<55)

summary(ces2019_phone$age)

years = ces2019_phone$age

sd(years)

ces2019_phone %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 10) +
  labs(x = "Ages of Respondents",
       y = "Number of Respondents Who Gave a Low Rating ",
       title = "Histogram 1",
       caption = "Stephenson, Laura B; Harell, Allison; Rubenson, Daniel; 
       Loewen, Peter John, 2020, '2019 Canadian Election Study - Phone Survey'")
theme_bw()
```
# Discussion of Histogram 1 and My Findings:

Histogram 1 shows us the distribution of the ages of the respondents who gave a low rating to the Federal Green Party (a low rating is a rating below 55). A histogram is a graph that shows us the distribution of a variable and the frequency of various small groups of values in the data.
  The distribution is roughly bell-shaped. The mean age of respondents who gave the Federal Green Party is roughly 52 years. The median age of these respondents is 53 years. The distribution's IQR (interquartile range) is 47 years. The interquartile range of a set of values is the difference between the third quartile and the first quartile of the set of values. A higher IQR, all else equal, indicates more variation in the values. The standard deviation of the ages of the respondents who gave the Federal Green Party a rating below 55 was 16.31 years. The standard deviation of a set of values is a measure of the variation among the values.
Since the distribution is roughly bell-shaped, the empirical rule can be applied to it to draw further insights related to the respondents’ opinions of the Federal Green Party. The empirical rule tells us how the values in a normal distribution are distributed. For instance, it tells us that about 68% of the values fall within 1 standard deviation of the mean, about 95% of the values fall within 2 standard deviations of the mean and about 99.7% of the values fall within 3 standard deviations of the mean. In this case, the empirical rule is telling us that approximately 68% of the respondents who gave a rating below 55 to the Federal Green Party were between approximately 36 and 68 years of age. That is, middle-aged respondents were reasonably likely to have given the Federal Green Party a rating below 55. The empirical rule is also telling us that approximately 2.5% of the respondents who gave a rating below 55 to the Federal Green Party were either 18 or 19 years of age. This conveys that respondents in the sample who were either 18 or 19 years of age were highly unlikely to give a rating below 55 to the Federal Green Party. This tells me that younger respondents are probably likely to have a more favourable view of the Federal Green Party. The shape of this distribution also conveys that respondents below about 50 years of age were not more likely to give a low rating to the Federal Green Party. 

```{r}
cesR::get_ces("ces2019_phone")

ces2019_phone <-
  ces2019_phone %>%
  filter(q18>55)

summary(ces2019_phone$age)

ces2019_phone %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 11) +
  labs(x = "Ages of Respondents",
       y = "Number of Respondents Who Gave a High Rating ",
       title = "Histogram 2",
       caption = "Stephenson, Laura B; Harell, Allison; Rubenson, Daniel;
       Loewen, Peter John, 2020, '2019 Canadian Election Study - Phone Survey'")
theme_bw()
```
# Discussion of Histogram 2 and My Findings:

Histogram 2 shows us the distribution of the ages of the respondents who gave a high rating to the Federal Green Party (a high rating is a rating above 55).

The distribution is a unimodal distribution because it has one clearly defined mode, which is about 39 years of age, as indicated by the one main “hump” in the distribution. A unimodal distribution is a distribution that has one main "hump" and the mode of a set of values is the value that occurs most frequently in the set of values.
The distribution’s right tail is slightly longer than its left tail, indicating that the distribution is slightly positively skewed (but certainly not very positively skewed). The tails of a distribution are the parts that typically trail off on either side of the distribution. If a distribution's right tail is longer than its left tail, it is said to be positively skewed. The slight positive skew is indicating that respondents between about 18 and 44 years of age were slightly more likely to be among the respondents who gave a high rating to the Federal Green Party. 
  One can see why the distribution would be slightly positively skewed. The Federal Green Party of Canada is working to build a more prosperous and sustainable future for all Canadians. Not surprisingly, Canadian respondents between 18 and 44 years of age would perhaps be more likely to have an interest in mitigating global warming and preventing climate change because they are going to live for more years in the future than people over the age of 50 years. People older than 50 years of age are not likely to be as interested in mitigating global warming and preventing climate change. Respondents between 18 and 44 years of age are likely to think of the issue of building a more sustainable future for all Canadians as being an important issue and hence, are more likely to have a more favourable view of the Federal Green party. 
  Not surprisingly, the mean age of the respondents who gave a high rating to the Federal Green Party is about 48 years, which is 4 years lesser than the mean age of the respondents who gave a low rating to the party. In addition, the median age of the respondents who gave a high rating to the Federal Green Party is 47 years, which is 6 years lesser than the median age of the respondents who gave a low rating to the Federal Green Party.

# Further Analaysis and Discussion On the Findings:

  Since the median age among respondents who did not have a favourable opinion of the Federal Green Party (that is, gave a rating below 55) was 53 years and the median age among respondents who gave a favourable opinion of the Federal Green Party (that is, gave a rating above 55) was 47 years, we can see that respondents over the age of 50 were slightly more likely to give a low rating to the Federal Green Party. Furthermore, the distribution in histogram 1 is bell-shaped, while the distribution in histogram 2 is sligthly positively skewed. This provides evidence that respondents below 50 years of age were more likely to have a favourable opinion of the Federal Green Party than respondents above 50 years of age.
    The results of my analysis can not be considered true without hypothesis testing or other statistical methods, but they do convey that it is possible that Canadians over the age of 50 are less likely to have a favourable opinion of the Federal Green party and vote for it in the next federal election in Canada.

# Shortcomings In My Analysis and Next Steps:

  1,351 respondents who took part in the survey (the survey with the survey code: “ces2019_phone”) gave the Federal Green Party a high rating whereas 2,621 respondents who took part in that survey (the survey with the survey code: “ces2019_phone”) gave the Federal Green Party a low rating. Histogram 2 was therefore based on data collected from a smaller sample, which may explain why the distribution it showed was slightly positively skewed. Hence, comparing the distributions in histograms 1 and 2 and their summary statistics could lead to misleading conclusions about Canadian voters' opinions of the Federal Green Party.
I did not try to estimate or test any hypotheses regarding the mean age of Canadians who are likely to give a low rating to the Federal Green Party or the proportion of Canadian voters over the age of 50 who are likely to give a low rating to the Federal Green Party. All the conclusions of my analysis are applicable to the respondents of this survey only and not to all Canadian voters. Besides, the sample from which the data was taken may not have been representative of all Canadian voters for a variety of reasons. My analyses are only meant to identify certain possibilities but not confirm their existence. Further research on this interesting topic should try to estimate the mean age of Canadian voters who have a favourable opinion of the Federal Green Party so that the Federal Green Party can broadly figure out which age groups are less likely to have a favourable opinion of it.

# References

- The code and the other text in the R Markdown File were written in R Studio Cloud. 

- Citation for the survey file used in this analysis:
  
  - Stephenson, Laura B; Harell, Allison; Rubenson, Daniel; Loewen, Peter John, 2020, '2019 Canadian Election Study - Phone Survey', https://doi.org/10.7910/DVN/8RHLG1, Harvard Dataverse, V1, UNF:6:eyR28qaoYlHj9qwPWZmmVQ== [fileUNF]

- Stephenson, Laura, Allison Harrel, Daniel Rubenson and Peter Loewen. Forthcoming. 'Measuring Preferences and Behaviour in the 2019 Canadian Election Study,' Canadian Journal of Political Science.

LINK: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/8RHLG1

- Citation for R:
  
  R Core Team (2015). R: A language and environment for
##   statistical computing. R Foundation for Statistical Computing,
##   Vienna, Austria. URL https://www.R-project.org/.

@Manual{,
  title        = {R: A Language and Environment for Statistical
    Computing},
  author       = {{R Core Team}},
  organization = {R Foundation for Statistical Computing},
  address      = {Vienna, Austria},
  year         = 2020,
  url          = {https://www.R-project.org}
}

- Citation for R 4.0.2:
  
  ##   R Core Team (2020). R: A language and environment for statistical
  ##   computing. R Foundation for Statistical Computing, Vienna, Austria.
  ##   URL https://www.R-project.org/.
  ## 
  ## A BibTeX entry for LaTeX users is
  ## 
  ##   @Manual{,
  ##     title = {R: A Language and Environment for Statistical Computing},
  ##     author = {{R Core Team}},
  ##     organization = {R Foundation for Statistical Computing},
##     address = {Vienna, Austria},
##     year = {2020},
##     url = {https://www.R-project.org/},
##   }
#

- Citation for the 'rmarkdown' package:
  
  Allaire J, Xie Y, McPherson J, Luraschi J, Ushey K, Atkins A, Wickham H, Cheng J, Chang W, Iannone R (2020). rmarkdown: Dynamic Documents for R. R package version 2.3, https://github.com/rstudio/rmarkdown.

Xie Y, Allaire J, Grolemund G (2018). R Markdown: The Definitive Guide. Chapman and Hall/CRC, Boca Raton, Florida. ISBN 9781138359338, https://bookdown.org/yihui/rmarkdown.

Corresponding BibTeX entries:
  
  @Manual{,
    title = {rmarkdown: Dynamic Documents for R},
    author = {JJ Allaire and Yihui Xie and Jonathan McPherson and
      Javier Luraschi and Kevin Ushey and Aron Atkins and Hadley
      Wickham and Joe Cheng and Winston Chang and Richard Iannone},
    year = {2020},
    note = {R package version 2.3},
    url = {https://github.com/rstudio/rmarkdown},
  }
@Book{,
  title = {R Markdown: The Definitive Guide},
  author = {Yihui Xie and J.J. Allaire and Garrett Grolemund},
  publisher = {Chapman and Hall/CRC},
  address = {Boca Raton, Florida},
  year = {2018},
  note = {ISBN 9781138359338},
  url = {https://bookdown.org/yihui/rmarkdown},
}

- Citation for the dataset on which histograms 1 and 2 are based:
  
  Stephenson, Laura B; Harell, Allison; Rubenson, Daniel; Loewen, Peter John, 2020, “2019 Canadian Election Study - Phone Survey”, https://doi.org/10.7910/DVN/8RHLG1, Harvard Dataverse, V1, UNF:6:eyR28qaoYlHj9qwPWZmmVQ== fileUNF


- Paul A. Hodgetts and Rohan Alexander (2020). cesR: Access the CES Datasets a Little Easier.. R package
version 0.1.0.

@Manual{,
  title = {cesR: Access the CES Datasets a Little Easier.},
  author = {Paul A. Hodgetts and Rohan Alexander},
  year = {2020},
  note = {R package version 0.1.0},
}

Hadley Wickham, Jim Hester and Winston Chang (2020). devtools: Tools to Make Developing R Packages Easier.
https://devtools.r-lib.org/, https://github.com/r-lib/devtools.

@Manual{,
  title = {devtools: Tools to Make Developing R Packages Easier},
  author = {Hadley Wickham and Jim Hester and Winston Chang},
  year = {2020},
  note = {https://devtools.r-lib.org/, https://github.com/r-lib/devtools},
}

R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical
Computing, Vienna, Austria. URL https://www.R-project.org/.

@Manual{,
  title = {R: A Language and Environment for Statistical Computing},
  author = {{R Core Team}},
  organization = {R Foundation for Statistical Computing},
  address = {Vienna, Austria},
  year = {2020},
  url = {https://www.R-project.org/},
}

JJ Allaire, Jeffrey Horner, Yihui Xie, Vicent Marti and Natacha Porte (2019). markdown: Render Markdown
with the C Library 'Sundown'. R package version 1.1. https://github.com/rstudio/markdown

@Manual{,
  title = {markdown: Render Markdown with the C Library 'Sundown'},
  author = {JJ Allaire and Jeffrey Horner and Yihui Xie and Vicent Marti and Natacha Porte},
  year = {2019},
  note = {R package version 1.1},
  url = {https://github.com/rstudio/markdown},
}

-Citation for the package "devtools":
  
  Hadley Wickham, Jim Hester and Winston Chang (2020). devtools: Tools to Make Developing R Packages Easier. https://devtools.r-lib.org/,
https://github.com/r-lib/devtools.

A BibTeX entry for LaTeX users is

@Manual{,
  title = {devtools: Tools to Make Developing R Packages Easier},
  author = {Hadley Wickham and Jim Hester and Winston Chang},
  year = {2020},
  note = {https://devtools.r-lib.org/, https://github.com/r-lib/devtools},
}

-Citation for the package "knitr":
  
  Yihui Xie (2020). knitr: A General-Purpose Package for Dynamic Report Generation in R. R package version 1.30.

Yihui Xie (2015) Dynamic Documents with R and knitr. 2nd edition. Chapman and Hall/CRC. ISBN 978-1498716963

Yihui Xie (2014) knitr: A Comprehensive Tool for Reproducible Research in R. In Victoria Stodden, Friedrich Leisch and Roger D. Peng, editors,
Implementing Reproducible Computational Research. Chapman and Hall/CRC. ISBN 978-1466561595

-Citation for the package 'tidyverse':
  
  Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source
Software, 4(43), 1686, https://doi.org/10.21105/joss.01686

A BibTeX entry for LaTeX users is

@Article{,
  title = {Welcome to the {tidyverse}},
  author = {Hadley Wickham and Mara Averick and Jennifer Bryan and Winston Chang and Lucy D'Agostino McGowan and Romain François and Garrett Grolemund and Alex Hayes and Lionel Henry and Jim Hester and Max Kuhn and Thomas Lin Pedersen and Evan Miller and Stephan Milton Bache and Kirill Müller and Jeroen Ooms and David Robinson and Dana Paige Seidel and Vitalie Spinu and Kohske Takahashi and Davis Vaughan and Claus Wilke and Kara Woo and Hiroaki Yutani},
    year = {2019},
    journal = {Journal of Open Source Software},
    volume = {4},
    number = {43},
    pages = {1686},
    doi = {10.21105/joss.01686},
  }
  
- Citation for the package 'skimr':

  Elin Waring, Michael Quinn, Amelia McNamara, Eduardo Arino de la Rubia, Hao
  Zhu and Shannon Ellis (2020). skimr: Compact and Flexible Summaries of Data.
  https://docs.ropensci.org/skimr (website), https://github.com/ropensci/skimr.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {skimr: Compact and Flexible Summaries of Data},
    author = {Elin Waring and Michael Quinn and Amelia McNamara and Eduardo {Arino de la Rubia} and Hao Zhu and Shannon Ellis},
    year = {2020},
    note = {https://docs.ropensci.org/skimr (website),
https://github.com/ropensci/skimr},
  }

-Citation for the package 'visdat':

Tierney N (2017). “visdat: Visualising Whole Data Frames.” _JOSS_, *2*(16), 355.
doi: 10.21105/joss.00355 (URL: https://doi.org/10.21105/joss.00355), <URL:
http://dx.doi.org/10.21105/joss.00355>.

A BibTeX entry for LaTeX users is

  @Article{,
    title = {visdat: Visualising Whole Data Frames},
    author = {Nicholas Tierney},
    doi = {10.21105/joss.00355},
    url = {http://dx.doi.org/10.21105/joss.00355},
    year = {2017},
    publisher = {Journal of Open Source Software},
    volume = {2},
    number = {16},
    pages = {355},
    journal = {JOSS},
  }

-Citation for the package 'cesR':

To cite package ‘cesR’ in publications use:

  Paul A. Hodgetts and Rohan Alexander (2020). cesR: Access the CES Datasets a
  Little Easier.. R package version 0.1.0.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {cesR: Access the CES Datasets a Little Easier.},
    author = {Paul A. Hodgetts and Rohan Alexander},
    year = {2020},
    note = {R package version 0.1.0},
  }
-Citation for the literature and other websited used:
  
Sharpe, Norean R., De Veaux, Richard D., Velleman, Paul F., David Wright. Business Statistics. Pearson Canada Inc.: 2018.
  
Green Party of Canada. Accessed September 27, 2020. https://www.greenparty.ca/en.

Canadian Election Study. Accessed September 27, 2020. http://www.ces-eec.ca.

cesR, Access the Canadian Election Study Datasets a bit easier. https://hodgettsp.github.io/cesR/. 



