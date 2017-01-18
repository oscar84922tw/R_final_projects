source("~/Dropbox/R_finalproject/src/multuple_plot.R")
pckg = c("ggplot2", "corrplot", "data.table", "grid")
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[, 1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
for (p in pckg) {
  usePackage(p)
}

# set path
setwd("~/Dropbox/R_finalproject/data/")

# load raw data
raw_data = read.csv(
  "movie_metadata.csv",
  sep = ",",
  stringsAsFactors = T,
  header = T,
  na.strings = ""
)

# set path
setwd("~/Dropbox/R_finalproject/src/")

# remove some coloumns
drops = c(
  "color",
  "actor_3_facebook_likes",
  "actor_3_name",
  "facenumber_in_poster",
  "plot_keywords",
  "movie_imdb_link",
  "aspect_ratio"
)

raw_data = raw_data[,!(names(raw_data) %in% drops)]

# remove before 1990s movies
raw_data = raw_data[raw_data$title_year > 1990, ]

# remove all has null data column / type: list
data = raw_data[complete.cases(raw_data), ]

# remove outliers
remove_outliers = function(x, na.rm = TRUE, ...) {
  qnt = quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H = 1.5 * IQR(x, na.rm = na.rm)
  y = x
  y[x < (qnt[1] - H)] = NA
  y[x > (qnt[2] + H)] = NA
  y
}

# just use to find correlations
# change column type to numeric
dta_numeric = apply(data, 2, as.numeric)
# correlation matrix
cor_matrix = cor(dta_numeric[,colSums(is.na(dta_numeric))<nrow(dta_numeric)])


# after delete outlier data
after_outlier_data.imdb = remove_outliers(data$imdb_score)
after_outlier_data.imdb.quantile = quantile(after_outlier_data.imdb, na.rm = T)
after_outlier_data.gross = remove_outliers(data$gross)
after_outlier_data.gross.quantile = quantile(after_outlier_data.gross, na.rm = T)
# avg imdb_score for each director
director = unique(data$director_name, na.rm = TRUE)
director_avg_imdb = c()
for (i in 1:length(director)) {
  if (is.na(director[i])) {
    
  } else {
    tmp = subset(data, director_name == director[i], imdb_score)
    # print(sum(tmp) / nrow(tmp))
    director_avg_imdb[i] = sum(tmp) / nrow(tmp)
  }
  rm(i)
  rm(tmp)
}
names(director_avg_imdb) = director
for (i in names(director_avg_imdb)) {
  data[data$director_name == i, "director_avg_imdb"] = director_avg_imdb[i]
}

# splice data to four group:
# high gross and high imdb score
dataGroup.hh = data[data$gross > after_outlier_data.gross.quantile[3] &
                      data$imdb_score > after_outlier_data.imdb.quantile[3],]
# high gross and low imdb score
dataGroup.hl = data[data$gross > after_outlier_data.gross.quantile[3] &
                      data$imdb_score < after_outlier_data.imdb.quantile[3],]
# low gross and high imdb score
dataGroup.lh = data[data$gross < after_outlier_data.gross.quantile[3] &
                      data$imdb_score > after_outlier_data.imdb.quantile[3],]
# low gross and low imdb score
dataGroup.ll = data[data$gross < after_outlier_data.gross.quantile[3] &
                      data$imdb_score < after_outlier_data.imdb.quantile[3],]

data[data$movie_title %in% dataGroup.hh$movie_title, "level"] = 0
data[data$movie_title %in% dataGroup.hl$movie_title, "level"] = 1
data[data$movie_title %in% dataGroup.lh$movie_title, "level"] = 2
data[data$movie_title %in% dataGroup.ll$movie_title, "level"] = 3
data = data[!is.na(data$level), ]
attach(data)


p1 = ggplot(data, aes(x = as.factor(level), y = remove_outliers(director_facebook_likes))) + geom_boxplot()
p2 = ggplot(data, aes(x = as.factor(level), y = remove_outliers(duration))) + geom_boxplot()
p3 = ggplot(data, aes(x = as.factor(level), y = remove_outliers(num_voted_users))) + geom_boxplot()
p4 = ggplot(data, aes(x = as.factor(level), y = remove_outliers(num_user_for_reviews))) + geom_boxplot()
p5 = ggplot(data, aes(x = as.factor(level), y =remove_outliers(num_critic_for_reviews))) + geom_boxplot()
p6 = ggplot(data, aes(x = as.factor(level), y = remove_outliers(cast_total_facebook_likes))) + geom_boxplot()
p7 = ggplot(data, aes(x = as.factor(level), y = remove_outliers(director_avg_imdb))) + geom_boxplot()
p8 = ggplot(data, aes(x = as.factor(level), y = remove_outliers(title_year))) + geom_boxplot()
p9 = ggplot(data, aes(x = as.factor(level), y = remove_outliers(budget))) + geom_boxplot()
# ggplot(data, aes(x = as.factor(level), y = actor_1_facebook_likes)) + geom_boxplot()
# ggplot(data, aes(x = as.factor(level), y = actor_2_facebook_likes)) + geom_boxplot()
multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,cols = 3)

# ****************** not uses ***********************
# get all groups quantiles
dataGroup.hh.quantile = as.data.frame(sapply(dataGroup.hh, function(x) {
  quantile(as.numeric(x), na.rm = T)
}))

dataGroup.hl.quantile = as.data.frame(sapply(dataGroup.hl, function(x) {
  quantile(as.numeric(x), na.rm = T)
}))

dataGroup.lh.quantile = as.data.frame(sapply(dataGroup.lh, function(x) {
  quantile(as.numeric(x), na.rm = T)
}))

dataGroup.ll.quantile = as.data.frame(sapply(dataGroup.ll, function(x) {
  quantile(as.numeric(x), na.rm = T)
}))
# *****************************************************