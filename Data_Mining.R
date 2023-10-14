# import data

achivement = read.csv('achievements.csv', na.strings = c('', 'N/A'), fileEncoding = 'UTF-8-BOM')
achivement

achivement_category = read.csv('achievements-category.csv', na.strings = c('', 'N/A'), fileEncoding = 'UTF-8-BOM')
achivement_category

# merge 2 dataset
merged_data = merge(achivement, achivement_category, by = 'Unique.Entry.ID')
merged_data

# Removing N/A values
merged_data = na.omit(merged_data)
merged_data

# Data Visualization

# Showing top 5 Categories
category = table(merged_data$Category)
category

top5 = sort(category, decreasing = TRUE)
top5 = head(top5,5)
top5

top5_label = paste(names(top5),'\n', top5)
top5_label

pie(top5,labels = top5_label, main = 'Top 5 Achivements Categories', col = rainbow(5))

# Showing Frequency of Achivement based on Sequential

sequential_achivement = table(merged_data$Sequential)
sequential_achivement

barplot(sequential_achivement, main = 'Frequency of Sequential Achivement', col = rainbow(2), ylab = 'Frequency')

# Show total number of achivements based on reward tier 1 that sepereated into 3 categories Large, Normal ,Small

tier1_reward = merged_data$Reward.Tier.1
tier1_reward

large = sum(tier1_reward > 820)
normal = sum(tier1_reward > 480 & tier1_reward <= 820)
small = sum(tier1_reward <= 480)

categorical_reward = c(large, normal,small)
categorical_reward

barplot(categorical_reward, main = 'Total Number of Achivement based on Reward Tier 1', col = rainbow(3), ylab = 'Frequency')

# 2 Data Pre-Processing, Based on Conditional

newdata = merged_data[
    merged_data$Category != 'Money' &
    merged_data$Category != 'Communication' &
    merged_data$Category != 'LandMaking' &
    merged_data$Sequential != 'No' &
    merged_data$Num.of.Tiers != 6,
]

newdata

# Data Transformation, Change data so it can suitable apriori analysis

split_data = split(newdata$Category, newdata$Num.of.Tiers)
split_data

# Data Mining with Apriori with minnmum support 0.6

install.packages('arules')
library(arules)

rules = apriori(split_data, parameter = list(support = 0.6, target = 'Frequent itemsets'))

inspect(rules)

# Association rules with minnum confidence 0.8

assoc_rules = ruleInduction(rules, confidence = 0.8)
inspect(assoc_rules)