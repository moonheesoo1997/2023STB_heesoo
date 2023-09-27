#survey의 Gender 1개의 인자를 가지고 도수분포표를 작성하세요.
table(survey $Gender)

#survey의 Gender 1개의 인자를 가지고 상대도수분포표를 작성하세요.
ECN <- table(survey $Gender)
prop.table(ECN)

#survey의 Gender와 Grade 2개의 인자를 가지고 교차표를 작성하세요.
table(survey $Gender, survey $Grade)

#survey의 Nationality 1개의 인자를 가지고 막대그래프를 작성하세요.
barplot(table(survey $Nationality))

#survey의 residential area 1개의 인자를 가지고 (가로) 막대그래프를 작성하세요. 
barplot(table(survey $`residential area`), horiz = TRUE)

#survey의 Gender와 Grade 2개의 인자를 가지고 막대그래프를 작성하세요.
entry <- table(survey $Gender, survey $Grade)
barplot(entry, legend = TRUE)

#survey의 Gender 1개의 인자를 가지고 파이차트를 작성하세요.
pie(table(survey $Grade))

#survey의 Age 1개의 인자를 가지고 히스토그램을 작성하세요.
hist(survey $Age, main = "age", col = terrain.colors(12))

#survey의 Grade별 Age를 비교하는 박스 플롯을 만들어보세요. 그리고 Grade별 Age에 대한 기술 통계 분석을 실시하여 각 박스 플롯을 비교 설명하세요.  
grade2 = subset(survey, survey$Grade=='2')
grade3 = subset(survey, survey$Grade=='3')
grade4 = subset(survey, survey$Grade=='4')
boxplot(grade2,grade3,grade4, col = "yellow", names = c("grade2", "grade3", "grade4"))

#survey의 Grade를 X값으로 Age를 Y값으로 하는 산점도를 만들어보세요.
plot(x=survey$Grade, y=survey$Age, xlab = "Gender", ylab = "Age",pch = 24, col = "red", bg = "yellow", cex = 1.5)