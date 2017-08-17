# 1. use XLConnect
library(XLConnect)
library(lubridate)
library(scales)
library(ggplot2)
# 2. set url for download
url <- "http://www.ndc.gov.tw/LatestFile.aspx?n=9D32B61B1E56E558&sms=9D3CAFD318C60877&s=C367F13BF38C5711&type=xls"
destfile <- "indicator201706.xlsx"
download.file(url, destfile, mode = "wb")
# 3. read Excel file(xlsx)
monitor <- readWorksheetFromFile(destfile, sheet = 1, header=TRUE)
# 4. show data
head(monitor)
tail(monitor)
# 5. Change Column name
# IDX1:領先指標綜合指數, IDX2:領先指標不含趨勢指數, IDX3:同時指標綜合指數
# IDX4:同時指標不含趨勢指數, IDX5:落後指標綜合指數, IDX6:落後指標不含趨勢指數
# Fraction:景氣對策信號綜合分數, Light:景氣對策信號
colnames(monitor) <- c("DATE","IDX1", "IDX2", "IDX3","IDX4","IDX5","IDX6","Fraction","Light")
monitor$year <- year(monitor$DATE)
monitor$month <- month(monitor$DATE, label = TRUE)
head(monitor)
tail(monitor)
M201706 <- subset(monitor, monitor$DATE > "2016-05-01")
head(M201706)
tail(M201706)
plot(M201706$DATE, M201706$Fraction, type = "b", ylim = c(15,35))
# 6. Compare 2016/2017
M2016 <- subset(monitor, monitor$DATE>"2015-12-01" & monitor$DATE<"2016-07-01")
M2016
M2017 <- subset(monitor, monitor$DATE>"2016-12-01" )
M2017
plot(M2016$DATE, M2016$Fraction, type = "b", ylim = c(10,35), col="blue", xaxt="n")
axis(1, M2016$DATE, labels = M2016$DATE)
par(new=T)
plot(M2017$DATE, M2017$Fraction, type = "b",axes=F, ylab = "", ylim = c(10,35), col="green")
# Difference
DIFF <- NULL
DIFF$MM <- c("01","02","03","04","05","06")
DIFF$Y2016 <- M2016$Fraction
DIFF$Y2017 <- M2017$Fraction
DIFF <- as.data.frame(DIFF)
DIFF
DIFF$RANGE <- DIFF$Y2017-DIFF$Y2016
DIFF$BEF <- c(NA, DIFF$Y2017[2:6] - DIFF$Y2017[1:5])
DIFF
# Leading
plot(M2017$DATE, M2017$IDX1, type = "b", ylim = c(min(M201706$IDX1),max(M201706$IDX1)))
# ggplot
M1617 <- subset(monitor, monitor$DATE > "2015-12-01")
g <- ggplot(M1617, aes(x=month, y=Fraction))
g <- g + geom_line(aes(color=factor(year), group = year))
g <- g + scale_color_discrete(name="Year")
g <- g + scale_y_continuous(labels = comma)
g <- g + labs(title="Monitoring Indicator", x="Month", y="Fraction")
g
