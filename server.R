library(shiny)

zz <- file("D:/SHINYPROJECT/CorrelationAnalysis/error.txt", open="wt")
sink(zz, type="message")
print("Loading pakages..")
library(xts)
library(lubridate)
library(data.table)
library(RMySQL)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)
library(dygraphs)

FinalPortfolio<-NULL
data<-NULL
courseDBChoise <- function(startDate, endDate, inst1, ratio1, inst2, ratio2, inst3, ratio3, inst4, ratio4, inst5, ratio5, nInterval, Interval){
  startDate<-'2015-10-10'
  endDate<-'2015-11-01'
  inst1<-"AD"
  ratio1<-.12#476.1 #/.21
  inst2<-"TY"
  ratio2<--1
  inst3<-"AX"
  ratio3<-100
  inst4<-""
  ratio4<-1
  inst5<-""
  ratio5<-1
  nInterval<-10
  Interval<-"mins"
  
  print(paste("Fetching Data", inst1))
  sqlData <-NULL
  nInst<-0
  data1<-NULL
  data2<-NULL
  data3<-NULL
  data4<-NULL
  data5<-NULL
  #con <- dbConnect(MySQL(), user='root', password='data786', dbname='new_price_data', host='192.168.115.233')
  con <- dbConnect(MySQL(), user='root', password='data786', dbname='GMT_Minute_Data', host='192.168.115.233')
  sqlData <- dbSendQuery(con, paste0('select TradeTime, Open, High, Low, Close from ', inst1, ' where tradetime between \'',  startDate, '\' and \'',  endDate, '\'' ))
  ad = dbFetch(sqlData, n=-1)
  
  sqlData <- dbSendQuery(con, paste0('select TradeTime, Open, High, Low, Close from ', inst2, ' where tradetime between \'',  startDate, '\' and \'',  endDate, '\'' ))
  ty = dbFetch(sqlData, n=-1)
  
  sqlData <- dbSendQuery(con, paste0('select TradeTime, Open, High, Low, Close from ', inst3, ' where tradetime between \'',  startDate, '\' and \'',  endDate, '\'' ))
  ax = dbFetch(sqlData, n=-1)
  connections <- dbListConnections(MySQL())
  for(i in connections) {dbDisconnect(i)}
  
  xtsad <- as.xts(ad[2:5],  ymd_hms(ad[,1], tz="GMT"))
  xtsty <- as.xts(ty[2:5],  ymd_hms(ty[,1], tz="GMT"))
  xtsax <- as.xts(ax[2:5],  ymd_hms(ax[,1], tz="GMT"))
  
  finalMergedData<-na.omit(merge(xtsad,xtsty))
  finalMergedData<-na.omit(merge(finalMergedData, xtsax))
  
  xtsad <- finalMergedData[,c(1:4)]
  xtsty <- finalMergedData[,c(5:8)]
  xtsax <- finalMergedData[,c(9:12)]
  
  act<-(xtsad * .471) - (xtsty * .12)
  str<-"(xtsad * .471) - (xtsty * .12)"
  
  a<-eval(parse(text=str))
  head(a)
  
  
  nInterval=10
  xtsad<-to.period(xtsad,"minutes", nInterval, indexAt="startof", OHLC=T)
  xtsty<-to.period(xtsty,"minutes", nInterval, indexAt="startof", OHLC=T)
  xtsax<-to.period(xtsax,"minutes", nInterval, indexAt="startof", OHLC=T)
  
  finalMergedData<-na.omit(merge(xtsad,xtsty))
  finalMergedData<-na.omit(merge(finalMergedData, xtsax))
  
  ColList<-grep("Close", names(finalMergedData), value=T)
  FinalPortfolio<-NULL
  FinalPortfolio <- data.table(TradeTime=index(finalMergedData), coredata(finalMergedData)[,ColList])
  
  FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1)) + (get(ColList[2]) *  as.numeric(ratio2)) + (get(ColList[3]) *  as.numeric(ratio3))]
  FinalPortfolio[, c("SpreadRet", inst1, inst2, inst3) := list(c(0,diff((Spread) * 100)), c(0,diff(log(get(ColList[1])))), c(0,diff(log(get(ColList[2])))), c(0,logreturn1(get(ColList[3]))))]
  
  write.csv(FinalPortfolio, "D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
  
  
  if(inst1 != ""){
    sqlData <- dbSendQuery(con, paste0('select TradeTime, Open, High, Low, Close from ', inst1, ' where tradetime between \'',  startDate, '\' and \'',  endDate, '\'' ))
    
    ad = dbFetch(sqlData, n=-1)
    nInst<-nInst+1
  } 
  if(inst2 != ""){
    sqlData <- dbSendQuery(con, paste0('select TradeTime, Open, High, Low, Close from ', inst2, ' where tradetime between \'',  startDate, '\' and \'',  endDate, '\'' ))
    data2 = dbFetch(sqlData, n=-1)
    nInst<-nInst+1
  }
  if(inst3 != ""){
    sqlData <- dbSendQuery(con, paste0('select TradeTime, Open, High, Low, Close from ', inst3, ' where tradetime between \'',  startDate, '\' and \'',  endDate, '\'' ))
    ty = dbFetch(sqlData, n=-1)
    nInst<-nInst+1
  }
  if(inst4 != ""){
    sqlData <- dbSendQuery(con, paste0('select TradeTime, Open, High, Low, Close from ', inst4, ' where tradetime between \'',  startDate, '\' and \'',  endDate, '\'' ))
    ax = dbFetch(sqlData, n=-1)
    nInst<-nInst+1
  }
  if(inst5 != ""){
    sqlData <- dbSendQuery(con, paste0('select TradeTime, Open, High, Low, Close from ', inst5, ' where tradetime between \'',  startDate, '\' and \'',  endDate, '\'' ))
    data5 = dbFetch(sqlData, n=-1)
    nInst<-nInst+1
  }
  connections <- dbListConnections(MySQL())
  for(i in connections) {dbDisconnect(i)}
  #suppressWarnings(dbDisconnect(con))
  if(nInst==1)
    stop("Please select more than one instruments.")
  if(nInst >= 1)
    xtsData1 <- as.xts(data1[2:5],  ymd_hms(data1[,1], tz="GMT"))
  if(nInst >= 2)
    xtsData2 <- as.xts(data2[2:5],  ymd_hms(data2[,1], tz="GMT"))
  if(nInst >= 3)
    xtsData3 <- as.xts(data3[2:5],  ymd_hms(data3[,1], tz="GMT"))
  if(nInst >= 4)
    xtsData4 <- as.xts(data4[2:5],  ymd_hms(data4[,1], tz="GMT"))
  if(nInst == 5)
    xtsData5 <- as.xts(data5[2:5],  ymd_hms(data5[,1], tz="GMT"))
  print(head(xtsData1))
  print(head(xtsData2))
  print(head(xtsData3))
  
  data1<-NULL
  data2<-NULL
  data3<-NULL
  data4<-NULL
  data5<-NULL
  
  ################ Merge all products in minute data
  merged<-NULL
  finalMergedData<-NULL
  #do.call(merge, R)  do.call(merge, monthly)
  #cdates <- Reduce(intersect,list(data1$TradeTime,data2$TradeTime,data3$TradeTime))
  if(nInst >= 2){
    finalMergedData<-na.omit(merge(xtsData1,xtsData2))
    print(head(finalMergedData))
    write.zoo(finalMergedData, "D:/SHINYPROJECT/MADistanceShiny/testing.csv", sep=",")
  }
  if(nInst >= 3){
    finalMergedData<-na.omit(merge(finalMergedData, xtsData3))
    #write.zoo(xtsData3, "D:/SHINYPROJECT/MADistanceShiny/testing2.csv", sep=",")
    print(head(finalMergedData))
  }
  if(nInst >= 4)
    finalMergedData<-na.omit(merge(finalMergedData, xtsData4))
  if(nInst == 5)
    finalMergedData<-na.omit(merge(finalMergedData, xtsData5))
  print("merged")
  merged<-NULL 
  xtsData1<-NULL
  xtsData2<-NULL
  xtsData3<-NULL
  xtsData4<-NULL
  xtsData5<-NULL
  
  #to.daily not working on all columns therefore seperating all inst and aggregating individually
  if(nInst >= 1)
    xtsData1 <- finalMergedData[,c(1:4)]
  if(nInst >= 2)
    xtsData2 <- finalMergedData[,c(5:8)]
  if(nInst >= 3)
    xtsData3 <- finalMergedData[,c(9:12)]
  if(nInst >= 4)
    xtsData4 <- finalMergedData[,c(13:16)]
  if(nInst == 5)
    xtsData5 <- finalMergedData[,c(17:20)]
  
  ############# Aggregate into higher time Frame
  
  #period.apply(x, endpoints(x,"hours"), sum) 
  print("Converting Time Frame")
  if(Interval=="mins"){
    if(nInst >= 1)
      xtsData1<-to.period(xtsData1,"minutes", nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 2)
      xtsData2<-to.period(xtsData2,"minutes", nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 3)
      xtsData3<-to.period(xtsData3,"minutes", nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 4)
      xtsData4<-to.period(xtsData4,"minutes", nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 5)
      xtsData5<-to.period(xtsData5,"minutes", nInterval, indexAt="startof", OHLC=T)
  } else if(Interval=="hours")  {
    
    if(nInst >= 1)
      xtsData1<-to.period(xtsData1,"hours", nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 2)
      xtsData2<-to.period(xtsData2,"hours", nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 3)
      xtsData3<-to.period(xtsData3,"hours", nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 4)
      xtsData4<-to.period(xtsData4,"hours", nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 5)
      xtsData5<-to.period(xtsData5,"hours", nInterval, indexAt="startof", OHLC=T)
  } else if(Interval=="daily")  {
    
    if(nInst >= 1)
      xtsData1<-to.daily(xtsData1,drop.time=T, nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 2)
      xtsData2<-to.daily(xtsData2,drop.time=T, nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 3)
      xtsData3<-to.daily(xtsData3,drop.time=T, nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 4)
      xtsData4<-to.daily(xtsData4,drop.time=T, nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 5)
      xtsData5<-to.daily(xtsData5,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  } else if(Interval=="weekly") {
    
    if(nInst >= 1)
      xtsData1<-to.weekly(xtsData1,drop.time=T, nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 2)
      xtsData2<-to.weekly(xtsData2,drop.time=T, nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 3)
      xtsData3<-to.weekly(xtsData3,drop.time=T, nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 4)
      xtsData4<-to.weekly(xtsData4,drop.time=T, nInterval, indexAt="startof", OHLC=T)
    if(nInst >= 5)
      xtsData5<-to.weekly(xtsData5,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  }
  
  if(nInst >= 2)
    finalMergedData<-na.omit(merge(xtsData1,xtsData2))
  if(nInst >= 3)
    finalMergedData<-na.omit(merge(finalMergedData, xtsData3))
  if(nInst >= 4)
    finalMergedData<-na.omit(merge(finalMergedData, xtsData4))
  if(nInst == 5)
    finalMergedData<-na.omit(merge(finalMergedData, xtsData5))
  
  write.csv(finalMergedData, "D:/SHINYPROJECT/MADistanceShiny/MergedSeries.csv")
  
  ColList<-grep("Close", names(finalMergedData), value=T)
  FinalPortfolio<-NULL
  FinalPortfolio <- data.table(TradeTime=index(finalMergedData), coredata(finalMergedData)[,ColList])
  
  print(ColList)
  finalMergedData<-NULL 
  xtsData1<-NULL
  xtsData2<-NULL
  xtsData3<-NULL
  xtsData4<-NULL
  xtsData5<-NULL
  
  ##########  Create Strategy
  #x<-c(1,2,8,4,5)
  logreturn1 <- function(x) log(tail(x,-1)/head(x,-1))  #MAKE SURE PRIC SERICE IN ASSENDING ORDER ELSE RETULE WILL BE -VE
  #c(0,diff(log(x))
  #Delt(x) % RETURNS
  print("Creating Spread")
  if(nInst==2){
    print("2")
    FinalPortfolio[, Spread:= (get(ColList[1]) * as.numeric(ratio1)) + (get(ColList[2]) *  as.numeric(ratio2))]
    FinalPortfolio[, c("SpreadRet", inst1 , inst2) := list(c(0,diff((Spread) * 100)), c(0,diff(log(get(ColList[1])))), c(0,diff(log(get(ColList[2])))))]
  }
  if(nInst==3){
    #FinalPortfolio[, Spread:= (AD/.21) - (Close.1 * .12) - (Close.19 * 100)]
    print("3")
    FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1)) + (get(ColList[2]) *  as.numeric(ratio2)) + (get(ColList[3]) *  as.numeric(ratio3))]
    FinalPortfolio[, c("SpreadRet", inst1, inst2, inst3) := list(c(0,diff((Spread) * 100)), c(0,diff(log(get(ColList[1])))), c(0,diff(log(get(ColList[2])))), c(0,logreturn1(get(ColList[3]))))]
  }
  if(nInst==4){
    print("4")
    FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1)) + (get(ColList[2]) *  as.numeric(ratio2)) + (get(ColList[3]) *  as.numeric(ratio3)) + (get(ColList[4]) *  as.numeric(ratio4))]
    FinalPortfolio[, c("SpreadRet", inst1, inst2, inst3, inst4) := list(c(0,diff((Spread) * 100)), c(0,diff(log(get(ColList[1])))), c(0,diff(log(get(ColList[2])))), c(0,diff(log(get(ColList[3])))), c(0,logreturn1(get(ColList[4]))))]
  }
  if(nInst==5){
    print("5")
    FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1)) + (get(ColList[2]) *  as.numeric(ratio2)) + (get(ColList[3]) *  as.numeric(ratio3)) + (get(ColList[4]) *  as.numeric(ratio4)) + (get(ColList[5])*  as.numeric(ratio5))]
    FinalPortfolio[, c("SpreadRet", inst1, inst2, inst3, inst4, inst5) := list(c(0,diff((Spread) * 100)), c(0,diff(log(get(ColList[1])))), c(0,diff(log(get(ColList[2])))), c(0,diff(log(get(ColList[3])))), c(0,diff(log(get(ColList[4])))), c(0,logreturn1(get(ColList[5]))))]
  }
  
  print(head(FinalPortfolio))
  print(paste("Finished "))
  #DT[, c("AUDRet", "NotesRet", "TensRet", "PortRet") := list(round(Delt(AD),6), round(Delt(TY),6), round(Delt(XM),6), round(Delt(Spread),6))]
  
  write.csv(FinalPortfolio, "D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
  
  return(FinalPortfolio[,Spread])##here
}

#head((DTPort$Close * 476.1) - (DTPort$Close.1 * 12) + (DTPort$Close.2 * 100))

CalculateNumberOfInstruments <- function(inst1,inst2,inst3,inst4,inst5){
  #inst1="TY"
  #inst2="XM"
  # inst3=""
  #inst4=""
  #inst5=""
  nInst<-0
  if(inst1 != "")
    nInst<-nInst+1
  if(inst2 != "")
    nInst<-nInst+1
  if(inst3 != "")
    nInst<-nInst+1
  if(inst4 != "")
    nInst<-nInst+1
  if(inst5 != "")
    nInst<-nInst+1
  return(nInst)
} 

shinyServer(function(input, output)
  {
  
  
  #getFreshData<-eventReactive(input$goButton, {
  #print("Enter getFreshData")
  #var<-input$Model
  #print(paste(""))
  #data<-read.csv("D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
  #print("Exit getFreshData")
  #as.data.table(data)
  #})
  
  getListofModels<-eventReactive(input$goButton, {
    var<-input$Model
    print(paste("Text box", var))
  })
  
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)
  ntext <- eventReactive(input$goButton, {
    print("Creating Portfolio")
    a<-courseDBChoise(isolate(input$Date[1]),isolate(input$Date[2]),input$Inst1, input$Ratio1, input$Inst2, input$Ratio2, input$Inst3, input$Ratio3, input$Inst4, 
                      input$Ratio4, input$Inst5, input$Ratio5, input$nInterval, input$Interval)
  })
  
  #output$VolOut <- renderText({
  # ntext()
  #})
  
  output$PricePlot <- renderDygraph({
    #print("Enter PricePlot")
    #ntext()
    
    data<-read.csv("D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
    SMALength<-input$SMA
    Interval<-isolate(input$Interval)
    print(SMALength)
    print(length(data[,"Spread"]))
    print(Interval)
    if(length(data[,"Spread"]) < SMALength)
    {
      print("1")
      if(Interval=="daily" || Interval=="weekly")
        xxts<-as.xts(data[,"Spread"], ymd(data[,"TradeTime"]))
      else
        xxts<-as.xts(data[,"Spread"], ymd_hms(data[,"TradeTime"], tz="GMT"))
      
      names(xxts)<-c("Spread")
    } else {
      print("2")
      MA200<-runMean(data[,"Spread"], n = SMALength, cumulative = FALSE)
      if(Interval=="daily" || Interval=="weekly")
        xxts<-as.xts(cbind(data[,"Spread"], MA200), ymd(data[,"TradeTime"]))
      else
        xxts<-as.xts(cbind(data[,"Spread"], MA200), ymd_hms(data[,"TradeTime"], tz="GMT"))
      
      names(xxts)<-c("Spread","SMA")
    }
    print("Exit PricePlot.")
    if(length(data[,"Spread"]) > SMALength){
      dygraph(xxts, main = "Model Price Series") %>%
        dySeries(c("Spread"), label = c("Price"))%>%
        dySeries(c("SMA"), label = "SMA")%>%
        dyOptions(drawGrid = TRUE) %>%
        dyRangeSelector()
    } else {
      dygraph(xxts, main = "Model Price Series") %>%
        dySeries(c("Spread"), label = c("Price"))%>%
        dyOptions(drawGrid = TRUE) %>%
        dyRangeSelector()
    }
    #print("Exit PricePlot")
    # dyOptions(stackedGraph = TRUE) %>%
    #dyRangeSelector()
    
    #chartSeries(xxts, theme = chartTheme("white"), type = "line", TA = NULL) 
    #addSMA(n=input$SMA, col="red")
  })
  
  output$CorrPlot<- renderPlot({
    print("Enter CorrPlot")
    #data<-read.csv("D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
    data<-as.data.frame(getFreshData())
    nCols<-CalculateNumberOfInstruments(isolate(input$Inst1),isolate(input$Inst2),isolate(input$Inst3),isolate(input$Inst4),isolate(input$Inst5))
    print(nCols)
    print("Exit CorrPlot")
    chart.Correlation(data[, -(1:as.numeric(nCols+4))], histogram = F, method = c("kendall"))
    
  })
  
  output$ProbPlot <- renderPlot({
    print("ENTER ProbPlot")
    #data<-read.csv("D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
    #calculateProbability (as.data.table(data))
    print("Exit ProbPlot")
    calculateProbability (getFreshData())
  })
  
  output$SummaryAbove<- renderPrint({
    print("Enter SummaryAbove")
    print("Quantiles for Price ABOVE SMA")
    hr()
    print("Exit SummaryAbove")
    #data<-read.csv("D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
    calculateSummaryAbove (getFreshData())
  })
  
  output$SummaryBelow<- renderPrint({
    print("Enter SummaryBelow")
    print("Quantiles for Price BELOW SMA")
    hr()
    print("Exit SummaryBelow")
    #data<-read.csv("D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
    calculateSummaryBelow (getFreshData())
    
  })
  
  output$BoxPlot<- renderPlot({
    #data<-read.csv("D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
    print("Enter BoxPlot")
    summ<-calculateSummaryPlot (getFreshData())
    #boxplot(t(summ), at= 1:2, horizontal = T)
    m <- melt(summ)
    m <- transform(m,variable=reorder(Var1,value))
    #pdf(width=10,height=50,file="boxplot.pdf")
    print("Exit BoxPlot")
    ggplot(m,aes(x=variable,y=value))+geom_boxplot()+coord_flip()
    #dev.off()
    
  })
  
  #output$ChangePlot<- renderPlot({
  #data<-read.csv("D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
  #plot(data[,"TradeTime"],data[,"SpreadRet"], type="l")
  #   data<-getFreshData()[,c(TradeTime, SpreadRet)]
  #  print(data)
  #  plot(data[,"TradeTime"], data[,"SpreadRet"])
  #})
  
  
  #output$VolatilityPlot<- renderPlot({
  #  data<-read.csv("D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
  #  print(head(data))
  
  #  plot(apply(data[,-(1:4)]*100, 2, sd), type="h",col = "red", lwd = 10)
  
  #})
  
  #HTML(paste0("Logged in as <code>", user()$user, "</code> with <code>", airlineName(userCompany()),"</code>."))
})

calculateSummaryPlot <- function(data){
  #data<-read.csv("D:/SHINYPROJECT/MADistanceShiny/FinalPortfolioSeries.csv")
  #data<-as.data.table(data)
  Price<-data[,Spread]
  if(length(Price) < 200)
    stop(paste('Oops! Only',  length(Price), 'number of rows. Cannot calculate 200 SMA.'))
  MA200<-runMean(data[,Spread], n = 200, cumulative = FALSE)
  
  DT<-as.data.table(cbind(Price, MA200))
  
  DT[,Distance:=round(Price-MA200,2)]
  dist<-DT[,Distance]
  
  PositiveDT<-DT[Distance>0,Distance]
  NegetiveDT<-DT[Distance<0,-Distance]
  
  AboveSMA<-summary(fivenum(PositiveDT))
  BelowSMA<-summary(fivenum(NegetiveDT))
  
  finaldata<-rbind(AboveSMA, BelowSMA)
  #boxplot(t(finaldata), horizontal = T)
  #print(summary(fivenum(PositiveDT)))
  return(finaldata)
}

calculateSummaryAbove <- function(data){
  
  Price<-data[,Spread]
  if(length(Price) < 200)
    stop(paste('Oops! Only',  length(Price), 'number of rows. Cannot calculate 200 SMA.'))
  MA200<-runMean(data[,Spread], n = 200, cumulative = FALSE)
  
  DT<-as.data.table(cbind(Price, MA200))
  
  DT[,Distance:=round(Price-MA200,2)]
  dist<-DT[,Distance]
  
  PositiveDT<-DT[Distance>0,Distance]
  #print(summary(fivenum(PositiveDT)))
  return(summary(fivenum(PositiveDT)))
}

calculateSummaryBelow <- function(data){
  
  Price<-data[,Spread]
  if(length(Price) < 200)
    stop(paste('Oops! Only',  length(Price), 'number of rows. Cannot calculate 200 SMA.'))
  MA200<-runMean(data[,Spread], n = 200, cumulative = FALSE)
  
  DT<-as.data.table(cbind(Price, MA200))
  
  DT[,Distance:=round(Price-MA200,2)]
  dist<-DT[,Distance]
  
  NegetiveDT<-DT[Distance<0,-Distance]
  #print(summary(fivenum(PositiveDT)))
  return(summary(fivenum(NegetiveDT)))
}

calculateProbability <- function(data){
  data<-as.data.table(data)
  Price<-data[,Spread]
  print(length(Price))
  if(length(Price) < 200)
    stop(paste('Oops! Only',  length(Price), 'number of rows. Cannot calculate 200 SMA.'))
  
  MA200<-runMean(data[,Spread], n = 200, cumulative = FALSE)
  
  DT<-as.data.table(cbind(Price, MA200))
  
  DT[,Distance:=round(Price-MA200,2)]
  
  PositiveDT<-DT[Distance>0,Distance]
  NegativeDT<-DT[Distance<0,-Distance]
  
  summary(fivenum(NegativeDT))
  
  pos.ecdf<-ecdf(PositiveDT)
  neg.ecdf<-ecdf(NegativeDT)
  
  ############################################## Find Probalility
  acumulated.distrib= function(sample,x){
    minors= 0
    for(n in sample){
      
      if(n<=x){
        minors= minors+1
      }
    }
    return (minors/length(sample))
  }
  
  referencePoint<-4 # make variable
  
  ################################### Plot Price above MA
  
  aHistColor=rgb(1,0,0,0.2)
  # create the plot directly with just the plot.ecdf() function, but this doesn't produce any empirical CDF values
  plot.ecdf(PositiveDT,xlab = 'Distance from 200 MA', ylab = '', main = 'Prob(Price ABOVE from 200 MA)', col=aHistColor)
  mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5)
  
  percetage<-acumulated.distrib(na.omit(PositiveDT),referencePoint)
  
  # mark the 3rd quartile
  abline(v = referencePoint, h = percetage)
  #legend(referencePoint,percetage , paste0(round(percetage * 100,1), '%', ' Prob of <= ', referencePoint ) , box.lwd = 0)
  legend(referencePoint,percetage , paste0(round(percetage * 100,1), '%') , box.lwd = 0)
  
  ################################### Plot Price below MA
  
  bHistColor=rgb(0,1,0,0.2)
  plot.ecdf(NegativeDT, xlab = 'Distance from 200 MA', ylab = '', main = 'Prob(Price BELOW from 200 MA)', col=bHistColor, add=T)
  mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5)
  
  percetage<-acumulated.distrib(na.omit(NegativeDT),referencePoint)
  
  # mark the 3rd quartile
  abline(v = referencePoint, h = percetage)
  #legend(referencePoint,percetage , paste0(round(percetage * 100,1), '%', ' Prob of <= ', referencePoint ) , box.lwd = 0)
  legend(referencePoint,percetage , paste0(round(percetage * 100,1), '%') , box.lwd = 0)
  
  # Add a legend to the chart.
  legend('right', c('Above', 'Below'), fill=c('red', 'green'))
}

#runApp('D:/SHINYPROJECT/MADistanceShiny',host="192.168.115.234",port=3700)



