#******************************************************************************************
#****************************************************************************************** 
#York University
#Assignment Submition for the course CSDA1010 Basic Methods of Data Analytics 
#Lab1
# Submitted by: Melkamu Dedefo Gishu




#**********************************************************
#import dataset
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")
#**********************************************************************************


#Data Cleaning
str(IBM)
str(GE)
str(ProcterGamble)
str(CocaCola)
str(Boeing)

# 1. What is the data type of the Date variable?
# ANSWER Factor
GE$Date = as.Date(GE$Date, "%m/%d/%y")
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

  #1. How many rows of data are in each dataset? 
  #   480
  #2. What is the earliest/latest year in our datasets?    
     min(GE$Date)  # 1970-01-01 
     max(GE$Date)  # 2009-12-01
  
  #3. For the period above what is the average stock price of Coca Cola?
      mean(CocaCola$StockPrice)  #60.02973
  #4. What is the maximum price of IBM during this period?
      max(IBM$StockPrice)        # 438.9016
  #5. What is the standard deviation of P&G stock price over this period?
      sd(ProcterGamble$StockPrice)  # 18.19414
  #6. What is the median price of Boeing in the last 5 years for which we have data?
      Boeinglast5yr= subset(Boeing, Date > "2004-12-01", c("Date", "StockPrice"))
      median(Boeinglast5yr$StockPrice, na.rm=TRUE)  # 69.67567
      
#*************************************************************************************    
      #Part II basic ploting
      
      plot(CocaCola$Date, CocaCola$StockPrice)
      plot(CocaCola$Date, CocaCola$StockPrice, "l", col='red')
      # 1. Identify the year during which Coca-Cola had the highest/lowest stock price?
          #the highest around 1973  and lowest around 1980
      # 2  What calendar year did it look to have the biggest (Year-over-Year) percentage increase?
          # around 1971-1972
      
      lines(ProcterGamble$Date, ProcterGamble$StockPrice,"l", col='blue')
      
      # 1 In March of 2000 the stock market plummeted as the tech bubble burst. Using the plot above,
      #  which company’s stock dropped more (relatively – i.e. percentage-wise)?
      #  In March 200 the stock price of ProcterGamble dropped more than CocaCola percentage-wise.
      # 2. In the year 1983 which company stock was going up? Which was going down?
             # in 1983   Stoke price for CocaCola going up where as stock price for ProcterGamble going down
      #3. Across the entire time period shown in your plot which stock had a generally lower price?
            #  CocaCola
      
#********************************************************************************************************
      
      # Data Visualization from 1995-2005:
      
      Coca1995_2005= subset(CocaCola, Date>="1995-01-01"& Date<="2005-12-01", c("Date", "StockPrice"))
      #First stock price of the year 1995 sits in row position: 301
      #Last stock price of the year 2005 sits in row position: 432
      plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
      lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],"l", col='blue',ylim=c(0,210))
      
    
      #1. Which stock price fell the most right after the tech bubble of March 2000?
         #CocaCola
      #2. What stock had the highest maximum price between 1995-2005?
          # ProcterGamble
      # 3. Afewyearsbeforethetechbubbleof1997,therewasanotherstockmarketcrashtriggedby
      #economic crisis in Asia in October of 1997. If you compare stock prices from September 1997 to
      #November 1997, which companies saw a decrease in price? Which company experienced the
      #biggest decrease?
      #  both campanies showed decrease. ProcterGamble showed the biggest decresse 
      #  4. Whichstock seemedtoprovidethebest return(i.e.increaseinprice)between2004-2005?
      #  Coca
      #  5. Between 1995-2005, which company had the biggest delta between the maximum and
      #minimum stock price?    ProcterGamble
      max(CocaCola$StockPrice[301:432])-min(CocaCola$StockPrice[301:432]) #45.67
       max(ProcterGamble$StockPrice[301:432])-min(ProcterGamble$StockPrice[301:432]) #96.62
      
      # Which two companies’ stock price seem to be the most correlated (i.e. move up/down together)?
      
       plot(CocaCola$Date, CocaCola$StockPrice, "l", col='red')
       lines(ProcterGamble$Date, ProcterGamble$StockPrice,"l", col='blue')
       lines(Boeing$Date, Boeing$StockPrice,"l", col='black')
       lines(GE$Date, GE$StockPrice,"l", col='yellow')
       lines(IBM$Date, IBM$StockPrice,"l", col='green')
        
       # CocaCola with IBM
       
#*********************************************************************************************************       
# monthly trend analysis
#*********************************************************************************************************
       mean(IBM$StockPrice)
       tapply(IBM$StockPrice,months(IBM$Date), FUN=mean, na.rm=TRUE)
       plot(tapply(IBM$StockPrice,months(IBM$Date), FUN=mean, na.rm=TRUE),xaxt="n", col='green')
       axis(1, at=1:length(months(IBM$Date)), labels=months(IBM$Date),"l")
       #1 ForIBM,comparetheaveragestockpriceforeachmonthtotheitsoverallaveragestockprice
       #andidentifyallthemonthsforwhichIBMhistoricallyhadahigherstockprice(wecallthisoverindexing)? Which month over-indexed the most?
       # historical higher stockprice for IBM occurred in the months of January, February, March, May and April
       # And the month of February over-indexed the most
     
       # 2. Repeatthetapply()functionyouusedtosolve thelastquestionforeachof the 4 remaining
       #companies. 
       #Do any of two or more companies have their highest stock price in the same months as each other?  YES
       # Which companies and months does this happen for? CocaCola and GE in the month of April
       
       mean(Boeing$StockPrice)
       tapply(Boeing$StockPrice,months(Boeing$Date), FUN=mean, na.rm=TRUE)
       plot(tapply(Boeing$StockPrice,months(Boeing$Date), FUN=mean, na.rm=TRUE),xaxt="n", col='black')
       axis(1, at=1:length(months(Boeing$Date)), labels=months(Boeing$Date),"l")
       # historical higher stockprice for Boeing occurred in the months of February, June, March, May, August and April
       # And the month of May over-indexed the most
       
       
       mean(CocaCola$StockPrice)
       tapply(CocaCola$StockPrice,months(CocaCola$Date), FUN=mean, na.rm=TRUE)
       plot(tapply(CocaCola$StockPrice,months(CocaCola$Date), FUN=mean, na.rm=TRUE),xaxt="n", col='red')
       axis(1, at=1:length(months(CocaCola$Date)), labels=months(CocaCola$Date),"l")
       # historical higher stockprice for CocaCola occurred in the months of January, February, June, March, May and April
       # And the month of April over-indexed the most
       
       mean(ProcterGamble$StockPrice)
       tapply(ProcterGamble$StockPrice,months(ProcterGamble$Date), FUN=mean, na.rm=TRUE)
       plot(tapply(ProcterGamble$StockPrice,months(ProcterGamble$Date), FUN=mean, na.rm=TRUE),xaxt="n", col='blue')
       axis(1, at=1:length(months(ProcterGamble$Date)), labels=months(ProcterGamble$Date),"l")
       # historical higher stockprice for ProcterGamble occurred in the months of January, February, May, November and December
       # And the month of January over-indexed the most
       
       mean(GE$StockPrice)
       tapply(GE$StockPrice,months(GE$Date), FUN=mean, na.rm=TRUE)
       plot(tapply(GE$StockPrice,months(GE$Date), FUN=mean, na.rm=TRUE),xaxt="n", col='yellow')
       axis(1, at=1:length(months(GE$Date)), labels=months(GE$Date),"l")
       # historical higher stockprice for ProcterGamble occurred in the months of April, January, February, March and May
       # And the month of April over-indexed the most
      

       # 3. What trend do you see for the months of December vs January for each company? Is there an
       #over-arching trend that applies to all companies when comparing all historical December vs
       #January stock prices?
       #CocacCola_DecJan <- subset(CocaCola, months(CocaCola$Date) =="December" | months(CocaCola$Date)=="January", c("Date", "StockPrice"))
       
       CocacCola_Dec <- subset(CocaCola, months(CocaCola$Date) =="December", c("Date", "StockPrice"))
       CocacCola_Jan <- subset(CocaCola, months(CocaCola$Date)=="January", c("Date", "StockPrice"))
         plot(CocacCola_Dec$Date, CocacCola_Dec$StockPrice, "l", col='red')
         lines(CocacCola_Jan$Date, CocacCola_Jan$StockPrice,"l", col='black')
       # from the trend line it is shown that historical December vs January stock prices of Cocacola follows the same trend all over.
        
         Boeing_Dec <- subset(Boeing, months(Boeing$Date) =="December", c("Date", "StockPrice"))
         Boeing_Jan <- subset(Boeing, months(Boeing$Date)=="January", c("Date", "StockPrice"))
         plot(Boeing_Dec$Date, Boeing_Dec$StockPrice, "l", col='red')
         lines(Boeing_Jan$Date, Boeing_Jan$StockPrice,"l", col='black')
         # from the trend line it is shown that historical December vs January stock prices of Boieng follows the same trend all over.
         
         GE_Dec <- subset(GE, months(GE$Date) =="December", c("Date", "StockPrice"))
         GE_Jan <- subset(GE, months(GE$Date)=="January", c("Date", "StockPrice"))
         plot(GE_Dec$Date, GE_Dec$StockPrice, "l", col='red')
         lines(GE_Jan$Date, GE_Jan$StockPrice,"l", col='black')
         # from the trend line it is shown that historical December vs January stock prices of GE follows the same trend all over.
         
         IBM_Dec <- subset(IBM, months(IBM$Date) =="December", c("Date", "StockPrice"))
         IBM_Jan <- subset(IBM, months(IBM$Date)=="January", c("Date", "StockPrice"))
         plot(IBM_Dec$Date, IBM_Dec$StockPrice, "l", col='red')
         lines(IBM_Jan$Date, IBM_Jan$StockPrice,"l", col='black')
         # from the trend line it is shown that historical December vs January stock prices of IBM follows the same trend all over.
         
         ProcterGamble_Dec <- subset(ProcterGamble, months(ProcterGamble$Date) =="December", c("Date", "StockPrice"))
         ProcterGamble_Jan <- subset(ProcterGamble, months(ProcterGamble$Date)=="January", c("Date", "StockPrice"))
         plot(ProcterGamble_Dec$Date, ProcterGamble_Dec$StockPrice, "l", col='red')
         lines(ProcterGamble_Jan$Date, ProcterGamble_Jan$StockPrice,"l", col='black')
         # from the trend line it is shown that historical December vs January stock prices of ProcterGamble follows the same trend all over.
         # Based on historical trend line of Stoke Price for December vs January of all companies shown above there is
         # a clear over-arching trend for the months of December vs  January across`` all
         
#*************************************************************************************************************************         