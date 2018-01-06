library(magrittr)
library(data.table)
library(xts)
library(quadprog)

# Load stock data"
link = "https://raw.githubusercontent.com/chenbowen184/Investment---Equity-Analysis/master/"
INTC = fread(paste(link,"INTC.csv", sep=""))
MSFT = fread(paste(link,"MSFT.csv", sep=""))
JNJ = fread(paste(link,"JNJ.csv", sep=""))
LUV = fread(paste(link,"LUV.csv", sep=""))
MCD = fread(paste(link,"MCD.csv", sep=""))
FF = fread(paste(link,"F-F_Research_Data_Factors_daily.csv", sep="")
           ,skip=4L)

# change FF date column to date format
FF$Date = as.Date(as.character(FF$V1), "%Y%m%d")

# Use only Intel and Micrsoft

# Find Intel's weekly return
INTC[, Date := as.Date(as.character(Date))]
ep  = endpoints(INTC[["Date"]], on = "weeks")
INTC_weekly =  INTC[ep,]
INTC_weekly[, return := `Adj Close`/ shift(`Adj Close`) - 1]
INTC_weekly = INTC_weekly[complete.cases(INTC_weekly)]

# Find Microsoft's weekly return
MSFT[, Date := as.Date(as.character(Date))]
ep  = endpoints(INTC[["Date"]], on = "weeks")
MSFT_weekly =  MSFT[ep,]
MSFT_weekly[, return := `Adj Close`/ shift(`Adj Close`) - 1]
MSFT_weekly = MSFT_weekly[complete.cases(MSFT_weekly)]


# Find Intel's annual expected return
n = length(INTC_weekly$Date) 
INTC_expected_return = prod(1+INTC_weekly$return)^(52/n) - 1

# Find MSFT's annual expected return
n = length(MSFT_weekly$Date)
MSFT_expected_return = prod(1+MSFT_weekly$return)^(52/n) -1

# Find Intel and Microsoft's Covariance
Q2 = cov(cbind(INTC_weekly$return,MSFT_weekly$return)) * 52 
Q2 = round(Q2, 6)

# build a vector of weights for Intel and Microsoft
w_intel = seq(-2,2,0.001)
w_microsoft = 1 - w_intel
portfolio2 = data.table(cbind(w_intel,w_microsoft))

# Find the portfolio return
W2 = as.matrix(cbind(w_intel, w_microsoft))
R2 = as.matrix(cbind(INTC_expected_return,MSFT_expected_return ))
portfolio2[, portfolio_return:= W2%*%t(R2)]

# Find portfolio variance
portfolio2[, portfolio_volatility:= apply(W2, 1, function(W2) sqrt(t(W2)%*%Q2%*%W2))]
# Find minimum variance portfolio
min_var2 = portfolio2[portfolio_volatility == min(portfolio_volatility),]

# Efficient frontier
efficient_frontier2 = portfolio2[portfolio_return >= min_var2$portfolio_return,]

# Use all of the 5 assets to form a portfolio

# Find Johnson & Johnson's weekly return
JNJ[, Date := as.Date(as.character(Date))]
ep  = endpoints(JNJ[["Date"]], on = "weeks")
JNJ_weekly =  JNJ[ep,]
JNJ_weekly[, return := `Adj Close`/ shift(`Adj Close`) - 1]
JNJ_weekly = JNJ_weekly[complete.cases(JNJ_weekly)]

# Find Southwest Airline's weekly return
LUV[, Date := as.Date(as.character(Date))]
ep  = endpoints(LUV[["Date"]], on = "weeks")
LUV_weekly =  LUV[ep,]
LUV_weekly[, return := `Adj Close`/ shift(`Adj Close`) - 1]
LUV_weekly = LUV_weekly[complete.cases(LUV_weekly)]

# Find McDonald's weekly return
MCD[, Date := as.Date(as.character(Date))]
ep  = endpoints(MCD[["Date"]], on = "weeks")
MCD_weekly =  MCD[ep,]
MCD_weekly[, return := `Adj Close`/ shift(`Adj Close`) - 1]
MCD_weekly = MCD_weekly[complete.cases(MCD_weekly)]

# Find Johnson & Johnson's annual expected return
n = length(JNJ_weekly$Date) 
JNJ_expected_return = prod(1+JNJ_weekly$return)^(52/n) - 1

# Find Southwest's annual expected return
n = length(LUV_weekly$Date)
LUV_expected_return = prod(1+LUV_weekly$return)^(52/n) -1

# Find McDonald's annual expected return
n = length(MCD_weekly$Date)
MCD_expected_return = prod(1+MCD_weekly$return)^(52/n) -1

# Find Covariance for 5 assets
Q5 = cov(cbind(INTC_weekly$return,
               MSFT_weekly$return,
               JNJ_weekly$return,
               LUV_weekly$return,
               MCD_weekly$return
             )) * 52

Q5 = round(Q5,6)
R5 = as.matrix(cbind(INTC_expected_return,
                                  MSFT_expected_return,
                                  JNJ_expected_return,
                                  LUV_expected_return,
                                  MCD_expected_return))

# Find global efficient frontier weights
target_return = as.matrix(seq(0,0.32, 0.0001))
num_assets = 5
W5 = t(sapply(target_return, 
              function(target_return) {
                    # build equation set in Constraints
                    i = as.matrix(rep(1, num_assets))
                    Constraints = rbind(t(i), R5)
                    # Use QP to solve for minimum variance weights
                    optimal_weights = solve.QP(2*Q5, 
                                      rep(0, num_assets), 
                                      t(Constraints), 
                                      c(1, target_return), 
                                      meq = 2)$solution
                    }
              )
       )
# build global efficient frontier
portfolio5 = data.table(cbind(target_return))%>%setNames("portfolio_return")
# Add frontier weights
portfolio5[, c("w_INTC","w_MSFT", "w_JNJ", "w_LUV", "w_MCD") := data.table(W5)]

# Calculate variance
portfolio5[,portfolio_volatility := apply(W5, 1, function(W5) sqrt(t(W5)%*%Q5%*%W5))]

# Find minimum variance portfolio
min_var5 = portfolio5[portfolio_volatility == min(portfolio_volatility),]

# Efficient frontier
efficient_frontier5 = portfolio5[portfolio_return >= min_var5$portfolio_return,]

# Plot all portfolios for 5 assets
matplot(portfolio5$portfolio_volatility, 
        portfolio5$portfolio_return,
        xlim = c(0.1,0.4),
        ylim = c(0, 0.25),
        xlab = "Portfolio Volatility",
        ylab = "Portfolio Return",
        type = "l",
        col = "red",
        main = "Portfolio Frontiers - 2 Assets vs. 5 Assets"
)

#Plot efficient frontier
matplot(efficient_frontier5$portfolio_volatility, 
        efficient_frontier5$portfolio_return,
        type = "l",
        col = "blue",
        add = T)

# Plot 2-asset portfolios
matplot(portfolio2$portfolio_volatility, 
        portfolio2$portfolio_return,
        type = "l",
        col = "red",
        add = T)

# Plot efficient frontier
matplot(efficient_frontier2$portfolio_volatility, 
        efficient_frontier2$portfolio_return,
        type = "l",
        col = "blue",
        add = T)

# Plot Intel, Microsoft, Johnson & Johnson, Southwest and McDonald's

matplot(c(sqrt(Q5[1,1]),
          sqrt(Q5[2,2]),
          sqrt(Q5[3,3]),
          sqrt(Q5[4,4]),
          sqrt(Q5[5,5])),
        
        c(INTC_expected_return,
          MSFT_expected_return,
          JNJ_expected_return,
          LUV_expected_return,
          MCD_expected_return),
        
        type = "p",
        pch = 20,
        add = T)


text(c(sqrt(Q5[1,1]),
          sqrt(Q5[2,2]),
          sqrt(Q5[3,3]),
          sqrt(Q5[4,4]),
          sqrt(Q5[5,5])),
     
          c(INTC_expected_return + 0.01,
            MSFT_expected_return - 0.01,
            JNJ_expected_return + 0.01,
            LUV_expected_return - 0.01,
            MCD_expected_return - 0.01 ),
     
          labels=c("Intel",
                   "Microsoft", 
                   "Johnson & Johnson",
                   "Southwest",
                   "McDonald's"),
          add = T)

# Minimum variance portfolio for Intel - Microsoft
matplot(min_var2$portfolio_volatility,
        min_var2$portfolio_return,
        type = "p",
        pch = 1,
        add = T)

text( min_var2$portfolio_volatility,
      min_var2$portfolio_return - 0.01,
      labels= "minimum variance portfolio",
      add = T)

# Minimum variance portfolio for 5 assets
matplot(min_var5$portfolio_volatility,
        min_var5$portfolio_return,
        type = "p",
        pch = 1,
        add = T)

text( min_var5$portfolio_volatility,
      min_var5$portfolio_return - 0.01,
      labels= "minimum variance portfolio",
      add = T)

# Add risk-free asset to the Intel- Microsoft Portfolio

# Adjust risk-free rate to weekly basis
ep  = endpoints(FF[["Date"]], on = "weeks")
FF_weekly = FF[ep, c("Date", "RF")]
FF_weekly[,RF_weekly:= (1+RF/12)^(1/4) -1]

# Find expected risk free rate by taking the geometric mean for risk-free rate
n = length(FF_weekly$RF_weekly)
Rf = prod(1+FF_weekly$RF_weekly)^(52/n) -1

# Form a portfolio with Intel, Microsoft and risk free rate asset
# Use QP to solve for minimum variance weights
Constraints =  t(R2) - Rf
QP = solve.QP(2*Q2, rep(0,2), Constraints, 1, meq = 1)$solution

# Scale to get tangency portfolio weights - the weights that set rf to 0
tangent_weights2 = as.matrix(QP/sum(QP))

# Find market portfolio return/variance
market_portfolio_return2 = R2%*%tangent_weights2
market_portfolio_volatility2 = sqrt(t(tangent_weights2)%*%Q2%*%tangent_weights2)
  
# Find tangent portfolio return/variance
tangent2 = data.table(w_Rf = seq(-0.2, 1, 0.001),
                      w_maket = 1-seq(-0.2, 1, 0.001))

W_tangent2 = as.matrix(tangent2[,c(1,2)])
R_tangent2 = as.matrix(cbind(Rf,market_portfolio_return2))

tangent2[,`:=`(portfolio_return = w_maket*as.vector(market_portfolio_return2) +
                                                    w_Rf* as.vector(Rf),
               portfolio_volatility = w_maket * as.vector(market_portfolio_volatility2)                                 
              )]

# Compute sharpe ratio
SR2 = (market_portfolio_return2-Rf)/market_portfolio_volatility2

# Plot tangency portfolio, risky portfolio with Intel and Microsoft

# Plot Intel - Microsoft portfolio with risk-free asset
matplot(tangent2$portfolio_volatility, 
        tangent2$portfolio_return,
        xlim = c(0.00191,0.4),
        xlab = "Portfolio Volitality",
        ylab = "Portfolio Return",
        type = "l",
        main = "Tangency Portfolio for Intel - Microsoft",
        col = "dark blue")

# Plot 2-asset portfolios
matplot(portfolio2$portfolio_volatility, 
        portfolio2$portfolio_return,
        type = "l",
        col = "red",
        add = T)

# Plot tangenct portfolio,Rf 
matplot(c(market_portfolio_volatility2, 0),
        c(market_portfolio_return2, Rf),
        type = "p",
        pch = 20,
        col = "red",
        add = T)

text(c(market_portfolio_volatility2-0.02, 0.003),
     c(market_portfolio_return2-0.005, Rf),
     labels=c("Market Portfolio", "Risk free"),
     add = T)


matplot(c(sqrt(Q5[1,1]),
          sqrt(Q5[2,2]),
          sqrt(Q5[3,3]),
          sqrt(Q5[4,4]),
          sqrt(Q5[5,5])),
        
        c(INTC_expected_return,
          MSFT_expected_return,
          JNJ_expected_return,
          LUV_expected_return,
          MCD_expected_return),
        
        type = "p",
        pch = 20,
        add = T)


text(c(sqrt(Q5[1,1]),
       sqrt(Q5[2,2])+0.02,
       sqrt(Q5[3,3]),
       sqrt(Q5[4,4]),
       sqrt(Q5[5,5])),
     
     c(INTC_expected_return + 0.005,
       MSFT_expected_return ,
       JNJ_expected_return + 0.005,
       LUV_expected_return - 0.005,
       MCD_expected_return - 0.005),
     
     labels=c("Intel",
              "Microsoft", 
              "Johnson & Johnson",
              "Southwest",
              "McDonald's"),
     add = T)

# Find tangency portfolio for 5 asset and risk-free portfolio
# Use QP to solve for minimum variance weights
Constraints =  t(R5) - Rf
QP = solve.QP(2*Q5, rep(0,5), Constraints, 1, meq = 1)$solution

# Scale to get tangency portfolio weights - the weights that set rf to 0
tangent_weights5 = as.matrix(QP/sum(QP))

# Find market portfolio return/variance
market_portfolio_return5 = R5%*%tangent_weights5
market_portfolio_volatility5 = sqrt(t(tangent_weights5)%*%Q5%*%tangent_weights5)

# Find tangent portfolio return/variance
tangent5 = data.table(w_Rf = seq(-0.7, 1, 0.001),
                      w_maket = 1-seq(-0.7, 1, 0.001))

W_tangent5 = as.matrix(tangent5[,c(1,2)])
R_tangent5 = as.matrix(cbind(Rf,market_portfolio_return5))

tangent5[,`:=`(portfolio_return = w_maket*as.vector(market_portfolio_return5) +
                 w_Rf* as.vector(Rf),
               portfolio_volatility = w_maket * as.vector(market_portfolio_volatility5)                                 
)]

# Compute sharpe ratio
SR5 = (market_portfolio_return5-Rf)/market_portfolio_volatility5

# Plot tangency portfolio, risky portfolio with Intel and Microsoft

# Plot 5-asset portfolio with risk-free asset
matplot(tangent5$portfolio_volatility, 
        tangent5$portfolio_return,
        xlim = c(0.01,0.45),
        xlab = "Portfolio Volitality",
        ylab = "Portfolio Return",
        type = "l",
        main = "Tangency Portfolio for 5 Risky Assets",
        col = "darkblue")

# Plot 5-asset portfolios
matplot(portfolio5$portfolio_volatility, 
        portfolio5$portfolio_return,
        type = "l",
        col = "red",
        add = T)

# Plot tangenct portfolio,Rf 
matplot(c(market_portfolio_volatility5, 0),
        c(market_portfolio_return5, Rf),
        type = "p",
        pch = 20,
        col = "red",
        add = T)

text(c(market_portfolio_volatility5-0.0025, 0.003),
     c(market_portfolio_return5+0.005, Rf),
     labels=c("Market Portfolio", "Risk free"),
     add = T)


matplot(c(sqrt(Q5[1,1]),
          sqrt(Q5[2,2]),
          sqrt(Q5[3,3]),
          sqrt(Q5[4,4]),
          sqrt(Q5[5,5])),
        
        c(INTC_expected_return,
          MSFT_expected_return,
          JNJ_expected_return,
          LUV_expected_return,
          MCD_expected_return),
        
        type = "p",
        pch = 20,
        add = T)


text(c(sqrt(Q5[1,1]),
       sqrt(Q5[2,2])+0.002,
       sqrt(Q5[3,3]),
       sqrt(Q5[4,4]),
       sqrt(Q5[5,5])),
     
     c(INTC_expected_return + 0.005,
       MSFT_expected_return - 0.005,
       JNJ_expected_return + 0.005,
       LUV_expected_return - 0.005,
       MCD_expected_return - 0.005 ),
     
     labels=c("Intel",
              "Microsoft", 
              "Johnson & Johnson",
              "Southwest",
              "McDonald's"),
     add = T)

# Plot everything on the same graph

# Plot 5-asset portfolio with risk-free asset
matplot(tangent5$portfolio_volatility, 
        tangent5$portfolio_return,
        xlim = c(0.02,0.45),
        ylim = c(0,0.25),
        xlab = "Portfolio Volitality",
        ylab = "Portfolio Return",
        type = "l",
        main = "Tangency Portfolios for 5 Risky Assets Vs. Intel-Microsoft",
        col = "dark green")

# Plot 5-asset portfolios
matplot(portfolio5$portfolio_volatility, 
        portfolio5$portfolio_return,
        type = "l",
        col = "red",
        add = T)

#Plot efficient frontier
matplot(efficient_frontier5$portfolio_volatility, 
        efficient_frontier5$portfolio_return,
        type = "l",
        col = "blue",
        add = T)

# Plot tangenct portfolio,Rf 
matplot(c(market_portfolio_volatility5, 0),
        c(market_portfolio_return5, Rf),
        type = "p",
        pch = 20,
        col = "red",
        add = T)

text(c(market_portfolio_volatility5-0.0025, 0.03),
     c(market_portfolio_return5+0.005, Rf),
     labels=c("Market Portfolio (5)", "Risk free"),
     add = T)

matplot(tangent2$portfolio_volatility, 
        tangent2$portfolio_return,
        xlim = c(0.00191,0.4),
        xlab = "Portfolio Volitality",
        ylab = "Portfolio Return",
        type = "l",
        main = "Tangency Portfolio for Intel - Microsoft",
        col = "dark green",
        add  = T)

# Plot 2-asset portfolios
matplot(portfolio2$portfolio_volatility, 
        portfolio2$portfolio_return,
        type = "l",
        col = "red",
        add = T)

# Plot efficient frontier
matplot(efficient_frontier2$portfolio_volatility, 
        efficient_frontier2$portfolio_return,
        type = "l",
        col = "blue",
        add = T)


# Plot tangenct portfolio,Rf 
matplot(c(market_portfolio_volatility2, 0),
        c(market_portfolio_return2, Rf),
        type = "p",
        pch = 20,
        col = "red",
        add = T)

text(c(market_portfolio_volatility2-0.02, 0.03),
     c(market_portfolio_return2-0.005, Rf),
     labels=c("Market Portfolio (2)", "Risk free"),
     add = T)

matplot(c(sqrt(Q5[1,1]),
          sqrt(Q5[2,2]),
          sqrt(Q5[3,3]),
          sqrt(Q5[4,4]),
          sqrt(Q5[5,5])),
        
        c(INTC_expected_return,
          MSFT_expected_return,
          JNJ_expected_return,
          LUV_expected_return,
          MCD_expected_return),
        
        type = "p",
        pch = 20,
        add = T)

text(c(sqrt(Q5[1,1]),
      sqrt(Q5[2,2])+0.02,
      sqrt(Q5[3,3]),
      sqrt(Q5[4,4]),
      sqrt(Q5[5,5])),
    
    c(INTC_expected_return + 0.005,
      MSFT_expected_return ,
      JNJ_expected_return + 0.005,
      LUV_expected_return - 0.005,
      MCD_expected_return - 0.005),
    
    labels=c("Intel",
             "Microsoft", 
             "Johnson & Johnson",
             "Southwest",
             "McDonald's"),
    add = T)
# Optimal portfolio when A = 5

A = 5
optimal_portfolio = (market_portfolio_return5 - Rf)/(A*market_portfolio_volatility5^2)


