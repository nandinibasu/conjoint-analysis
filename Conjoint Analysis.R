## ---------------------------------------------------------------------------
## 
## Title:       Homework 01: Conjoint Analysis
## Author(s):   Nandini Basu, Alayna Myrick, Ana Parra Vera, and Allyson Tsuji 
## Affiliation: UC Davis MSBA Program
## Description: BAX-442 – Advanced Statistics
## Date:        2020-01-21
## 
## ---------------------------------------------------------------------------


#############################################
#        Conjoint Analysis Function         #
#############################################

# Inputs: Preferences, Design matrix

#   where Preferences take the format of 8 columns containing the following (in order):
#         Profile Number, Profile, Preference, 52 inch screen, 65 inch screen, 2D = 1, Sony = 1, Price

#   and Design matrix takes the format of a matrix with:
#         6 columns (intercept, 52 in, 65 in, technology, brand, price) 
#         3 rows (own brand design, sony design, sharp design)

# Outputs: Partworth estimates, Attribute importance (%), 
#          Willingness to pay for each attribute ($), Optimal price,
#          Market share at optimal price, Maximum profit at optimal price

conjoint_func = function(prefs, designmatrix) { 
  
  ############################
  # data prep and regression #
  ############################
  # renames preferences' columns
  colnames(prefs) <- c("profilenum", "profiles", "preference", "52in", "65in", "2D3D", "sony", "price")

  # design matrix
  # designmatrix <- matrix(c(1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 2500, 2500, 2000), 3)
  
  # saves column names to reuse later
  profile_names <- c("intercept", "screen52", "screen65", "technology", "brand", "price")
  
  # renames design matrix's rows and columns
  rownames(designmatrix) <- c("own_brand_design", "sony_design", "sharp_design")
  colnames(designmatrix) <- profile_names
  print(designmatrix)
  print(" ")
  
  # regresses preference on independent variables
  Y <- prefs$preference
  X <- cbind(prefs$`52in`, prefs$`65in`, prefs$`2D3D`, prefs$sony, prefs$price)
  reg <- lm(Y~X)
  
  
  ####################
  #    partworths    #
  ####################
  regdata <- summary(reg)$coef
  colnames(regdata) <- c("estimate", "stderror", "tvalue", "pr>t")
  rownames(regdata) <- profile_names
  regdata <- data.frame(regdata)
  
  partworths <- data.frame(regdata$estimate)
  rownames(partworths) <- profile_names
  
  intercept_partworth <- partworths["intercept",]
  screen52_partworth <- partworths["screen52",]
  screen65_partworth <- partworths["screen65",]
  tech_partworth <- partworths["technology",]
  brand_partworth <- partworths["brand",]
  price_partworth <- partworths["price",]
  
  #########################
  # attribute importances #
  #########################
  # finds ranges for each feature
  screensize <- abs(max(screen52_partworth, screen65_partworth) - min(screen52_partworth, screen65_partworth))
  technology <- abs(tech_partworth - 0)
  brand <- abs(brand_partworth - 0)
  price <- abs(price_partworth - 0)
  
  # finds total range
  total_range <- screensize + technology + brand + price
  
  # attribute importances
  screen_attr_importance <- screensize / total_range * 100
  tech_attr_importance <- technology / total_range * 100
  brand_attr_importance <- brand / total_range * 100
  price_attr_importance <- price / total_range * 100

  
  ######################
  # willingness to pay #
  ######################
  
  price_savings <- abs(designmatrix["sony_design", "price"] - designmatrix["sharp_design", "price"])
  price_per_util <- price_savings/abs(price_partworth)
  
  wtp_46 <- intercept_partworth * price_per_util  # willingness to pay (WTP) for 46"
  wtp_65 <- screen65_partworth * price_per_util   # willingness to pay (WTP) for 65"
  wtp_52 <- screen52_partworth * price_per_util   # willingness to pay (WTP) for 52"
  wtp_sony <- brand_partworth * price_per_util    # willingness to pay (WTP) for Sony name
  wtp_3D <- tech_partworth * price_per_util       # willingness to pay (WTP) for 3D technology

  
  ############################################
  # utility, attractiveness and market share #
  ############################################
  partworths_vec <- t(partworths)[,1:5]    # saves into vector form and eliminates price column
  
  own_brand_design <- designmatrix["own_brand_design",1:5]
  sony <- designmatrix["sony_design",1:5]
  sharp <- designmatrix["sharp_design",1:5]
  
  costs <- c(1000, 500, 1000, 250, 250)
  net_cost <- sum(costs * own_brand_design)
  
  # utility 
  utility_own_design <- sum(own_brand_design * partworths_vec) + 
                        price_partworth * (designmatrix["own_brand_design", "price"]-2000) / price_savings
  utility_sony <- sum(sony * partworths_vec) + 
                  price_partworth * (designmatrix["sony_design", "price"]-2000) / price_savings
  utility_sharp <- sum(sharp * partworths_vec) + 
                   price_partworth * (designmatrix["sharp_design", "price"]-2000) / price_savings

  # attractiveness
  attract_own_design <- exp(utility_own_design)
  attract_sony <- exp(utility_sony)
  attract_sharp <- exp(utility_sharp)
  total_attractiveness <- attract_own_design + attract_sony + attract_sharp
  
  # market share
  mktshare_own_design <- attract_own_design / total_attractiveness * 100
  mktshare_sony <- attract_sony / total_attractiveness * 100
  mktshare_sharp <- attract_sharp / total_attractiveness * 100
  
  # margin
  price <- designmatrix["own_brand_design", "price"]
  margin_own_design <- price - net_cost
  
  # profit per TV
  profit_per_tv_own_design <- margin_own_design * mktshare_own_design / 100
  
  
  ###################################
  # prices, marketshare and profits #
  ###################################
  prices <- c()
  marketshares <- c()
  profits <- c()
  # looks through prices to find max profit
  for (price in seq(1000, 3000, 100))
  {
    # calculates utility, attractiveness and total attractiveness again based on new price
    utility <- sum(own_brand_design * partworths_vec) + price_partworth * (price-2000)/price_savings
    attract_own_design <- exp(utility)
    total_attractiveness <- attract_own_design + attract_sony + attract_sharp
    
    # calculates marketshare, margin and profit per TV based on new price
    marketshare <- attract_own_design / total_attractiveness * 100
    margin <- price - net_cost
    profit_per_tv <- margin * marketshare / 100
    
    # saves price, marketshare and profit per TV on each list
    prices <- c(prices, price)
    marketshares <- c(marketshares, marketshare)
    profits <- c(profits, profit_per_tv)
  }
  # combines results into a dataframe
  price_mkt_profit <- data.frame(price=prices, marketshare=marketshares, profit=profits)

  
  ###################
  #  optimal price  #
  ###################
  # saves optimal price along with its marketshare and max profit
  optimal_metrics <- price_mkt_profit[which.max(price_mkt_profit$profit),]
  names(optimal_metrics) <- c("optimal_price", "marketshare", "max_profit")

  ###################
  #  saving output  #
  ###################  
  partworth_names <- paste(profile_names, 'pw', sep='_')
  partworths <- as.vector(t(partworths))
  names(partworths) <- partworth_names

  willingness_to_pay <- c(wtp_46, wtp_65, wtp_52, wtp_sony, wtp_3D)
  names(willingness_to_pay) <- paste(c('screen46', profile_names[-6][2:5]), 'wtp', sep='_')
  
  attribute_importance <- c(screen_attr_importance, tech_attr_importance, brand_attr_importance, price_attr_importance)
  names(attribute_importance) <- c("screen_attr_importance", "tech_attr_importance", "brand_attr_importance", "price_attr_importance")
  
  # final output
  output <- t(c(partworths, willingness_to_pay, attribute_importance, optimal_metrics))
  return(output) 	
}



###############################################################################################

# filename <- "~/Grad School/BAX 442 Adv Stats/alliepreferences.csv"  # allie
# filename <- ""  # nandini
# filename <- ""  # alayna
filename <- "~/UCDavis/Winter2020/BAX442-AdvStats/Week-02/Class 2/Design Matrix Ana.csv"  # ana

# reads in the data
prefs <- read.csv("Nandini Preferences.csv")
head(prefs)

designmatrix <- matrix(c(1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 2500, 2500, 2000), 3)

# runs the conjoint analysis function and prints the result
conjoint_func(prefs, designmatrix)

