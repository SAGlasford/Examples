get_zero_coupon<-function(coupons=c(5,5.5,5,6),BondPrices=c(101,101.5,99,100),nominal_value=100){
  
  #We assume both coupons and BondPrices vectors are arranged to 1 year increasing maturity.
  price_matrix <- matrix(0,nrow=length(coupons),ncol=length(coupons))
  
  #Assign the coupons for each year
  for(i in 1:length(coupons)){
    price_matrix[i,1:i] <- coupons[i]
  }
  
  #Add the maturity nominal value
  diag(price_matrix) <- diag(price_matrix) + nominal_value
  
  #Solve the system of equations to get B(0,t)
  zero_coupon_prices<-solve(price_matrix,BondPrices)
  
  #Get zero coupon yields R(0,t)
  zero_coupon_yields <- (1/zero_coupon_prices)^(1/1:length(coupons))-1
  
  return(list(B0t=zero_coupon_prices,R0t=zero_coupon_yields))
}

get_zero_coupon()