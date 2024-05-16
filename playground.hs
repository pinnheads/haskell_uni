dollarRate2018 = 1.18215
dollarRate2019 = 1.3617
dollarRate = 0.98546541

price = 79

-- convert EUR to USD
usd euros = euros * dollarRate

-- convert USD to EUR
euro usds = usd usds/dollarRate

x ~== y = x - y < 10e-15

-- max number of intersections of n lines
nisect :: Integer -> Integer
nisect 0 = 0
nisect n | n > 0 = nisect (n - 1) + n - 1