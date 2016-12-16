clear all

set seed 100
set obs 1000

gen x= rnormal()
gen epsilon=rnormal()

gen y_star =2+x+epsilon
gen y=2+x+epsilon
replace y=0 if y_star<=0

*(c)
reg y x if y>0

*(d)
tobit y x,ll
