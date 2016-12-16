clear all
cls
set more off

****** 変更点
use "/home/4059944817/Downloads/AEJApp_2008_0146_Data/SLMSMaster.dta"


*cd c:\stata\data\srilanka\
*use data\SLMSMaster.dta, clear
*use data\Master11roundsClean.dta, clear
*This data set contains only indirectly affected and unaffected, 11 rounds
drop if ets==1 | ets==.
*Keep only enterprises where the gender of the owner is clearly identifed and the identification of the owner does not change
drop if femaleverified==.

sort sheno wave
merge sheno wave using "/home/4059944817/Downloads/AEJApp_2008_0146_Data/int_pres.dta"
drop _merge

*---Trimming points---*
tsset sheno wave
gen abschange=prof-L.prof
gen perchange=100*(prof-L.prof)/L.prof

*cuts for top 1% (about one observation per wave, on average)
egen xtreme_high=pctile(perchange), p(99)
egen xtreme_low=pctile(perchange), p(1)
egen xtreme_high_abs=pctile(abschange), p(99)
egen xtreme_low_abs=pctile(abschange), p(1)

sum perchange, de

egen minchange=min(perchange), by(sheno)
egen maxchange=max(perchange), by(sheno)

*sample 2 trims only the upper 1% in both percentage and absolute changes in profits 
gen sample2=1 if waves>=3 & ets>1 &  (perchange <=xtreme_high | perchange==.) & (abschange<=xtreme_high_abs  | abschange==.) 

***********************
*replace variables from original dataset that have been altered by profit corrections
drop logprof
gen logprof=log(realprof)
*For Appendix Table A-2
replace amount=.25 if amount==0 & wave>=6
replace amount_femaleverified=.25 if amount_femaleverified==0 & wave>=6

*log using \Applications\stata\logs\srl\genderDec2008_AEJApp, replace

gen equip100=equipamount==1
gen equip200=equipamount==2
recode cashamount .25=0
gen cash100=cashamount==1
gen cash200=cashamount==2

gen lK2_nolandnew=log(K2_nolandnew)

*BASIC REGRESSION
xtreg realprof amount wave2-wave9  wave10 wave11 if sample2==1, i(sheno) fe cluster(sheno)

*BASIC GENDER IMPACTS
*---gender interactions---*
for num 2/11: gen femaleverified_waveX=femaleverified*waveX
gen K_noland_femaleverified=K2_noland*femaleverified

xtreg realprof amount amount_female wave2-wave9 wave10 wave11  femaleverified_wave2-femaleverified_wave11 if ets>1 & sample2==1, fe i(sheno) cluster (sheno)
lincom amount + amount_female 

*With no trimming
xtreg realprof amount amount_female wave2-wave9 wave10 wave11  femaleverified_wave2-femaleverified_wave11 if ets>1 & waves>=3, fe i(sheno) cluster (sheno)
*In logs, w/ trimming
xtreg logprof amount amount_female wave2-wave9 wave10 wave11  femaleverified_wave2-femaleverified_wave11 if ets>1 & sample2==1, fe i(sheno) cluster (sheno)

**************************problem

gen probamount = 100*amount
gen probamount_female = 100*amount_female

*(A),(B)

*ALL
xtreg realprof probamount probamount_female wave2-wave9 wave10 wave11  femaleverified_wave2-femaleverified_wave11 if ets>1 & sample2==1, fe i(sheno) cluster (sheno)
eststo ALLsample


*10000 LKR
xtreg realprof probamount probamount_female wave2-wave9 wave10 wave11  femaleverified_wave2-femaleverified_wave11 if ets>1 & sample2==1 & cashamount==1, fe i(sheno) cluster (sheno)
eststo Cash_10000

*20000 LKR
xtreg realprof probamount probamount_female wave2-wave9 wave10 wave11  femaleverified_wave2-femaleverified_wave11 if ets>1 & sample2==1 & cashamount==2, fe i(sheno) cluster (sheno)
eststo Cash_20000

*NONE
xtreg realprof probamount probamount_female wave2-wave9 wave10 wave11  femaleverified_wave2-femaleverified_wave11 if ets>1 & sample2==1 & cashamount==0, fe i(sheno) cluster (sheno)
eststo Cash_none

esttab using result.tex, b(3) keep(probamount probamount_female) replace
esttab using resultall.tex, b(3) replace

*(c)


*HAUSMAN

xtreg realprof probamount probamount_female wave2-wave9 wave10 wave11  femaleverified_wave2-femaleverified_wave11 if ets>1 & sample2==1, fe
estimates store fixed

xtreg realprof probamount probamount_female wave2-wave9 wave10 wave11  femaleverified_wave2-femaleverified_wave11 if ets>1 & sample2==1, re
estimates store random
hausman fixed random, sigmamore

