/* 

Y = log family income 1980 ===== FAMINC
D = 1 if family has 3 or more children, D = 0 ow ==== KIDCOUNT
Z = 1 if mother's oldest two children are same sex ==== SEX2ND ASEX2ND
X =  
	race (i assume this is race of mother) ==== RACEM
	mother's current age ==== AGEM
	age of mom at time of birth of first child == YOBM YOBK (year of birth mom, year of birth, first born)
	dummy for sex of oldest child ==== SEXK (sex, first born 0=male) or ASEX (sex of oldest allocated)

*/ 


* Load 1980 data 
import spss using "/Users/anyamarchenko/CEGA Dropbox/Anya Marchenko/metrics_pset2_fall2023_data/m_d_806.sav", clear

keep FAMINC KIDCOUNT RACEM AGEM SEXK SEX2ND YOBM YOBK

rename *, lower 

destring faminc, replace force
destring racem, replace force
destring agem, replace force
destring sexk, replace force
destring sex2nd, replace force

* Gen age of mom at time of birth of first child
gen agemomfirst = yobk - yobm 

* Gen log family income
gen lnincome = ln(faminc)

* Gen treatment D
gen d = 0
replace d = 1 if kidcount > 2 //D = 1 if 3 or more children, D = 0 ow

* Gen IV Z
drop if sex2nd == . 
gen z = 0  //if there is a second kid
replace z = 1 if sex2nd == sexk

export delimited using "/Users/anyamarchenko/CEGA Dropbox/Anya Marchenko/metrics_pset2_fall2023_data/anya_cleaned.csv", replace


