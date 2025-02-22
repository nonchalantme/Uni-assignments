* ===== Problem 1 =========
clear

*** import the dataset
cd "C:\Users\HP\Downloads\"

use UK_Election.dta

* We need to keep the columns that will be needed for OLS and 2SLS
keep b01 Q24_CSES Q23_CSES edlevel y01_Annual Constit_Code

* Drop the null values
foreach var of varlist _all {
    drop if missing(`var')
}


describe
summarize
* a) Run linear regression
regress b01 Q24_CSES Q23_CSES edlevel y01_Annual, vce(robust)

* beta_4 = -0.000975, it means that for every additional unit increase in gross annual household income, the likelihood of voting decreases by 0.0975%, ceteris paribus. 

* every p-value is greater than 0.05, so effects are not statistically significant at 5%

*The R-squared is 0.0010, indicating that only 0.1% of the variation in b01 is explained by the model. This suggests a poor fit.

* The overall F-statistic is 0.82, with a p-value of 0.5109, meaning that the model as a whole is not statistically significant.


* b1) H0: beta_1 = beta_2 = 0
test Q24_CSES Q23_CSES

* p-value = 0.594 > 5%, we fail to reject the null hypothesis. In conclusion, there is no enough evidence to reject that the effect of the gender and age are jointly equal to zeros. 


* b2) LM test: H0: beta_1 = beta_2 = 0
* To perform a LM test for heteroskedasticity, we need to regress the squared residuals from the original regression on the independent variables.

* Step 1: Regress the primary model
regress b01 edlevel y01_Annual

* Step2: Obtain the residuals
predict utilde, residuals

* Step 3: Generate squared residuals
gen utilde2 = utilde^2

* Step 4: Regress squared residuals on independent variables
regress utilde2 Q24_CSES Q23_CSES edlevel y01_Annual

* Step 5: Calculate LM stat
gen LM_stat = e(N)*e(r2)

gen cv = invchi2(2,0.95) /*inverse of cdf from chi-squared distribution with 2 df evaluated at 95% */
gen pval = chi2tail(2,LM_stat)

* Step 6: Diplay results
disp "LM test statistic = " LM_stat
disp "critical value (5%) = " cv
disp "p-value = " pval

* Since LM test 1.1957 < 5.9915, we fail  to reject the null hypothesis at the 5% significance level.
* Since p-value 0.5499 > 0.05, we again fail to reject the null hypothesis.

* c) 
* Since Constit_Code is a str variable, we need to convert it into numeric value in order to perform regression and 2SLS.

encode Constit_Code, gen(Constit_Code_num)
summarize Constit_Code_num 

* We need to run a first stage regression where income is regressed on Constit_Code_num and other covariates in order to check whether the constituence is a valid instrument.

regress y01_Annual Constit_Code_num Q24_CSES Q23_CSES edlevel
* p-value is 0.477, it indicates that constituency is not statistically significant at a 5% significance level.
* F-statistic is 3.21 with a p-value of 0.0123, showing that the overall model is statistically significant. 
* However R squared is very low, meaning the model explains very little of the variation in income.
* In conclusion, constituency doesn't appear to be relevant as an instrument for income, since it isn't strongly correlated with y01_Annual.

*  Re-estimating the regression using 2SLS

ivregress 2sls b01 Q24_CSES Q23_CSES edlevel (y01_Annual = Constit_Code_num), vce(robust)

* The 2SLS estimate suggests a positive relationship between income and voting likelihood compared to OLS, but it is also not statistically significant.
* OLS estimates a small negative effect of income, while 2SLS estimates a positive and much larger effect. As suggested earlier, the instrument is weak.

estat firststage


************************* Problem 2 ************************
clear

cd "C:\Users\HP\Downloads"
use UK_Election.dta

summarize

************************************ a)
regress b01 Q24_CSES Q23_CSES edlevel y01_Annual, vce(robust)
predict b01_hat, xb
summarize b01_hat

*  Apply the logistic transformation to force predictions between 0 and 1
gen b01_hat_corrected = exp(b01_hat) / (1 + exp(b01_hat))
summarize b01_hat_corrected


***     nonlinear least squares
matrix B = [0,0,0,0,0] /*initial values */
nl (b01_hat_corrected = (exp({b0} + {b1}*Q24_CSES + {b2}*Q23_CSES + {b3}*edlevel + {b4}*y01_Annual))/(1 + exp({b0} + {b1}*Q24_CSES + {b2}*Q23_CSES + {b3}*edlevel + {b4}*y01_Annual))), initial(B) variables(Q24_CSES Q23_CSES edlevel y01_Annual) vce(robust)

*Here, NLS model is more appropriate than a linear OLS model. The NLS model explicitly uses the logistic functional form to capture the nonlinear relationship between predictors and the probability of the outcome. In contrast, OLS assumes a linear relationship, which might not be valid for probabilities, leading to biased estimates and poor fit to the data.
* The results show that the NLS model achieves an R-squared of 1, indicating a perfect fit to the transformed data. 
* OLS assumes homoscedasticity. However, for probability models, errors are typically heteroscedastic, violating OLS assumptions. Logistic-based NLS models are designed to handle the heteroscedasticity in binary/probability outcomes.
eststo md1

*matrix B = [0,0,0,0,0,0]
*nl (b02 = (exp({b0} + {b1}*Q24_CSES + {b2}*Q23_CSES + {b3}*edlevel + {b4}*y01_Annual + {b5} * p03b))/(1 + exp({b0} + {b1}*Q24_CSES + {b2}*Q23_CSES + {b3}*edlevel + {b4}*y01_Annual + {b5} * p03b))), initial(B) variables(Q24_CSES Q23_CSES edlevel y01_Annual p03b) vce(robust)

*regress b02 Q24_CSES Q23_CSES edlevel y01_Annual p03b, vce(robust)
*predict b02_hat, xb
*summarize b02_hat

*gen b02_hat_corrected = exp(b02_hat) / (1 + exp(b02_hat))
*summarize b02_hat_corrected

tabulate b02 

************************************ b) 
* i) 30-year-old, female, average annual household income between £36.400 - £46.799, holding a university degree.

* Generate ycons for Conservative Party voters
gen ycons = 0
replace ycons = 1 if b02 == 2

* Generate ylabour for Labour Party voters
gen ylabour = 0
replace ylabour = 1 if b02 == 1

tabulate ycons ylabour

* Checking all values in ylabour and ycons are between 0 and 1.
summarize ycons ylabour


* 1. Generate dummy for gender (female = 1, male = 0)
gen female = 0
replace female = 1 if Q23_CSES == 2  // 2 = female, in a given questionnaire

* 2. Generate dummy for university degree
gen university_degree = 0
replace university_degree = 1 if edlevel == 4 | edlevel == 5  // indicated in a given questionnaire

* 3. Generate dummy for the income range £36,400–£46,799
gen income_range = 0
replace income_range = 5 if y01_Annual == 5 // indicated in a given questionnaire

* 4. Generate dummy for favoring a second EU referendum
gen second_referendum = 0
replace second_referendum = 1 if p03b == 1  

* 5. Filter voters only (based on b01)
tab b01
drop if b01 == 2  // Remove non-voters, b01 = 2 indicates non-voting

*** We have decided to estimate it with NLS and Logit methods simultaneously in order to compare the results. 

matrix B = [0,0,0,0,0,0]
nl (ycons = (exp({b0} + {b1}*Q24_CSES + {b2}*female + {b3}*university_degree + {b4}*income_range + {b5}*second_referendum))/(1 + exp({b0} + {b1}*Q24_CSES + {b2}*female + {b3}*university_degree + {b4}*income_range + {b5}*second_referendum))), initial(B) variables(Q24_CSES female university_degree income_range second_referendum) vce(robust)

logit ycons Q24_CSES female income_range university_degree second_referendum
* Supporting a second EU referendum significantly reduces the likelihood of voting for the Conservative Party.
* Being female decreases the likelihood of voting Conservative compared to males.
* Each additional year of age slightly increases the likelihood of voting Conservative. But effect is not statistically significant, since p-value = 0.434.
* Being in the specified income range (£36,400–£46,799) increases the likelihood of voting Conservative. But it is not statistically significant because p-value = 0.223.
* Having a university degree decreases the likelihood of voting Conservative.
* All variables have a significant effect but age and income.

*** The two models produce broadly consistent results. The statistical significance of effects remains mostly the same. Although NLS operates on probabilities directly, making its coefficients easier to interpret for predicted probabilities, we decided to use logit. Since logit is specifically designed for binary outcomes like ycons (voted Conservative or not). Hence (https://en.wikipedia.org/wiki/Logistic_regression), Logit models work on the log-odds scale and are more compatible with binary data as they naturally handle the constrained range of probabilities (0 to 1) through the logistic function.

logit ylabour Q24_CSES female income_range university_degree second_referendum
* Supporting a second EU referendum significantly increases the likelihood of voting for the Labour Party.
* Being female increases the likelihood of voting Labour compared to males, but it is not statistically significant.
* Each additional year of age slightly decreases the likelihood of voting Labour. But effect is not statistically significant, since p-value = 0.125.
* Being in the specified income range (£36,400–£46,799) decreases the likelihood of voting Labour. But it is not statistically significant because p-value = 0.965.
* Having a university degree increases the likelihood of voting Labour. But not statistically significant
* Support for a second referendum has the strongest and most significant positive effect on voting for the Labour Party.


margins, at(second_referendum=(0 1) female=1 Q24_CSES=30 income_range=1 university_degree=1) predict()
* Predicted probabilities of voting for the Labour Party (or against Conservative Party and other parties) under two different scenarios.
* 30 year old females who do not support for a second EU referendum (_at = 1), votes for the Labour party with the probability of 22.62%. While second referundum supporting females (_at = 2) have a likelihood of voting for the same party is about 51.67%.
* Also, both probabilities have p-values < 0.05, meaning they are statistically significant.



* ii) 40-year-old, male, average annual household income between £26.000 - £36.399, primary education.

margins, at(second_referendum=(0 1) female=0 Q24_CSES=40 income_range=4 university_degree=0) predict()

* In this category, there is a 16.28% chance they would vote for the Labour Party if they oppose a second EU referendum, while there is a 41.56% chance they would vote for the Labour Party, if they support a referendum.

* In counclusion, while both categories experience a substantial increase in the probability of voting Labour when supporting a second referendum, the magnitude of this effect is slightly greater for the younger, more educated, and higher-income profile (females). This highlights how demographic and socioeconomic factors interact with political preferences.