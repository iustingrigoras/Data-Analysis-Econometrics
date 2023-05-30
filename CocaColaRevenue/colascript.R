attach(Coca_Cola_Co)
regresie2 <- lm(NetOperatingRevenues~CostOfGoodsSold+TreasuryStockCost+DepreciationAndAmortization+PaymentsOfDebt+OtherOperatingCharges+`Purchases of investments`)
summary(regresie2) #regresia initiala care contine variabila dependenta NetOperatingRevenues,adica veniturile nete
#si variabilele independente sau explicative pentru variabila explicata: costul bunurilor vandute, costul trezoreriei,
#deprecierea si amortizarea, plata indatoririlor, alte costuri de operare si cumpararea investitiilor
step(regresie2) #functia step are rolul de a elimina estimatorii care nu sunt semnificativi intr-un model 
#si ne da regresia cea mai buna,
#care contine variabila explicata: NetOperatingRevenues si cele trei variabile explicative: CostOfGoodsSold,
#TreasuryStockCost si OtherOperatingCharges
regresie_buna <- lm(NetOperatingRevenues~CostOfGoodsSold+TreasuryStockCost+OtherOperatingCharges)
summary(regresie_buna)
  
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -370.72 -223.65   18.51  150.91  408.07 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           8.553e+03  7.935e+02  10.778 3.77e-05 ***
#   CostOfGoodsSold       2.168e+00  3.621e-02  59.854 1.46e-09 ***
#   TreasuryStockCost     5.257e-02  2.000e-02   2.628   0.0392 *  
#   OtherOperatingCharges 6.328e-01  5.268e-01   1.201   0.2749    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 333.3 on 6 degrees of freedom
# Multiple R-squared:  0.9983,	Adjusted R-squared:  0.9975 
# F-statistic:  1197 on 3 and 6 DF,  p-value: 1.016e-08

#Interpretare regresie: 
#testul f indica ca testul este valabil sub 0.05,
#cei 3 estimatori sunt puternic semnificativ diferiti de 0
#testul r-patrat = 0.9975 => aproximativ 99.75% din variatia Veniturilor nete ar putea fi explicata de model
#p-value = 1.016e-08 => respingerea ipotezei nule conform careia nu exista o relatie intre variabilele independente
#si cea dependenta. O valoare mica a p indica faptul ca coeficientii modelului de regresie sunt semnificativ 
#diferiti de zero, sustinand concluzia ca variabilele independente au un impact statistic semnificativ asupra variabilei dependente.

#Metoda VIF (Variance Influence Error)
vif(regresie_buna)

#CostOfGoodsSold     TreasuryStockCost OtherOperatingCharges 
#1.005798              2.912368              2.907632
#valori sub 5 => nu exista multicoloniaritate
#se poate spune ca se masoara de cate ori este supraevaluata varianta coeficientilor

#vizualizare vif
vif_values <- vif(regresie_buna)

barplot(vif_values, main = "VIF Values", col= "green", ylim= c(0.0,8.0))

bad_vif <- 5.0
abline(h= bad_vif, col= "red")

#Cu cat diferenta intre coeficientul de determinare si coeficientul de determinare ajustat
#este mai redusa, cu atat variabila sau variabilele adaugate aduc o informatie
#mai de calitate

#testarea autocorelatiei cu testul Darwin-Watson
library(lmtest)
dwtest(regresie_buna)  
#p -value: 0.2796 mai mare decat 0.05 => se accepta ipoteza nula in care erorile nu prezinta autocorelare de ordin 1

#testarea heteroscedasticitatii cu testul Breusch-Pagan
bptest(regresie_buna)
#p-value: 0.5517 => se accepta ipoteza nula nu prezinta heteroscedasticitate adica au dispersiile constante

#testarea normalitatii distributiei erorilor cu testul Jarque Berra
library(tseries)
names <- summary(regresie_buna)
rez <- summary(regresie_buna)$residuals
jarque.bera.test(rez)
#p-value: 0.7511 > 0.05 rezulta ca erorile sunt normal distribuite

#realizarea unei prognoze
regresie_prognoza <- lm (NetOperatingRevenues~CostOfGoodsSold, data=Coca_Cola_Co)
summary(regresie_prognoza)
predict(regresie_prognoza) #functia predict realizeaza prognoza

#Interpretare prognoza: 
#Prognoza rezultata din regresie indica valorile estimate pentru variabila dependenta 
#pe baza valorilor cunoscute ale variabilei independente (CostOfGoodsSold).
#Rezultatele regresiei indica faptul ca variabila independenta, CostOfGoodsSold, are o influenta semnificativa asupra variabilei 
#dependente. Coeficientul asociat cu CostOfGoodsSold este estimat la 2.161, cu o eroare standard de 0.05011. 
#Acest coeficient ne sugerează ca, in medie, pentru fiecare unitate de crestere a CostOfGoodsSold, 
#variabila dependenta are o crestere de aproximativ 2.161.
