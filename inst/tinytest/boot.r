Data <- gen_data_abn(n=100, p=20, a=2, b=3)
CI <- boot_ncvreg(Data$X, Data$y)$confidence_intervals
covered <- Data$beta >= CI[,"lower"] & Data$beta <= CI[,"upper"]
table(covered)
