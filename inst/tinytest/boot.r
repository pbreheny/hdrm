Data <- gen_data_abn(n=100, p=20, a=2, b=3)
CI <- boot.glmnet(Data$X, Data$y)
covered <- Data$beta >= CI[,1] & Data$beta <= CI[,2]
table(covered)
