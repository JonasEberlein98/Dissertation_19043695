library(Miniconda)
nnet_fit <-
       mlp(epochs = 100, hidden_units = 1) %>%
       set_mode("classification") %>% 
       set_engine("keras", verbose = 0) %>%
       fit(bugbinary ~ ., data = dataTrain)

NNPred <- predict(nnet_fit, new_data = dataTest)
val_results <- dataTest %>%
                    bind_cols(
                                             predict(nnet_fit, new_data = dataTest),
                                             predict(nnet_fit, new_data = dataTest, type = "prob")
                                         )
val_results %>% roc_auc(truth = bugbinary, .pred_class)

CC + CCL + CCO + CI + CLC + CLLC + LDC + LLDC + LCOM5 + NL + NLE + WMC + CBO + CBOI + NII + NOI + RFC + AD + CD + CLOC + DLOC + PDA + PUA + TCD + TCLOC + DIT +NOA + NOC + NOD + NOP + LLOC + LOC + NA + NG + NLA +NLG +NLM +NLPA +NLPM +NLS+ NM +NOS +NPA +NPM +NS +TLLOC+ TLOC +TNA+ TNG+ TNLA+ TNLG+ TNLM+ TNLPA+ TNLPM+ TNLS+ TNM+ TNOS+ TNPA +TNPM+ TNS+ wmc+ dit+ noc+ cbo+rfc +lcom +ca+ ce +npm+  lcom3+ loc+ dam+ moa+ mfa + cam  + ic +cbm + amc+ max_cc+ avg_cc 
model <- neuralnet(bugbinary ~ CC + CCL + CCO + CI + CLC + CLLC + LDC + LLDC + LCOM5 + NL + NLE + WMC + CBO + CBOI + NII + NOI + RFC + AD + CD + CLOC + DLOC + PDA + PUA + TCD + TCLOC + DIT + NOA + NOC + NOD + NOP + LLOC + LOC + NG + NLA + NLG + NLM + NLPA + NLPM + NLS + NM + NOS + NPA + NPM + NS + TLLOC + TLOC + TNA + TNG + TNLA + TNLG + TNLM + TNLPA + TNLPM + TNLS + TNM + TNOS + TNPA + TNPM + TNS + wmc + dit + noc + cbo + rfc + lcom + ca + ce + npm +  lcom3 + loc + dam + moa + mfa + cam  + ic + cbm + amc + max_cc + avg_cc , data = dataTrain)

model <- neuralnet(bugbinary ~ CC + CCL + CCO + CI + CLC + CLLC + LDC + LLDC + LCOM5 + NL + NLE + WMC + CBO + CBOI + NII + NOI + RFC + AD + CD + CLOC + DLOC + PDA + PUA + TCD + TCLOC + DIT + NOA + NOC + NOD + NOP + LLOC + LOC + NG + NLA + NLG + NLM + NLPA + NLPM + NLS + NM + NOS + NPA + NPM + NS + TLLOC + TLOC + TNA + TNG + TNLA + TNLG + TNLM + TNLPA + TNLPM + TNLS + TNM + TNOS + TNPA + TNPM + TNS + wmc + dit + noc + cbo + rfc + lcom + ca + ce + npm +  lcom3 + loc + dam + moa + mfa + cam  + ic + cbm + amc + max_cc + avg_cc , data = dataTrain)
plot(model)
pred <- predict(model, newdata = dataTest)
recall(data, dataTest$bugbinary, pred)