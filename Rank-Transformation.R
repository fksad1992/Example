WHOM = HOM_total
WHOM[, 3:6] = winsor(HOM_total[, 3:6], trim = 0.01)
WHOM = WHOM[complete.cases(WHOM), ]
WHOM$Code = substr(rownames(WHOM), 1, 6)
t = inner_join(winsorize_CSRpanel, WHOM, by = c('Code', 'Year'))
colnames(winsorize_CSRpanel)[24] = 'Year'
winsorize_CSRpanel$Code = substr(winsorize_CSRpanel$Code, 1, 6)
write.table(t, 'Winsorize_Crash_CSRpanle.txt')
write.table(WHOM, 'Winsorize_Crash.txt')
pt %>% group_by(Year, Ind) %>% mutate(rcsr = rxx(csr), rx1 = rxx(x1), rx2 = rxx(x2), rx3 = rxx(x3), rx4 = rxx(x4), rNC = rxx(NCSKEW), rDU = rxx(DUVOL)) ->rt
rxx = function(x){b = (x - min(x)) / diff(range(x))}
summary(rt$rcsr)
prt = pdata.frame(rt, index = c('Code', 'Year'))
