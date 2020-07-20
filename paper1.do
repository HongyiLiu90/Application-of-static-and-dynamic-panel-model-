
* Hongyi Liu
* Wuhan University
* 09/15/2013



use paper1.dta,clear

drop if (year<1998) | (year>2011) 
*------ generate real value -------

*1 panel data set up
xtset id year // 31 provs, 14 years


*2 get real sai based on (1998=100)
g rsai = sai/rsi


*3 real gdp which is based on Retail price index

qui{
g rpgdp = pgdp / rrpi
 }
*4 generate fiscal decentralization ratio
 qui{
 g pfd = (be/popu)/(cgs*10000/np) �
 g fd = be/(cgs*10000)
 g fd1 = bi / be 
 }
label var fd1 "预算内财政自主"
 *5 get real pi
qui g rppi = (pi*10000/popu/rsi)

*6 get real landlea_p
qui g rplandlea_p = (landlea_p /popu)/rrpi

/*(1)    *----separate the dataset into three region and 
              separate into two parts(coast and noncoast)-------*/
gen region=""
qui replace region="东部" if inlist(id,1,2,3,6,9,10,11,13,15,19,21)
qui replace region="中部" if inlist(id,4,7,8,12,14,16,17,18)
qui replace region="西部" if inlist(id,5,20,22,23,24,25,26,27,28,29,30,31)
sencode region,gen(regioncode)
gen district=""
qui replace district="沿海" if inlist(prov_num,2,3,6,9,10,11,13,15,19,21)
qui replace district="内陆" if inlist(prov_num,1,4,5,7,8,12,14,16,17,18,20,22,23,24,25,26,27,28,29,30,31)
sencode district,gen(districtcode)
order id year prov_num prov region regioncode district districtcode


 *---------data processing--------
gen rk = gcf/gdp
gen inf = rpi-100
gen rpIAV =IAV*10000/popu/rrpi
gen lnrpIAV = ln(rpIAV)
gen rpfc = fc*10000/rcpi/popu
gen lnrpfc = ln(rpfc)
gen rpgcf = gcf*10000/rrpi/popu
gen lnrpgcf =ln(rpgcf)
gen lnFAI = ln(FAI/rsi/popu)
gen lnrpgdp = ln(gdp*10000/popu/rrpi)
gen PT = bi*100/(gdp*10000) 
gen TB = tax*100/(gdp*10000)
gen IE_y = IE*ex_r
gen XM = IE_y/gdp*100
gen lnFDI = ln(FDI*10000/popu)
gen pFDI = FDI/popu
gen lnrppi = ln(rppi)
gen lnplandlea_sq = ln(landlea_sq*10000/popu)
gen lnlandlea_sq = ln(landlea_sq)
gen lnlandlea_n = ln(landlea_n)
gen lnrplandlea_p = ln(rplandlea_p)
gen llp_gr = (landlea_p - L.landlea_p)/L.landlea_p*100
gen price_land = landlea_p/ landlea_sq
gen price_land_gr = (price_land - L.price_land)/L.price_land*100
gen perlp= landlea_p/popu 
gen perlr=lnreturn/popu
gen pgdp_gr = (pgdp - L.pgdp)/L.pgdp*100
gen lnlandlea_p = ln(landlea_p/rrpi)
gen lnrpgi = ln(gi*10000/popu/rsi)
qui tab year, gen(yr)
drop yr1
gen lnprice_land =ln(price_land)
gen edu = hc*100/popu
gen FDI_ind = FDI*ex_r/gdp
gen y = fd1*L.lnrplandlea_p
gen z = fd1*L.lnlandlea_sq
gen ppi = pi*10000/popu
gen rpgi = gi*10000/popu/rsi
gen plandlea_sq = landlea_sq*10000/popu
label var edu "高中学历以上占总人口比（%）"
label var price_land "土地价格(万元/公顷)"
label var rpgdp "人均实际GDP(元)"
label var rppi "人均实际私人投资(元)"
label var rpgi "人均实际公共投资(元)"
label var fd1 "地方预算内财政自主度"
label var PT "广义宏观税负(%)"
label var plandlea_sq "人均土地出让面积(公顷/人)"
label var rplandlea_p "人均实际土地出让收入(元/人)"
label var inf "通货膨胀率(%)"
 drop if id ==26

*------------------graph------------------
twoway (scatter  perlp pgdp) (lfit  perlp pgdp) ///
, ///
 xtitle("人均GDP(元)", margin(medsmall)) ///
 ytitle("人均土地出让金(元)") ///
  ylabel(0(4000)8000) ymtick(##1) ///
  xlabel(0(20000)80000) xmtick(##3) ///
 legend(label(1 "散点") label(2 "拟合") size(*0.7)) ///
 note("资料来源：1999-2012年《中国国土资源年鉴》和《中国统计年鉴》",size(small)) ///
 caption("说明：省级数据",size(small)) scheme(s1mono) ///
 saving(paper1.gph, replace)
twoway (scatter  perlr pgdp) (lfit  perlr pgdp) ///
,  ///
 xtitle("人均GDP(元)", margin(medsmall)) ///
 ytitle("人均土地出让纯收益(元)") ///
 ylabel(0(1400)3500) ymtick(##1) ///
 xlabel(0(20000)80000) xmtick(##3) ///
 legend(label(1 "散点") label(2 "拟合") size(*0.7)) ///
 note("资料来源：2004-2009年《中国国土资源年鉴》和《中国统计年鉴》",size(small)) ///
 caption("说明：省级数据",size(small)) scheme(s1mono) ///
 saving(paper2.gph, replace)
 twoway (scatter  perlp ppi) (lfit  perlp ppi) ///
 ,  ///
 xtitle("人均私人投资(元)", margin(medsmall)) ///
 ytitle("人均土地出让金(元)") ///
  ylabel(0(4000)8000) ymtick(##1) ///
  xlabel(0(10000)60000) xmtick(##3) ///
 legend(label(1 "散点") label(2 "拟合") size(*0.7)) ///
 note("资料来源：1999-2012年《中国国土资源年鉴》和各地区历年统计年鉴",size(small)) ///
 caption("说明：省级数据",size(small)) scheme(s1mono) ///
 saving(paper3.gph, replace)
 twoway (scatter  perlr ppi) (lfit  perlr ppi) ///
,  ///
 xtitle("人均私人投资(元)", margin(medsmall)) ///
 ytitle("人均土地出让纯收益(元)") ///
 ylabel(0(1600)3200) ymtick(##1) ///
 xlabel(0(8000)40000) xmtick(##3) ///
 legend(label(1 "散点") label(2 "拟合") size(*0.7)) ///
 note("资料来源：2004-2009年《中国国土资源年鉴》和各地区历年统计年鉴",size(small)) ///
 caption("说明：省级数据",size(small)) scheme(s1mono) ///
 saving(paper4.gph, replace)
 graph combine paper1.gph paper2.gph paper3.gph paper4.gph, scheme(s1mono)
 
 
 /*reganat gdp  popu pi gi be bi landlea_p FDI hc fd1, nocovlist  
 save paper3.gph,replace
 reganat pi popu gdp gi be bi landlea_p FDI hc fd1, nocovlist  
 save paper4.gph,replace*/
 reganat rpgdp rppi rpgi FDI fd1 PT edu inf plandlea_sq rplandlea_p price_land landlea_n,nocovlist
 save paper3.gph,replace
 reganat rppi rpgdp rpgi FDI fd1 PT edu inf plandlea_sq rplandlea_p price_land landlea_n r,nocovlist
  save paper4.gph,replace


 *----------regression analysis-------------
xtdes /*------show the data satisfies the short panel data-------*/


xtsum id year gdp pi gi bi be FDI fd1 edu landlea_n landlea_sq landlea_p price_land r
logout, save(summary)  word replace fix(2): xtsum ///
 id year gdp pi gi bi be FDI fd1 inf edu landlea_n landlea_sq landlea_p price_land r
 logout, save(summary1)  word replace : sum ///
  gdp popu pi gi bi be FDI fd1 inf edu landlea_n landlea_sq landlea_p price_land r
/*data description*/
graph matrix D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf lnplandlea_sq lnrplandlea_p price_land lnlandlea_n
*--------- Estimation for choosing the best variable to describe the land finance----------


* (1) Fixed effect

xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnplandlea_sq z yr*,fe vce(cluster id)
est store gfe1
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnrplandlea_p y yr*,fe vce(cluster id)
est store gfe2
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).price_land  yr*,fe vce(cluster id)
est store gfe3
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnlandlea_n  yr*,fe vce(cluster id)
est store gfe4
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
* (2) Random effect
xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnplandlea_sq z yr*,re vce(cluster id)
est store gre1
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnrplandlea_p y yr*,re vce(cluster id)
est store gre2
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).price_land  yr*,re vce(cluster id)
est store gre3
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnlandlea_n  yr*,re vce(cluster id)
est store gre4
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
outreg2 [gfe1 gfe2 gfe3 gfe4 gre1 gre2 gre3 gre4] using result1, word replace ///
 title("表1:土地对经济增长的静态效应" )  /// // (T1)
 tdec(2) rdec(3) r2 e(F) /// // (T3)
 nonote /// // (T4)
 addnote("注：(1)***,**,*分别表示在1%,5%和10%水平上显著;", ///
 "(2)括号中为标准误;", ///
 "(3)其它注释语句。")
 
 *----------Hausman test ----------------
 qui xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnplandlea_sq z yr*,fe
 est store m1_fe
 qui xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnplandlea_sq z yr*,re 
 est store m1_re
 hausman m1_fe m1_re,sigmamore  // the result is the fixed effect model
 
 qui xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnrplandlea_p y yr*,fe
 est store m2_fe
 qui xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnrplandlea_p y yr*,re 
 est store m2_re
 hausman m2_fe m2_re,sigmamore  // the result is the fixed effect model
 
 qui xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).price_land  yr*,fe
 est store m3_fe
 qui xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).price_land  yr*,re 
 est store m3_re
 hausman m3_fe m3_re,sigmamore  // the result is the fixed effect model
 
 qui xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnlandlea_n  yr*,fe
 est store m4_fe
 qui xtreg D.lnrpgdp D.lnrppi D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnlandlea_n  yr*,re 
 est store m4_re
 hausman m4_fe m4_re,sigmamore  // the result is the fixed effect model
  
 *----------------- private investment & land finance -----------------------
 
 * (1) Fixed effect

xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnplandlea_sq z r yr*,fe vce(cluster id)
est store pfe1
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnrplandlea_p y r yr*,fe vce(cluster id)
est store pfe2
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).price_land r yr*,fe vce(cluster id)
est store pfe3
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnlandlea_n r yr*,fe vce(cluster id)
est store pfe4
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
* (2) Random effect
xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnplandlea_sq z r yr*,re vce(cluster id)
est store pre1
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnrplandlea_p y r yr*,re vce(cluster id)
est store pre2
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).price_land r yr*,re vce(cluster id)
est store pre3
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnlandlea_n r yr*,re vce(cluster id)
est store pre4
test yr2=yr3=yr4=yr5=yr6=yr7=yr8=yr9=yr10=yr11=yr12=yr13=yr14=0
outreg2 [pfe1 pfe2 pfe3 pfe4 pre1 pre2 pre3 pre4] using result2, word replace ///
 title("表2:土地对私人投资的静态效应" )  /// // (T1)
 tdec(2) rdec(3) r2 e(F) /// // (T3)
 nonote /// // (T4)
 addnote("注：(1)***,**,*分别表示在1%,5%和10%水平上显著;", ///
 "(2)括号中为标准误;", ///
 "(3)其它注释语句。")
 
 *----------Hausman test ------------------*
 qui xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnplandlea_sq z r yr*,fe
 est store pm1_fe
 qui xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnplandlea_sq z r yr*,re 
 est store pm1_re
 hausman pm1_fe pm1_re,sigmamore  // the result is the fixed effect model
 
 qui xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnrplandlea_p y r yr*,fe
 est store pm2_fe
 qui xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnrplandlea_p y r yr*,re 
 est store pm2_re
 hausman pm2_fe pm2_re,sigmamore  // the result is the fixed effect model
 
 qui xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).price_land r yr*,fe
 est store pm3_fe
 qui xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).price_land r yr*,re 
 est store pm3_re
 hausman pm3_fe pm3_re,sigmamore  // the result is the fixed effect model
 
 qui xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnlandlea_n r yr*,fe
 est store pm4_fe
 qui xtreg D.lnrppi D.lnrpgdp D.lnrpgi FDI_ind fd1 PT L.edu inf L(0/1).lnlandlea_n r yr*,re 
 est store pm4_re
 hausman pm4_fe pm4_re,sigmamore  // the result is the fixed effect model
 
 *-----------dynamic panel data and sys-GMM--------

*(1)----------endogenous test--------------
/*xtivreg lngrp lnFAI lnFDI PT XM  lnlandlea_p (lnrgrp = L.lnrgrp),fe
dmexogxt */

*----------country economic growth--------------

*   --------   landlease price ----------

xtabond2 D.lnrpgdp L.D.lnrpgdp D.lnrppi D.lnrpgi L.lnrplandlea_p y L.edu fd1 FDI_ind PT inf yr*,  ///
    gmm(L.D.lnrpgdp,lag(4 .))  iv(D.lnrppi D.lnrpgi L.lnrplandlea_p y L.edu fd1 FDI_ind PT inf  yr*) ///
    noleveleq small robust
est store fd_1s

xtabond2 D.lnrpgdp L.D.lnrpgdp D.lnrppi D.lnrpgi L.lnrplandlea_p y L.edu fd1 FDI_ind PT inf yr*,  ///
        gmm(L.D.lnrpgdp,lag(4 .))  iv(D.lnrppi D.lnrpgi L.lnrplandlea_p fd1 y L.edu PT inf FDI_ind yr*) ///
        noleveleq small robust twostep
est store fd_2s

xtabond2 D.lnrpgdp L.D.lnrpgdp D.lnrppi D.lnrpgi L.lnrplandlea_p y L.edu fd1 FDI_ind PT inf  yr*,     ///
             gmm(L.D.lnrpgdp,lag(2 .))  iv(D.lnrppi D.lnrpgi L.lnrplandlea_p fd1 y L.edu PT inf FDI_ind yr*) ///
             small robust
est store sys_GMM1

xtabond2 D.lnrpgdp L.D.lnrpgdp D.lnrppi D.lnrpgi L.lnplandlea_sq z L.edu fd1 FDI_ind PT inf  yr*,     ///
             gmm(L.D.lnrpgdp,lag(2 .))  iv(D.lnrppi D.lnrpgi L.lnplandlea_sq z fd1 L.edu PT inf FDI_ind yr*) ///
             small robust 
est store sys_GMM2
xtabond2 D.lnrpgdp L.D.lnrpgdp D.lnrppi D.lnrpgi price_land  L.edu fd1 FDI_ind PT inf  yr*,     ///
        gmm(L.D.lnrpgdp price_land,lag(2 9)) iv(D.lnrppi D.lnrpgi L.edu fd1 FDI_ind PT inf  yr*) ///
        small robust
est store sys_GMM3
 			  
reg D.lnrpgdp L.D.lnrpgdp D.lnrppi D.lnrpgi L.lnrplandlea_p y L.edu fd1 FDI_ind PT inf  yr*,robust
est store OLS
xtreg D.lnrpgdp L.D.lnrpgdp D.lnrppi D.lnrpgi L.lnrplandlea_p y L.edu fd1 FDI_ind PT inf  yr*,fe robust
est store FE
local mm "fd_1s fd_2s sys_GMM1 sys_GMM2 sys_GMM3 OLS FE"
 local ss "ar2 ar2p sargan sar_df sarganp"
  esttab `mm',mtitle(`mm') scalar(`ss')
   outreg2 [fd_1s fd_2s sys_GMM1 sys_GMM2 sys_GMM3 OLS FE] using result3, word replace  ///
 title("表3:中国经济增长与土地财政的动态面板FD与sys-GMM估计1" )  /// // (T1)
 tdec(2) rdec(3) r2 e(F)   /// // (T3)
 nonote /// // (T4)
 addnote("注：(1)***,**,*分别表示在1%,5%和10%水平上显著;", ///
 "(2)括号中为标准误;", ///
 "(3)其它注释语句。")
 esttab fd_1s fd_2s sys_GMM1 sys_GMM2 sys_GMM3 OLS FE using result4.rtf,  replace ///
 compress nogap b(%6.3f) se(%6.2f) scalars(ar2 ar2p sargan sar_df sarganp) ///
 star(* 0.1 ** 0.05 *** 0.01) obslast
 
 

 
