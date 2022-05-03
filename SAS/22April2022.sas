quit ;
dm 'odsresults;clear';

options nocenter ;
title ;



data a (keep=house stand prop); 
input income1 income2 house stand double prop;
cards;
  521.502    118.348    735.779     920.53      0     1215.86
   14.116    457.801    413.522     690.15      0      917.67
  308.237    205.341    567.238     903.11      1     1201.16
  449.589    157.470    496.226     659.05      1     1099.73
   47.286    555.871    414.292     769.92      0     1077.25
   12.702    400.744    283.223     539.62      1      973.28
  303.539    360.630    671.121     934.58      0     1206.01
  325.548    369.610    284.473     707.85      0     1071.03
  328.079     17.192    492.552     699.23      0      834.60
  479.735     34.212    767.408    1097.32      0     1102.11
   70.381    319.148    373.140     760.19      0      774.12
  232.232    255.517    238.515     577.39      0      828.64
   56.125    326.705    589.865     930.42      0     1020.10
  510.569     36.773    461.059     920.65      0     1044.39
   15.890    353.851    345.385     655.05      1      932.65
  298.906    126.398    531.592    1093.24      0     1036.68
  280.401    105.089    497.296     727.87      0      867.00
  188.411    419.229    383.097     903.32      0     1114.49
   11.004    462.602    351.969     575.44      0      833.46
  408.952    119.757    650.882     950.26      0     1044.39
  114.999    253.868    439.853     849.32      0      831.60
  200.932    141.234    400.907     571.64      0      773.70
  276.907    350.366    554.191     948.33      1     1273.24
  271.076    109.235    734.862     970.72      1     1124.66
  357.141    324.151    507.147     686.02      0     1146.86
   74.029    403.535    372.881     520.79      0      836.79
  112.752    195.755    550.987    1048.71      0     1023.95
  189.496    273.100    400.458     550.31      0      834.02
  283.516    395.697    445.404     600.35      0     1064.84
  255.701    154.743    535.123    1078.51      0     1075.30
  ;
run ;



/*data a ;*/
/* set a  (obs=10);*/
/*run ;*/


proc print data=a ;
run ;

proc corr  data=a ;
run ;

proc reg data=a ;
model prop =  house stand / covb;
run ;


* response => prop ;
* feature => stand size (stand) and house size (house) ;

proc iml ;

use a ;
read all into xy ;  *order is NB! ;

print xy ;
n=nrow(xy) ;
*prepare data matrices ;

x = J(n,1,1) || xy[,1:2] ;
y = xy[,3] ;

print n x y ;

k= ncol(x) ;

*estimate regression parameters ;
bh = inv(x`*x)*x`*y ;
nm={"bh1" "bh2" "bh3"};
print bh[rowname=nm] ;

*calculate ANOVA table ;

res=y-x*bh ;
rss=res`*res ;  *residual sum of squares ;
tss=(y-y[:,])[##,] ; *total sum of squares ;
ess=tss-rss ; *explained/estimated/model sum of squares ;

df_e = ncol(x)-1 ; *degrees of freedom - model/estimated ;
df_r = n-ncol(x) ; *degrees of freedom - residual ;
df_t = n-1 ; *total degrees of freedom ;

ms_e = ess/df_e ;*mean square model /estimated ;
ms_r = rss/df_r ;*mean square residual or error ;
f=ms_e/ms_r ; *f value ;

p_f = 1-probf(f,df_e,df_r) ; * prob f ;

print res tss ess rss df_t df_e df_r  ms_e ms_r f p_f ;

* calculate fit summary ;
rmsr = sqrt(ms_r) ;
meany = y[:,] ;
cv=rmsr/meany*100 ;

r2=ess/tss ; *r square ;
ar2 = 1 - ((1-r2)*((n-1)/(n-k))) ; *adjusted r square ;

print rmsr meany cv r2 ar2 ;

*calculate parameter table ;

seb=sqrt(vecdiag(ms_r*inv(x`*x))) ;
t=bh/seb ;
p_t = 2*(1-probt(abs(t),df_r)) ;

cll = bh - tinv(0.95,df_r)*seb ;
clu = bh + tinv(0.95,df_r)*seb ;

print bh seb t p_t cll clu ;


* create output in format similar to SAS proc reg ;

res1 = J(3,5,.) ;
res1[,1] = df_e // df_r // df_t ;
res1[,2] = ess // rss // tss ;
res1[,3] = ms_e // ms_r // . ;
res1[1,4] = f ;
res1[1,5] = p_f ;


cnm={"Df" "SS" "MS" "F" "P_F"} ;
rnm={"Model" "Error " "CTotal"} ;
print res1[colname=cnm rowname=rnm] ;

res2=rmsr // meany // cv ;
/*print res2;*/

res3= r2 // ar2;

rnm2={"Root MSE" "Dep Mean" "Coeff Var"} ;
rnm3={"R-square" "Adj R-sq"} ;
print res2[rowname=rnm2] 
res3[rowname=rnm3] ;


cnm={"df" "Par est" "SEB" "t" "p>|t|" "cll" "clu"} ;
rnm={"Int"  "House" "Stand" } ;
res4 = J(ncol(x),1,1) || bh || seb || 
     t || p_t || cll || clu;

print res4[colname=cnm rowname=rnm] ;


*some other calculations ;

corr_dat = corr(xy) ;
nm1={"House-X1" "Stand-X2" "Prop-Y"} ;
print corr_dat[colnames=nm1 rownames=nm1] ;

cov_bh = ms_r*inv(x`*x);
nm1={"bh1" "bh2" "bh3"} ;
print cov_bh[colnames=nm1 rownames=nm1]  ;


quit ;



/*
dep -> prop

exp / feat -> 

income = income1+income2

ratio = house/stand 

double 

*/
