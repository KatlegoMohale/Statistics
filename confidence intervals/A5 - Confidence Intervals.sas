
goptions reset=all;
title "Ordinary least squares (OLS) regression with inference";

proc reg data=work.import plot=none;
	model 'Hourly Wage'n='Years of Schooling'n/ clb alpha=0.01;
run;
*use the decimal value in alpha to give the interval percentage;


goptions reset=all;
title "Confidence interval for sigma square";

*Calculating the sigma squares  confidence interval;
data MSE_data;
	set work.import;
	x='Years of Schooling'n;
	y='Hourly Wage'n;
	keep x y;
run;

proc iml;
use MSE_data;
read all into x_and_y;
x=x_and_y[,1];
y=x_and_y[,2];
n=nrow(x_and_y);
sum_x=sum(x);
sum_y=sum(y);

sum_x2=ssq(x);
sum_xy=x`*y;
xbar=sum_x/n;
ybar=sum_y/n;
betahat2=(n#sum_xy-sum_x#sum_y)/(n#sum_x2-sum_x##2);
betahat1=ybar-betahat2#xbar;
rss=ssq(y-betahat1-betahat2#x);
mse=rss/(n-2);
alpha=0.05;

ltchi2=cinv(alpha/2,n-2);
utchi2=cinv(1-alpha/2,n-2);
lcl=(n-2)#mse/utchi2;
ucl=(n-2)#mse/ltchi2;

print 'Mean Sqeare error: 'mse[label=none];
print 'Lower tail chi-square value:' ltchi2[label=none];
print 'Upper tail chi-square value:' utchi2[label=none];
print 'Lower confidence limit for sigma-square:' lcl[label=none];
print 'Upper confidence limit for sigma-square:' ucl[label=none];
quit;

