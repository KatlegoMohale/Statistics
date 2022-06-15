goptions reset=all;
title1 'Years of Schooling & Hourly Wages';

proc print data=work.import;
run;

goptions reset=all;
symbol1 color=blue value=star width=2;
title1 'Ordinary least squares (OLS) regression';
proc reg data=work.import plot=none;
	model 'Hourly Wage'n='Years of Schooling'n;
	plot 'Hourly Wage'n*'Years of Schooling'n;
run;
*model dependent variable = explanitory variable;


