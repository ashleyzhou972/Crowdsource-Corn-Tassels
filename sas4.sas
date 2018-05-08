proc import datafile = "\\my.files.iastate.edu\Users\nzhou\Documents\20170518\per_user_image_filter.csv"
	out=mydata
	dbms = csv
	replace;
	GETNAMES = yes;
run;


data mydata1;
set mydata;
if type=1 then type1="3Master";
else if type=2 then type1="2Turker";
else type1="1SONA";
run;

data mydata2;
set mydata;
if type=1 then type1="1Master";
else if type=2 then type1="2Turker";
else type1="3SONA";
run;

data mydata3;
set mydata;
if type=1 then type1="2Master";
else if type=2 then type1="3Turker";
else type1="1SONA";
run;

data mydata_log;
set mydata1;
logtime = log(question_elapsed);
run;

*The big one;
proc mixed data=mydata_log;
	class type1 user image;
	model fmean = type1/ddfm = kr HTYPE=1 SOLUTION;
	random user(type1) image /SOLUTION;
run;

*logtime vs group;

proc mixed data=mydata_log;
	class type1 user image;
	model logtime = type1 question_ordinal type1*question_ordinal/ddfm=kr HTYPE=1 SOLUTION;
	random user(type1) image/solution;
run;

data mydata2_log;
set mydata_log;
if type=1 then type2="1Master";
else if type=2 then type2="2Turker";
else type2="3SONA";
run;


proc mixed data=mydata2_log;
	class type2 user image;
	model logtime = type2 question_ordinal type2*question_ordinal/ddfm=kr HTYPE=1 SOLUTION;
	random user(type2) image/SOLUTION;
run;



data master_log;
set mydata_log;
if type=2 or type=3 then delete;
run;

proc mixed data=master_log;
	class type1 user image;
	model logtime =  question_ordinal /ddfm=kr HTYPE=1 SOLUTION;
	random user(type1) image;
run;

data turker_log;
set mydata_log;
if type=1 or type=3 then delete;
run;

proc mixed data=turker_log;
	class type1 user image;
	model logtime =  question_ordinal /ddfm=kr HTYPE=1 SOLUTION;
	random user(type1) image;
run;

data sona_log;
set mydata_log;
if type=1 or type=2 then delete;
run;

proc mixed data=sona_log;
	class type1 user image;
	model logtime =  question_ordinal /ddfm=kr HTYPE=1 SOLUTION;
	random user(type1) image;
run;


proc mixed data=mydata2_log;
	class type1 user image;
	model logtime = type1 question_ordinal type1*question_ordinal/ddfm=kr HTYPE=1 SOLUTION;
	random user(type1) image;
run;

*accuracy vs logtime;

proc mixed data=mydata_log;
	class type1 user image;
	model fmean = logtime type1 logtime*type1/ddfm=kr HTYPE=1 SOLUTION;
	random user(type1) image;
run;

proc mixed data=master_log;
	class type1 user image;
	model fmean = logtime/ddfm=kr HTYPE=1 SOLUTION;
	random user(type1) image;
run;

proc mixed data=turker_log;
	class type1 user image;
	model fmean = logtime/ddfm=kr HTYPE=1 SOLUTION;
	random user(type1) image;
run;


proc mixed data=sona_log;
	class type1 user image;
	model fmean = logtime /ddfm=kr HTYPE=1 SOLUTION;
	random user(type1) image;
run;

