/*-----------------------------------------------------------------------------
SAS Version:           9.2 
Operating System:      UNIX
-------------------------------------------------------------------------------

Author:                tigflanker Guo
Creation Date:         17 Mar 2014
Program Name:          adm.sas
Files Created:        
Program Purpose:       input variable metadata from the analysis dataset specifications
                       to use in assigning variable attributes and keep statments
Macro Parameters       in: input working analysis dataset, = (latest dataset) as default.
                           (options: RL[Real Length] + TC[Type Conversion] + NOWSTF)
                           (options:RL) if log occur warning as "Multiple length ... 
						    may cause truncation ...". Use this option ex. "in = adsl|RL" 
							to perform a all character variable STRIP + LEFT operation.
						   (option:TC) if log occur warning as "Variables are defined as multiple types"
						   (option:NOWSTF) Mean: No white space transform.
						    Use this option ex. "in = adsl|tc" to perform a conversion ex. "1" to 1. 
                       out: permanent analysis dataset ([libname.]datasetname)
					        = &in. as default. 
					   sheet: sheetname containing the ADS specs for applicable dataset.
					          Add ADname option in version 1.7 as, sheet = Analysis Variable Metadata|ADCM
					   order: assign the variable order in sheet once the automatically grab
					          failed.(n:name b:label t:type l:length f:format .:others)
                       path: &_ANAL.&_TIMS. ads.xls 
                       update: use this option if XLS's some information are out of date 
					           or wrong. "|" is specified delimiter to separate variables.
							   (ex. update = n : usubjid / l : 6 | n : age / b : "I'm a (test)")
                       div: if you use update option, use this variable in case separator automatically grab failed. 
					        You must define two different character if you use. Ex. div = @ #
					   by:    assign this variable to make a sort after final step.
					   ADMin: use this variable to assign ADM-dataset to Macro when extract information failure from EXCEL.
					          !!! Use Example !!!
							  1. You have to build an ADS dataset before use Macro ADM. Ex.
							  
							  data whatever;
							    infile datalines dsd dlm = '|';
								array char $200. a b c d e g;
								input a $ b $ c $ d $ e $ f g $;
								cards;
                                ADEG|*ALL* |STUDYID|Study Identifier|Char|15|
                                ADEG|*ALL* |RFSTDT|Subject Reference Start Date/Time|Num|8|Date9.
                              ;run;
							  
							  OR:
							  data whatever;
								infile datalines dsd truncover;
								input whatever $200.;
								cards;
                                ADEG|*ALL* |STUDYID|Study Identifier|Char|15|
                                ADEG|*ALL* |RFSTDT|Subject Reference Start Date/Time|Num|8|Date9.
                              ;run;

							  2. Use variable admin = whatever, and don't forget to use ORDER variable according.
							  
							  !!! Tips !!!
							  You can copy information from Excel and use Replace Function under SAS Editor, Notepad++,
							  Whatever place support 'Regular Replace' function. Find '\t' and replace by '|'.
                       debug: if any character assign here, save the process datasets named as ADM_TEMP:
                              open some debug statements likes mLogic mPrint or %put etc.  
-------------------------------------------------------------------------------
MODIFICATION HISTORY:

Version/   | Programmer    |
Date       | Name          | Description of change
------------------------------------------------------------------------------
1.1          tigflanker      Use PRX to get big and small separator from UPDATE
1.2          tigflanker      Add procedure "Transform white space into normal space.". 
                             Fix an error in TC mode. 
1.3			 tigflanker		 Fix an error : use %sysfunc(indexw(...)) instead of %index(...)
                             Add a sort function on macro variable BY.
1.4          tigflanker      If there have any variables in ADS but not in dataset, print to log. 
1.5          tigflanker      1.Update rule: for LENGHT, TYPE, LABEL value if ADS is mis-sing, use original value instead.
                             2.Add three macro variables to store/release print options.
1.5.1		 tigflanker      Fix variable sort Error.	
1.6          tigflanker      Forced to add a variable ADMIN(ADM dataset) in case of failure	that IMPORT information from ADS.
                             The reasons of IMPORT failure maybe like 'Value Hyperlinks', 'Value Format' or something others.
1.6.1        tigflanker      Add Free Style Writing in Update(Beta).
                             Ex. U can define Update like this now:
							 update = n anrind t c n sexn b "Sex' (N)" n vsorre_s l 20 n vsorresu l 20 n vsstresc l 20 
	                                  n vsstresu l 20 n aperiodc l 21 b Period (C)
							 Simplify two variables LGDIV and SMDIV into DIV.
1.7 		 tigflanker      Error fix.	
                             Add a function on variable sheet as, sheet = Analysis Variable Metadata|ADCM
							 Add process on Big-Empty-Excel dispose.
							 Add AD spec type with 'text' and imputation missing length with format numerical value.
1.7.1        tigflanker      Exclude non-character type in Name column.
                             Fix an error with format valid recognition.
-----------------------------------------------------------------------------*/

/*Perform an information transform from ANALYSIS DATASET SPECIFICATIONS to AD datasets*/
%macro ADM(in=_last_,out=%scan(&in.,1,%str(|/\)),sheet=,order=,path=,update=,div=,by=,admin=,debug=);
%let option_mprint    = %sysfunc(getoption(mprint));
%let option_mlogic    = %sysfunc(getoption(mlogic));
%let option_symbolgen = %sysfunc(getoption(symbolgen));

%let options = %scan(&in.,2,%str(|/\));
%let in = %scan(&in.,1,%str(|/\));
%if %length(&div.) %then %do;
  %let smdiv = %scan(&div.,1,%str( ));
  %let lgdiv = %scan(&div.,2,%str( ));
%end;%else %do;
  %let smdiv = #;
  %let lgdiv = \;
%end;
%if %length(&debug.) %then %do;options mprint mlogic symbolgen;%end;
  %else %do;options nomprint nomlogic nosymbolgen;%end;

/*Update Block*/
*Free Style Writing in Update(Beta);
%if %length(%scan(&update,2,%str( ))) ne 1 %then %do;
%let prx1 = %sysfunc(prxparse(s/( +n +\w+)/ |\1/));
%let update1 = %sysfunc(prxchange(&prx1.,-1,&update.));

%let update2 = &update1.;
%do x = 1 %to 5;
  %let prx = %scan(%nrstr((n) +(\w+)@(b) +(.+?)@(t) +(n|c|num|char)@(l) +(\d+)@(f) +(\w+\.)),&x.,@);
  %let prx2 = %sysfunc(prxparse(s/&prx./%sysfunc(ifc(&x. = 1,,\&lgdiv.)) \1 &smdiv. \2/));
  %let update2 = %sysfunc(prxchange(&prx2.,-1,&update2.));
%end;
%let update = &update2.;
%end;

%if %length(%nrbquote(&update.)) %then %do;
  %let start = 1;
  %let stop = %length(%nrbquote(&update.));
  %let prx = %sysfunc(prxparse(/[[:punct:]]/));
  %let position = 0;
  %let length = 0;
  
  %syscall prxnext(prx, start, stop, update, position, length);
  %let prxc = 2;
  
  %do %while(&position. and &prxc.);
    %let result = %nrbquote(%substr(%nrbquote(&update.),&position.,&length.));
    
    %if %sysfunc(prxmatch(%sysfunc(prxparse(%nrbquote(/[^_''""]/))),%nrbquote(&result.))) %then %do;
      %let %scan(lgdiv smdiv,&prxc.) = &result.;
      %let prxc = %eval(&prxc. - 1);
    %end;
    %syscall prxnext(prx, start, stop, update, position, length);
  %end;

  %if "&lgdiv." eq "&smdiv." %then %let lgdiv = %str( );

  %let ADS_update_N = %eval(%sysfunc(count(%nrbquote(&update.),%str(|))) + 1);
  %do i = 1 %to &ADS_update_N.;
    %let ADS_mass = %scan(%nrbquote(&update.),&i.,%str(|));
    %do j = 1 %to %eval(%sysfunc(count(&ADS_mass.,&lgdiv.)) + 1);
      %let ADS_mass_sub = %scan(%nrbquote(&ADS_mass.),&j.,&lgdiv.);
        %do k = 1 %to 5;
          %let ADS_which = %scan(N B T L F,&k.);
          %if %upcase(%scan(&ADS_mass_sub.,1,&smdiv.)) = &ADS_which. %then
            %let ADS_update_&i._&ADS_which. = %scan(&ADS_mass_sub.,2,&smdiv.);
        %end;
    %end;
  %end;

  %do z = 1 %to &ADS_update_N;
    %do y = 1 %to 5;
      %let ADS_which = %scan(N B T L F,&y.);
        %if not %symexist(ADS_update_&z._&ADS_which.) %then %let ADS_update_&z._&ADS_which. = ;
    %end;
  %end;
%end;
%else %let ADS_update_N = 0;
/*Update Block*/

/*Import XLS*/
/*PC or UNIX*/
%let dsname = %upcase(%scan(&sheet.,2,|));
%let sheet = %scan(&sheet.,1,|);

%if &SYSSCP eq WIN %then %do;
proc import datafile = "&path." out = ADM_TEMP1 REPLACE ;
  SHEET = "&sheet.";
  /*range = "&sheet. $ A1:Z500";*/
run;
%end;%else %do;
%if %length(&admin.) %then %do;
%if not %symexist(&admin.) %then %do;
  %put >>>ADS dataset &admin. not exist!!<<<;
  %put >>>Check it or disable ADMin variable.<<<;
%return;
%end;
%let hmc = 0;
data _null_;
  set &admin.;
  array howmanychar _char_;
  
  if _n_ = 1 then do over howmanychar;
    call symputx('hmc',dim(howmanychar));
	if symget('hmc') = '1' then do;
	  call symputx('adminame',vname(howmanychar));
	  call symputx('hmv',count(howmanychar,'|') + 1);
	  prx = prxparse('/^\d+$/');
      do z = 1 to input(symget('hmv'),best.);
	    if prxmatch(prx,scan(howmanychar,z,'|')) then call symputx('lth',z);
	  end;
	end;
  end;
run;

%if &hmc. eq 1 %then %do;
  data &admin.(drop = &adminame. admv0);
    set &admin.;
	length admv0 $200.;
	
	&adminame. = tranwrd(&adminame.,'|','| ');

	%do i = 1 %to &hmv.;
      length admv&i. %sysfunc(ifc(&i. eq &lth.,8.,$200.));
      admv0 = strip(scan(&adminame.,&i.,'|'));
      admv&i. = %sysfunc(ifc(&i. eq &lth.,input(admv0,best.),admv0));
	%end;
  run;
%end;

data ADM_TEMP1;
  set &admin.;
run;
%end;%else %do;
proc import datafile = "&path." out = ADM_TEMP1 DBMS = XLS REPLACE;
  SHEET = "&sheet.";
  GETNAMES = NO;
  DATAROW = 2;
run;

proc import datafile = "&path." out = ADM_TEMPU DBMS = XLS REPLACE;
  SHEET = "&sheet.";
  GETNAMES = NO ;
run;

%macro temp(tempin);
data &tempin. ADM_TEMP0;
  set &tempin.(%if %length(&dsname.) %then where = (upcase(a) = cats("&dsname.") or length(a) > 8););

  array TAC _Char_;

  TAL = 0;

  do over TAC;
    TAL = max(lengthn(TAC),TAL);
  end;

  if TAL;

  drop TAL;
run; 

%let rc = %sysfunc(open(ADM_TEMP0));
%let rc1 = %sysfunc(attrn(&rc.,NVARS));
%let rc = %sysfunc(close(&rc.));

option nosource;
data _null_;
  set ADM_TEMP0 end = last;

  array TAC _Char_;
  array TACC TACC1 - TACC&rc1.;
  retain TACC 0;

  do over TAC;
    TACC = TACC + ifn(lengthn(TAC),1,0);
  end;

  if last then do;
    call execute("data &tempin.;set &tempin.(drop = ");
      do over TAC;
        if TACC < 2 then call execute(vname(TAC));
	  end;
	call execute(');run;');
  end;
run;
option source;

%mend temp;
%temp(ADM_TEMP1)
%temp(ADM_TEMPU)

%let rc = %sysfunc(open(ADM_TEMPU));
%let rc1 = %sysfunc(attrn(&rc.,NVARS));
%let rc = %sysfunc(close(&rc.));

data _null_;set ADM_TEMPU;
  array all _char_;
  array col $25 col1 - col&rc1.;
 
  if _N_ > 1 then delete;
 
  prxparse = prxparse('s/[^a-zA-Z]/_/');
  prxparse1 = prxparse("s/(.*[[:alnum:]])_+/\1/");
  
  do over all;
    if length(all) then do;
      col = strip(all);
      call prxchange(prxparse,-1,col);
      call prxchange(prxparse1,-1,col);
      call symput('ADM_NAME'||strip(put(_i_,best.)),col);
      call symput('ADM_NAMEO'||strip(put(_i_,best.)),vname(all));
      call symput('ADM_NAMEN',strip(put(_i_,best.)));
	end;
  end;
run;

data ADM_TEMP1;set ADM_TEMP1;
  %do i = 1 %to &ADM_NAMEN.;
    rename &&ADM_NAMEO&i.. = &&ADM_NAME&i..;
  %end;
run;
%end;  *ADMin else end;
%end;

data ADM_TEMP_order;
  set ADM_TEMP1;
  ADM_TEMP_order = _n_;
run;

/*Manual fix order*/
%if &order. ne %then %do;
proc contents data = ADM_TEMP1 out = ADM_TEMP0(keep = NAME VARNUM) NOPRINT;
run;

%do i = 1 %to %eval(%sysfunc(count(%sysfunc(compbl(&order.)),%str( )))+1);
  %do j = 1 %to 5;
    %let k = %scan(N B T L F,&j.);
    %let l = %scan(ADM_NAME ADM_LABEL ADM_TYPE ADM_LENGTH ADM_FORMAT,&j.);
    %if %upcase(%scan(&order.,&i.,%str( ))) = &k. %then %let &l. = &i.;
  %end;
%end;

data _null_;set ADM_TEMP0;
  select(VARNUM);
    when(&ADM_NAME.) call symputx('ADM_NAME',NAME);
    when(&ADM_LABEL.) call symputx('ADM_LABEL',NAME);
    when(&ADM_TYPE.) call symputx('ADM_TYPE',NAME);
    when(&ADM_LENGTH.) call symputx('ADM_LENGTH',NAME);
    when(&ADM_FORMAT.) call symputx('ADM_FORMAT',NAME);
    otherwise;
  end;
run;
%end;
%else %do;
/*Auto fix order*/
data _null_;set ADM_TEMP1;
  array AN _Numeric_;
  array AC _Char_;

  if _N_ = 1 then do over AC;
    if find(vname(ac),'type','i') then call symputx('ADM_TYPE',vname(AC));
    if find(vname(ac),'label','i') then call symputx('ADM_LABEL',vname(AC));
    if find(vname(ac),'format','i') then call symputx('ADM_FORMAT',vname(AC));
    if find(vname(ac),'name','i') then call symputx('ADM_NAME',vname(AC));
  end;

  if _N_ = 1 then do over AN;
    call symputx('ADM_LENGTH',vname(AN));
  end;
run;
%end;

/*Build a dataset ADM_EMPTY with required frame from ADS*/
*White space transform into normal space;
*Translate type (char,text, ) into char, other into num;
*Perform an imputation when length is missing, and use format vlaue instead;
%if not %index(%upcase(&options.),NOWSTF) %then %do;
  data ADM_TEMP1(drop = prx:);set ADM_TEMP1;
    array allc &ADM_NAME. &ADM_LABEL. &ADM_TYPE. &ADM_FORMAT.;
    prx = prxparse('s/\s/ /');
	prx1 = prxparse('s/[^\w[:punct:]]/ /');
	&ADM_NAME. = strip(prxchange(prx1,-1,&ADM_NAME.));
	do over allc;
      call prxchange(prx,-1,allc);
	  allc = strip(allc);
	end;
	
	&ADM_TYPE. = ifc(upcase(&ADM_TYPE.) in ('CHAR','TEXT','') or index(&ADM_FORMAT.,'$'),'char','num');
	if missing(&ADM_LENGTH.) then &ADM_LENGTH. = input(compress(&ADM_FORMAT.,,'kd'),best.);
	&ADM_LENGTH. = ifn(&ADM_TYPE. = 'num',min(&ADM_LENGTH.,8),&ADM_LENGTH.);
  run;
%end;

*Update LENGHT, TYPE, LABEL value if ADS is mis-sing, use original instead;
%if %length(&in.) %then %do;    
proc contents data = &in. out = ADM_TEMP_EXAM1(keep = NAME TYPE LENGTH LABEL 
  rename=(NAME = &ADM_NAME. TYPE = OR_TYPE LENGTH = OR_LENGTH LABEL = OR_LABEL)) NOPRINT;run;
  
proc sql noprint;
  create table ADM_TEMP_EXAM2 as 
  select a.&ADM_NAME., a.&ADM_FORMAT., ifn(missing(a.&ADM_LENGTH.),b.OR_LENGTH,a.&ADM_LENGTH.) as &ADM_LENGTH.,
         ifc(missing(a.&ADM_TYPE.),ifc(b.OR_TYPE - 1,'Char','Num'),a.&ADM_TYPE.) as &ADM_TYPE. length = 20,
		 ifc(missing(a.&ADM_LABEL.),b.OR_LABEL,a.&ADM_LABEL.) as &ADM_LABEL. from ADM_TEMP1 a left join ADM_TEMP_EXAM1 b
		 on a.&ADM_NAME. = b.&ADM_NAME.;
quit;

proc sql noprint;
  create table ADM_TEMP1 as
  select a.*, b.ADM_TEMP_order from
  ADM_TEMP_EXAM2 a left join ADM_TEMP_order b
  on a.&ADM_NAME. = b.&ADM_NAME.
  order by ADM_TEMP_order;
quit;
%end;

*Update Block;
%if &ADS_update_N. %then %do;
  data ADM_TEMP1;set ADM_TEMP1;
    do update = 1 to &ADS_update_N.;
      if strip(&ADM_NAME.) = upcase(symget('ADS_update_'||strip(put(update,best.))||'_N')) then do;
        if not missing(symget('ADS_update_'||strip(put(update,best.))||'_B'))
           then &ADM_LABEL. = symget('ADS_update_'||strip(put(update,best.))||'_B');
        if not missing(symget('ADS_update_'||strip(put(update,best.))||'_T'))
           then &ADM_TYPE. = symget('ADS_update_'||strip(put(update,best.))||'_T');
        if not missing(symget('ADS_update_'||strip(put(update,best.))||'_L'))
           then &ADM_LENGTH. = input(symget('ADS_update_'||strip(put(update,best.))||'_L'),best.);
        if not missing(symget('ADS_update_'||strip(put(update,best.))||'_F'))
           then &ADM_FORMAT. = symget('ADS_update_'||strip(put(update,best.))||'_F');
      end;
    end;
	
    /*Disassemble protective quotation mark*/
	if substr(&ADM_LABEL.,1,1)||substr(reverse(strip(&ADM_LABEL.)),1,1) in ('""',"''") then 
	  &ADM_LABEL. = substr(&ADM_LABEL.,2,length(strip(&ADM_LABEL.))-2);
  run;
%end;

%let ADM_VARLIST=;
data _null_;
  set ADM_TEMP1(where = (length(&ADM_NAME.) > 2)) end=last;
  call symputx('ADM_VARLIST',symget('ADM_VARLIST')||' '||strip(&ADM_NAME.));
  
  if _N_ = 1 then call execute("data ADM_EMPTY;");
  call execute("attrib "  ||strip(&ADM_NAME.)||
               ' label = '||quote(strip(&ADM_LABEL.))||
               ifc(prxmatch('/.*\d+(\.)/',&ADM_FORMAT.) and length(&ADM_FORMAT.) le 10," format = "||strip(&ADM_FORMAT.),'')||
               " length = "||ifc(first(UPCASE(strip(&ADM_TYPE.)))="C","$","")||
                            ifc(missing(&ADM_LENGTH.),'8',strip(put(&ADM_LENGTH.,best.)))||
               ";");

  call execute("call missing("||strip(&ADM_NAME.)||");");
  if last then call execute("stop;run;");
run;

/*If a dataset in,update it's frame by ADM_EMPTY*/
%if %length(&in.) %then %do;    
  /*TYPE CONVERSION*/
  %if %index(%upcase(&options.),TC) %then %do;
    %macro ADM_TYPE(in2=,out=ADM_TEMP0);
    proc contents data = ADM_EMPTY out = ADM_TEMP2 NOPRINT;run;
    proc contents data = &in2. out = ADM_TEMP3 NOPRINT;run;
    
    data ADM_TEMP3(keep = NAME TYPE1);set ADM_TEMP3;
      NAME = upcase(NAME);
      TYPE1 = TYPE;
    run;
    
    %let ADM_TYPE_ALLVAR=;
    %let ADM_TYPE_ALLCHAR_TEMP=;
    %let ADM_TYPE_ALLNUM_TEMP=;      
    
    proc sort data = ADM_TEMP3;by NAME;run;
    proc sort data = ADM_TEMP2;by NAME;run;
    data ADM_TEMP4;
    merge ADM_TEMP3 ADM_TEMP2(keep = NAME TYPE in = in);
    by NAME;
	if in;
    
    if TYPE ^= TYPE1 and not missing(TYPE1) then do;
      call symputx('ADM_TYPE_ALLVAR',symget('ADM_TYPE_ALLVAR')||' '||strip(NAME));
    end;
    run;
    
    data _null_;
      set &in2.;
      array ADM_TYPE_ALLCHAR _char_;
      array ADM_TYPE_ALLNUM _numeric_;
    
      if _N_ = 1 then do;
        do over ADM_TYPE_ALLCHAR;
          call symputx('ADM_TYPE_ALLCHAR_TEMP',symget('ADM_TYPE_ALLCHAR_TEMP')||' '||upcase(vname(ADM_TYPE_ALLCHAR)));
        end;
        
        do over ADM_TYPE_ALLNUM;
          call symputx('ADM_TYPE_ALLNUM_TEMP',symget('ADM_TYPE_ALLNUM_TEMP')||' '||upcase(vname(ADM_TYPE_ALLNUM)));
        end;
      end;
    run;
    
    data &out.;set &in2.;
      %do i = 1 %to %eval(%sysfunc(count(&ADM_TYPE_ALLCHAR_TEMP.,%str( ))) + 1);
      %let ADM_TYPE_WHICH = %scan(&ADM_TYPE_ALLCHAR_TEMP.,&i.);
        %if %sysfunc(indexw(&ADM_TYPE_ALLVAR,&ADM_TYPE_WHICH.)) %then %do;
          &ADM_TYPE_WHICH._X = input(&ADM_TYPE_WHICH.,best.);
          drop &ADM_TYPE_WHICH.;
          rename &ADM_TYPE_WHICH._X = &ADM_TYPE_WHICH.;
          %end;
      %end;
      
      %do i = 1 %to %eval(%sysfunc(count(&ADM_TYPE_ALLNUM_TEMP.,%str( ))) + 1);
      %let ADM_TYPE_WHICH = %scan(&ADM_TYPE_ALLNUM_TEMP.,&i.);
        %if %sysfunc(indexw(&ADM_TYPE_ALLVAR,&ADM_TYPE_WHICH.)) %then %do;
          &ADM_TYPE_WHICH._X = left(strip(put(&ADM_TYPE_WHICH.,best.)));
          drop &ADM_TYPE_WHICH.;
          rename &ADM_TYPE_WHICH._X = &ADM_TYPE_WHICH.;
          %end;
      %end;
    run;
    %mend ADM_TYPE;
    %ADM_TYPE(in2=&in.);
	%let in = ADM_TEMP0;
  %end;
  /*TYPE CONVERSION END*/

  /*REAL LENGTH*/
  %if %index(%upcase(&options.),RL) %then %do;
    %macro ADM_RL(in1=,out1=);
      %let ADM_RL_ALLCHARLIST=;
      data _null_;set &in1.;
        array ADM_RL_ALLCHAR _char_;
        if _N_ = 1 then do;
          call symputx('ADM_RL_ALLCHARN',put(dim(ADM_RL_ALLCHAR),best.));
          do over ADM_RL_ALLCHAR;
            call symputx('ADM_RL_ALLCHARLIST',symget('ADM_RL_ALLCHARLIST')||' '||strip(vname(ADM_RL_ALLCHAR)));
          end;
        end;
      run;
      
      data _null_;set &in1. end = last;
        array ADM_RL_ALLCHAR _char_;
        array ADM_RL_ALLCHARCOUNT ADM_RL_ALLCHARCOUNT1 - ADM_RL_ALLCHARCOUNT&ADM_RL_ALLCHARN.;
      
        retain ADM_RL_ALLCHARCOUNT 1;
        do over ADM_RL_ALLCHAR;
          ADM_RL_ALLCHARCOUNT=max(length(ADM_RL_ALLCHAR),ADM_RL_ALLCHARCOUNT);
          if last then call symputx('ADM_RL_ALLCHARNEW'||strip(put(_i_,best.)),ADM_RL_ALLCHARCOUNT);
        end;
      run;
      
      data &out1.;set &in1.;
        array ADM_RL_ALLCHAR _char_;
        
        %do i = 1 %to &ADM_RL_ALLCHARN.;
          attrib ADM_RL_ALLCHARNEW&i. length = $ &&ADM_RL_ALLCHARNEW&i..;
        %end;
        array ADM_RL_ALLCHARNEW $ ADM_RL_ALLCHARNEW1 - ADM_RL_ALLCHARNEW&ADM_RL_ALLCHARN.;
        
        do over ADM_RL_ALLCHAR;
          ADM_RL_ALLCHARNEW = ADM_RL_ALLCHAR;
        end;
        drop &ADM_RL_ALLCHARLIST.;
        
        %do i = 1 %to &ADM_RL_ALLCHARN.;
          rename ADM_RL_ALLCHARNEW&i. = %scan(&ADM_RL_ALLCHARLIST.,&i.);
        %end;
      run;
    %mend ADM_RL;
    %ADM_RL(in1=&in.,out1=ADM_TEMP0);
	%let in = ADM_TEMP0;
  %end;
  /*REAL LENGTH END*/

  /*Exam if there have any variables in ADS but not in datasets.*/
  proc contents data = &in. out = ADM_TEMP_EXAM3(keep = NAME) NOPRINT;run;
  data ADM_TEMP_EXAM4(keep = NAME);set ADM_TEMP1(rename=&ADM_NAME.=NAME);NAME=upcase(NAME);run;
  data ADM_TEMP_EXAM3;set ADM_TEMP_EXAM3;NAME=upcase(NAME);run;
  
  proc sql noprint;
    select NAME into :in_ds_not_ads separated by ', ' 
      from(select NAME from ADM_TEMP_EXAM3 except select NAME from ADM_TEMP_EXAM4);
    select NAME into :in_ads_not_ds separated by ', ' 
      from(select NAME from ADM_TEMP_EXAM4 except select NAME from ADM_TEMP_EXAM3);
  quit;

data &out.;
  if 0 then set ADM_EMPTY;
  set &in.;
  
  keep &ADM_VARLIST.;
run;

  %if %length(&by.) %then %do;
    proc sort;
	  by &by.;
	run;
  %end;
  
%end;

/*Clean Datasets*/
%if not %length(&debug.) %then %do;
  proc datasets lib=work NOLIST;delete ADM_TEMP: %if %length(&in.) %then ADM_EMPTY;;quit;
%end;

  /*Put Exam result.*/
  %if %symexist(in_ads_not_ds) %then %do;
    %put >>>NOTICE!!! There have %eval(%sysfunc(count("&in_ads_not_ds.",%str(,))) + 1) variable(s) in ADS but not in final dataset.<<<;
    %put >>>Variables List : &in_ads_not_ds.<<<;
    %put ;
  %end;
  %if %symexist(in_ds_not_ads) %then %do;
  %if %length(%bquote(&in_ds_not_ads.)) < 256 %then %do;
    %put >>Additional, There have %eval(%sysfunc(count("&in_ds_not_ads.",%str(,))) + 1) variable(s) in final dataset but not in ADS.<<;
    %if %eval(%sysfunc(count("&in_ds_not_ads.",%str(,))) + 1) < 30 %then %put >>Variables List : &in_ds_not_ads.<<;
    %put ;
  %end;
  %end;

  options &option_mprint;
  options &option_mlogic;
  options &option_symbolgen;
%mend ADM;
