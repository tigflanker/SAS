/*-----------------------------------------------------------------------------
SAS Version:           9.2
Operating System:      UNIX
-------------------------------------------------------------------------------

Author:                tigflanker Guo
Creation Date:         21 Mar 2014
Program Name:          tf_fmt.sas
Files Created:        
Program Purpose:       Perform a simple PROC FORMAT from a dataset.
                       Or extract the real length from a dataset and calculate the pointed format.
Macro Parameters       data: input analysis dataset
                       out: output dataset(default as &data._F)
                       var: input the pair variable you want to create format.
					        ex. :     var =    sexN  sex |  race    raceN | ethnic (missing: +N or -N)
							effect as :         1  = 'M'   'ASIAN' = 4       'Han'  = 1
							In Real Length Mode, it assign the variable need to explore.
					   rule: use in RLM, to give the rule to calculate the pointed format.
					         ex. : rule = minF meanF + .1 sdF +0.2
					   by: assign this variable to give the BY variable in RLM, will be give a format 
					       dataset named as TF_FMT_RL. Include the wanted calculated format variables.
					   options: so far, only contain one option "M", means MULTILABEL for special use.
                       debug: if any character assign here, save the process dataset TF_FMT_TEMP
                              open some debug statements likes mLogic mPrint or %put etc. 
Use Example:			1. Format Extract Mode:  
                           %TF_FMT(data=ADLBF, var = AVISITN AVISITO | LBTEST LBSTRESU | SHIFT1 | SHIFT1N);
						     Create Format : AVISITNF, LBTESTF, SHIFT1F, etc.
                        2. Real Length Mode:
                           %TF_FMT(data=ADLB, var = CHG, rule = min_maxF mean_medianF + .1 sd_seF + .2, by = PARAMCD);
                           %TF_FMT(data=ADLB, var = CHG, by = PARAMCD);	
                             Create dataset : Tf_fmt_rl					   
                           %TF_FMT(data=ADLB, var = CHG, rule = min_maxF mean_medianF + .1 sd_seF + .2);	
                             Create Global Macro Variable : min_maxF, mean_medianF, etc.
                        3. Main Dataset Mode:
                           %TF_FMT(data=ADLB MAIN, var = CHG, rule = a b $80 +.1|c $40|d +.2|e f, by = PARAMCD);
                             (1) Extract real length for CHG from ADLB.
							 (2) Do a PUT function for assign variable(s). EX. for rule = a b $80 +.1
							     old variable: a, new variable: b(default: a), new variable length: $80(default: Numerical)
								 format: 5.2 + .1(5.2 is the real length for this variable, default: 5.2)
-------------------------------------------------------------------------------
MODIFICATION HISTORY:

Version/   | Programmer    |
Date       | Name          | Description of change
------------------------------------------------------------------------------
1.1          tigflanker      Fix an Error.
1.2          tigflanker      Add Real Length Mode and two variables RULE and BY according.
1.3          tigflanker      Update Char to Char and Num to Num format mode.
1.4          tigflanker      Update : You can omit RULE in RLM mode.
1.5          tigflanker      Fix 1 - int(1) = 1.23E-14 error and add a out option to define output dataset name.
1.6          tigflanker      Add main dataset mode.
1.7          tigflanker      Fix an error when input('other value',best.) as input('.',best.)
1.8          tigflanker      Fix an Error and update for OTHER condition.
1.8.1        tigflanker      Fix an Error.
1.9          tigflanker      Update Format Extract Mode on InValue.
                             Now, we can use like num = input(cats(1),invaluef.);
2.0          tigflanker      Now, we can use custom format name like : %tf_fmt(data = tempfmt,var = old new format_name);
-----------------------------------------------------------------------------*/

%macro TF_FMT(data=,out=&data._F,var=,rule=,by=,options=,debug=);
/*Debug or Not*/
%if %length(&debug.) %then %do;
  options mlogic mprint symbolgen;
%end;%else %do;
  options nomlogic nomprint nosymbolgen;
%end;

/*Sub Macro*/
%macro TF_JD_RL(where=);
  data _NULL_;
  set &data.%if %length(&where.) %then (where=&where.); end=last;
  retain TF_JD_Temp111 0 TF_JD_Temp222 0;

  if not missing(&var.) then do;
  %if &TF_JD_R. eq C %then %do;
    &var.1=input(&var.,best.);%end;
  %else %do;&var.1=input(put(&var., best.), best.);%end;

  TF_JD_Temp1=int(&var.1);
  TF_JD_Temp2=&var.1-TF_JD_Temp1;

  TF_JD_Temp11=length(strip(put(TF_JD_Temp1,best.)));
  TF_JD_Temp22=length(strip(compress(put(TF_JD_Temp2,best.),'-')))-2;

  TF_JD_Temp111=max(TF_JD_Temp11,TF_JD_Temp111);
  TF_JD_Temp222=max(TF_JD_Temp22,TF_JD_Temp222);
  end;

  if last then call symputx('TF_JD_R',
     strip(put(TF_JD_Temp111 + TF_JD_Temp222 + 1,best.))||'.'||strip(put(TF_JD_Temp222,best.)));
run;
%mend;

/*Main dataset version.*/
%if %index(&data.,%str( )) or %index(&data.,|) %then %do;
  %let main = %scan(&data.,2,|%str( ));
  %let data = %scan(&data.,1,|%str( ));
  %let out = %sysfunc(tranwrd(&out.,&data.,&main.));

  %let count = %eval(%sysfunc(count(&rule.,|)) + 1);
  %do i = 1 %to &count.;
    %let which&i. = %scan(&rule.,&i.,|);
    %let prx = %sysfunc(prxparse(/(\w+) *(\w+)? *(\$\d+\.?)? *(\+0?\.\d)?/));
	%if %sysfunc(prxmatch(&prx.,&&which&i..)) %then %do j = 1 %to 4;
      %let block&j. = %sysfunc(prxposn(&prx.,&j.,&&which&i..));

	  %if not %length(&&block&j..) %then %do;
        %if &j. = 2 %then %let block2 = &block1;
        %else %if &j. = 3 %then %let block3 = 8;
	    %else %if &j. = 4 %then %let block4 = +0;
	  %end;
	%end;
	%let which&i. = &block1. &block2. &block3. &block4.;
  %end;
  
  %let dsid=%sysfunc(open(&data.));  
  %let TF_JD_R=%sysfunc(vartype(&dsid,%sysfunc(varnum(&dsid,&var.))));
  %let rc=%sysfunc(close(&dsid));
  
  proc sql noprint;
    select distinct &by. into :TF_JD_VL separated by ' ' from &data.;
  quit;
  
  %do i = 1 %to %eval(%sysfunc(count(&TF_JD_VL.,%str( )))+1);
    %TF_JD_RL(where=(&by. eq "%scan(&TF_JD_VL.,&i.)"));

    %if &i. = 1 %then %do;
      %let dsid=%sysfunc(open(&data.));  
      %let TF_JD_R1=%sysfunc(varlen(&dsid,%sysfunc(varnum(&dsid,&by.))));
      %let rc=%sysfunc(close(&dsid));

      data TF_FMT_TEMP;
        length &by. $&TF_JD_R1. VFormat 8;
        &by. = "%scan(&TF_JD_VL.,&i.)";
        VFormat = %if %index(&TF_JD_R.,%str(.0)) %then &TF_JD_R.;%else %sysevalf(&TF_JD_R. + 1);;
      run;
    %end;%else %do;
      data TF_FMT_TEMP;
        set TF_FMT_TEMP end = last;
        output;
 	
        if last then do;
          &by. = "%scan(&TF_JD_VL.,&i.)";
          VFormat = %if %index(&TF_JD_R.,%str(.0)) %then &TF_JD_R.;%else %sysevalf(&TF_JD_R. + 1);;
          output;
        end;
      run;
    %end;
   %if %length(&rule.) %then %do;
     data TF_FMT_TEMP;
       set TF_FMT_TEMP;
          
	   %do j = 1 %to &count;
         TF_FMT_TEMP_&j. = VFormat + %scan(&&which&j..,2,+) * 10 + %scan(&&which&j..,2,+);
	   %end;
	 run;
   %end;
  %end;
	
  /*Update Main dataset	*/
  data &out.;
    merge &main. TF_FMT_TEMP;
	by &by.;
  run;

  data &out.;
    set &out.;
	
	%do i = 1 %to &count.;
	  length TF_VAR_TEMP_&i. %scan(&&which&i..,3,%str( ));
      %if %index(%scan(&&which&i..,3,%str( )),%str($)) %then %do;
        TF_VAR_TEMP_&i. = putn(%scan(&&which&i..,1,%str( )),cats(TF_FMT_TEMP_&i.));
	  %end;%else %do; 
        TF_VAR_TEMP_&i. = input(putn(%scan(&&which&i..,1,%str( )),cats(TF_FMT_TEMP_&i.)),best.);
      %end;
	%end;

	drop %do i = 1 %to &count.;%scan(&&which&i..,1,%str( )) %end; TF_FMT_TEMP:;
	rename %do i = 1 %to &count.;TF_VAR_TEMP_&i. = %scan(&&which&i..,2,%str( )) %end;;
  run;
%end;

%else %do;  *Other Two conditions;
%if %length(&rule.&by.) %then %do;  *Real Length Mode;
  /*Variables Decomposed */
  %if %length(&rule.) %then %do;
    %let prx = %sysfunc(prxparse(s/(\w+\d*\w*( \+ *0?\.\d)?)/$1/));
    %let start = 1;
    %let stop = %length(&rule.);
    %let position = 0;
    %let length = 0;
    %let count = 1;
    
    %syscall prxnext(prx, start, stop, rule, position, length);
    %let part1 = %substr(&rule.,&position.,&length.);
    %if not %length(%scan(&part1.,2,+)) %then %let part1 = &part1. + 0;
  
    %do %while(&start. < &stop.);
      %let count = %eval(&count. + 1);
  	  %syscall prxnext(prx, start, stop, rule, position, length);
  	  %let part&count. = %substr(&rule.,&position.,&length.);
      %if not %length(%scan(&&part&count..,2,+)) %then %let part&count. = &&part&count.. + 0;
    %end;
  %end;
  
  %let dsid=%sysfunc(open(&data.));  
  %let TF_JD_R=%sysfunc(vartype(&dsid,%sysfunc(varnum(&dsid,&var.))));
  %let rc=%sysfunc(close(&dsid));
    
  %if not %length(&by.) %then %do;
    %TF_JD_RL;
	%do i = 1 %to &count.;
	  %global %scan(&&part&i..,1,+);
	  %let %scan(&&part&i..,1,+) = %sysevalf(%scan(&&part&i..,2,+) * 10 + %scan(&&part&i..,2,+) + &TF_JD_R.);
	%end;
  %end;%else %do;
    proc sql noprint;
      select distinct &by. into :TF_JD_VL separated by ' ' from &data.;
    quit;
  
    %do i = 1 %to %eval(%sysfunc(count(&TF_JD_VL.,%str( )))+1);
      %TF_JD_RL(where=(&by. eq "%scan(&TF_JD_VL.,&i.)"));
      %if &i. = 1 %then %do;
        data &out.;
          length &var. $200 VFormat 8;
          &var. = "%scan(&TF_JD_VL.,&i.)";
          VFormat = %if %index(&TF_JD_R.,%str(.0)) %then &TF_JD_R.;%else %sysevalf(&TF_JD_R. + 1);;
        run;
      %end;%else %do;
        data &out.;
          set &out. end = last;
          output;
  		
          if last then do;
            &var. = "%scan(&TF_JD_VL.,&i.)";
            VFormat = %if %index(&TF_JD_R.,%str(.0)) %then &TF_JD_R.;%else %sysevalf(&TF_JD_R. + 1);;
            output;
          end;
        run;
      %end;
	    %if %length(&rule.) %then %do;
	      data &out.;
		    set &out.;
          
		    %do j = 1 %to &count;
              %scan(&&part&j..,1,+) = VFormat + %scan(&&part&j..,2,+) * 10 + %scan(&&part&j..,2,+);
		    %end;
		  run;
		%end;
    %end;
  %end;
%end;%else %do;  *Format Extract Mode;
  /*Variables Decomposed */
  %let prx = %sysfunc(prxparse(s/.+?([^_ [:alnum:]]).*/$1/));
  %let elimiters = %sysfunc(prxchange(&prx.,1,&var.));
  %let prx = %sysfunc(prxparse(/\w+/));
  %if %sysfunc(prxmatch(&prx.,&elimiters.)) %then %let elimiters = ;
  
  %let varnum = %eval(%sysfunc(count(&var.,&elimiters.)) + 1);
  %if %index(%upcase(&options.),M) %then %let opM = (MULTILABEL);
    %else %let opM = ;
  %do i = 1 %to &varnum.;
    %let pair = %scan(&var.,&i.,&elimiters.);
    %if not %index(&pair.,%str( )) %then %do;
      %if %upcase(%substr(&pair.,%length(&pair.),1)) eq N %then 
  	  %let pair = &pair. %substr(&pair.,1,%length(&pair.)-1);
      %else %let pair = &pair. &pair.N;
    %end;
  
    %let dsid = %sysfunc(open(&data.));  
    %let type2 = %sysfunc(vartype(&dsid,%sysfunc(varnum(&dsid,%scan(&pair.,2)))));
    %let type  = %sysfunc(vartype(&dsid,%sysfunc(varnum(&dsid,%scan(&pair.,1)))));
    %let rc = %sysfunc(close(&dsid));
    %let type = &type.&type2.;    *value for TYPE maybe:NC,NN,CN,CC;
  
    proc sort data = &data.
	                (keep = %scan(&pair.,1) %scan(&pair.,2) where=(not missing(%scan(&pair.,1)) and not missing(%scan(&pair.,2))))
      out = TF_FMT_TEMP nodupkey;
    by %if &type. eq CN %then %scan(&pair.,2) %scan(&pair.,1);%else %scan(&pair.,1) %scan(&pair.,2);;
    run;
  
    data _null_;
      set TF_FMT_TEMP end = last;
    
  	%if &type. eq NC %then %do;
        if _n_ = 1 then call execute("proc format;
  	    value %sysfunc(ifc(%length(%scan(&pair.,3)),%scan(&pair.,3),%scan(&pair.,1)F)) &opM.");
        call execute(put(%scan(&pair.,1), best.)||' = "'||strip(%scan(&pair.,2))||'"');
		if last then call execute('other = " "');
  	%end;%else %if &type. eq NN %then %do;
        if _n_ = 1 then call execute("proc format;
  	    invalue %sysfunc(ifc(%length(%scan(&pair.,3)),%scan(&pair.,3),%scan(&pair.,1)F)) &opM.");
        call execute(put(%scan(&pair.,1), best.)||' = '||strip(put(%scan(&pair.,2), best.)));
        if last then call execute('other = .');
  	%end;%else %if &type. eq CN %then %do;
        if _n_ = 1 then call execute("proc format;
  	    invalue %sysfunc(ifc(%length(%scan(&pair.,3)),%scan(&pair.,3),%scan(&pair.,1)F)) &opM.");
        call execute('"'||strip(%scan(&pair.,1))||'" = '||strip(put(%scan(&pair.,2), best.)));
        if last then call execute('other = .');
    %end;%else %if &type. eq CC %then %do;
        if _n_ = 1 then call execute("proc format;
  	    value $%sysfunc(ifc(%length(%scan(&pair.,3)),%scan(&pair.,3),%scan(&pair.,1)F)) &opM.");
        call execute('"'||strip(%scan(&pair.,1))||'" = "'||strip(%scan(&pair.,2))||'"');
		if last then call execute('other = " "');
    %end;
      if last then call execute(';run;');
    run;
  %end;
%end;
%end; *Other Two conditions;

/*Clean Datasets*/
%if not %length(&debug.) %then %do;
  proc datasets lib=work NOLIST;delete TF_FMT_TEMP;quit;
%end;
%mend TF_FMT;