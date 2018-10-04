/*-----------------------------------------------------------------------------
SAS Version:           9.2
Operating System:      UNIX
-------------------------------------------------------------------------------

Author:                tigflanker Guo / $LastChangedBy: tanggl $
PROC REPORT Guidance:  Bosh Yang
Creation Date:         31 Mar 2014 / $LastChangedDate: 2014-09-01 22:27:45 -0400 (Mon, 01 Sep 2014) $
Program Name:          tf_report.sas
Files Created:        
Program Purpose:       Use this macro to create a RTF(txt and lst will be consider later) by use PROC REPORT.
Macro Parameters       data: input dataset. (options will be consider later)
                       calign: Use this variable to let some column retain assign length from head to first number's end.
					     ex. Assign 5 to col, the value for col("1.2") will transform to "  1.2".
					     ex. Assign 7 to col, "  3.4 (34.5)" will transform to "    3.4 (34.5)".
                       outfile: full path name(include file name and file suffixes).
					   
					   headers/titles/footers: assign how many headers/titles/footers you will use this product time.
					          option:(c l r) assign items in the centre, left or right. Default: headers,l/titles,c/footers/l.
							  Detect exist for header&i. automatic when omit this variable.
							  In Version 1.2, you can assign headers option respectively like: 4 | 1 l 2-4 c
					   var: assign all variables you will be use this time.
					        option: (p:pageby, s:skipby, o:order, n:norepeat)
							ex.: trtpn(o p) trtp(p) id(o s) col1(s) col2 col3 col4 col5 col6 col7 col8 col9 col10 col11 col12
							Additional, you can use col1 - col3 take the place of col1 col2 col3
                       width: assign column width respective, the sum of all value must less equal 98.
					   align: assign column alignment, also you can use: c l r.
                       Twidth: assign the length of the table width.
					           option: use this option to fine adjust with all header's headline,ex. +1 or -2. 
                       options: Only have NOSUM option so far.
                       debug: assign any character here to open Mprint Mlogic Symbolgen and save process datasets.
           
-------------------------------------------------------------------------------

Before use this macro, you have to define some Macro Variables outside.

1. Header. If your table have 4 columns, you will define header likes ||A|B|C|.
           If your header have a merge display with col2 col3 and col4, you will define header likes: |col1|merge ^^|
		   If your header have a new line use this separator #.(Will be update that, you can assign separator personally)
ex1. 
%let header1 = |State/Site# /Subject # Number|Age(yr)/#Sex [1]/#Race [2]/#Height(cm)/#Weight(kg)|
Visit/#Date/#Day#[3]|Sum#of#Scores|%#Reduct-#ion from#Baseline|                  MPASI Score [4] #### ^^^^^^^^|;

ex2.
%let header2 = ||||||Trunk ^^ |Arms ^^ |Legs ^^|;

2. Headerl(HeaderLine). Define which column in which header you will add a headerline under this item.
                        If this item is in merge situation, you will only define it's first column number.
                        Option: if HeaderLine display not right, you can assign the length manual.(Will be updated)
ex1.
%let headerl2 = 6 9 12|21 21 21;

3. Title and Footer. Use as normal condition.
                     Option: you can use option to add additional code after this Title/Footer.
					 ex.: %let title1 = Maruho Co. Ltd. | j=r "Protocol &_prot.";
ex:
%let footer2 = %str([2]Race: White/Caucasian- W; Black/African American- B; American Indian/Alaska Native- N;
 Asian/S.E. Asian Decent- A; );
 
%let title2 = Page #{pageof};

%let title6 = Treatment group: #byval(trtp);
-------------------------------------------------------------------------------
MODIFICATION HISTORY: Subversion $Rev: 1 $

Version/   | Programmer    |
Date       | Name          | Description of change
------------------------------------------------------------------------------
1.0          tigflanker      The First Edition.
1.1          tigflanker      New algorithm for calculate slines.
                             If column's length >= 25 then add value + 1 * 25 times.
1.2          tigflanker      Adaptive mediate between two parties.
                             Now, in headers variable's option, you can assign three kinds of style:
							 L: All headerline's cell display from left. Ex. headers = 4 | l
							 C: All headerline's cell display to centre. Ex. headers = 4 | c
							 Assign respectively: Ex. headers = 4 | 1 l 2 - 4 c
1.2.1        tigflanker      Fix a error when use single' quote. 
                             %let header1 = %str(|State/|Subject|Laboratory Test|Result|Hy%'s    |Temple%'s |);
1.3          tigflanker      Finally, we can use pageby format like: trt01an(o p) trt01ax(p "Treatment Group: " $50.)
1.4          tigflanker      Add Summary report in log-windows.
                             Add OPTIONS macro variable.
-----------------------------------------------------------------------------*/

%macro TF_report(
        data = _last_,
		calign = ,
        outfile = ,
        
        headers = ,
        titles = ,
        footers = ,
        
        var = ,
        width = ,
        align = ,
        
        Twidth = ,
        
		debug = ,
		options = ,
        /*FontSize = 10|12   ,         */
        );

************************* Sub-Macro *************************;
%macro TF_align(in=,out=&in.,var=,TF_align_debug=);
/*Debug or Not*/
%if %length(&TF_align_debug.) %then %do;
  options mlogic mprint symbolgen;
%end;%else %do;
  options nomlogic nomprint nosymbolgen;
%end;

%let prx = %sysfunc(prxparse(/(.*?) +(\d+)/));

%let start = 1;
%let end = %length(&var.);
%let position = 0;
%let length = 0; 
%let count = 1;

%do %until(&start. ge &end.);
  %syscall prxnext(prx,start,end,var,position,length);
  %let block&count. = %substr(&var.,&position.,&length.);
  %let repeat&count. = %sysfunc(prxchange(%sysfunc(prxparse(s/.* +(\d+)/\1/)),1,&&block&count..));
  %let block&count. = %substr(&&block&count..,1,%length(&&block&count..) - %length(&&repeat&count..));

  %let start = %eval(&position. + &length.);
  %let count = %eval(&count. + 1);
%end;
%let count = %eval(&count. - 1);

data &out.;

  %do i = 1 %to &count.;
    length &&block&i.. $200.;
  %end;

  set &in.;

  %do i = 1 %to &count.;
    array tf_align&i. &&block&i..;
  %end;

  tf_align_prx = prxparse("/\d[\s\(]/");
  %do i = 1 %to &count.;
    do over tf_align&i.;
	  tf_align&i. = left(tf_align&i.);
      call prxsubstr(tf_align_prx,tf_align&i.,tf_align_position);

	  select(sign(&&repeat&i.. - tf_align_position));
        when(-1) do;
		  length tf_align_vname $50.;
		  tf_align_vname = strip(vname(tf_align&i.));
		  put "Assign length &&repeat&i.. is less than real length for Column " tf_align_vname ".";
		  put "Please reassign.";
		  stop;
		end;
		when(0) tf_align&i. = cat(tf_align&i.);
		when(1) tf_align&i. = cat(repeat(' ',&&repeat&i.. - 1 - tf_align_position),tf_align&i.);
        otherwise;
      end; 
	end;
  %end;
run;
%mend TF_align;

%macro add_blank(add_blank);
  %let prx = %sysfunc(prxparse(s/\|/ | /));
  %let prx1 = %sysfunc(prxparse(s/\| +([\w\(\[\{\}\]\)])/|\1/));
  /*%let prx1 = %sysfunc(prxparse(s/\| +(.*?)/|\1/));*/
  %let times = -1;
  
  %let &add_blank. = %sysfunc(prxchange(&prx.,&times.,"&&&add_blank.."));
  %let &add_blank. = %sysfunc(prxchange(&prx1.,&times.,"&&&add_blank.."));
  /*%let &add_blank. = %sysfunc(compbl(&&&add_blank..));*/
%mend add_blank;

%macro unfold(unfold);
%let prx = %sysfunc(prxparse(/\d+ *- *\d+ *\w.*?/));
%if %sysfunc(prxmatch(&prx.,&&&unfold..)) %then %do %until(not %index(&&&unfold..,-));
  %let position = 0;
  %let length = 0;
  %let times = 1;
  %syscall prxsubstr(prx,&unfold.,position,length);
  %let alias = %substr(&&&unfold..,&position.,&length.);

  %let replace = ;
  %do temp = %scan(&alias.,1,%str(-)) %to %sysfunc(compress(%scan(&alias.,2,%str(-)),,a));
    %let replace = &replace. &temp. %sysfunc(compress(%scan(&alias.,2,%str(-)),,d));
  %end;

  %let prx1 = %sysfunc(prxparse(s/\d+ *- *\d+ *\w.*?/&replace./));
  %let &unfold. = %sysfunc(prxchange(&prx1.,&times.,&&&unfold..));
  %let &unfold. = %sysfunc(compbl(&&&unfold..));
%end;
%mend unfold;
************************* Sub-Macro over *************************;
%let tf_syserr = &syserr.;
%let options = %upcase(&options.);

/*Debug or Not*/
%if %length(&debug.) %then %do;
  options mlogic mprint symbolgen;
%end;%else %do;
  options nomlogic nomprint nosymbolgen;
%end;

/* 1.Macro Variable Disassemble*/
* 1.1 Pretreatment and Disassemble;
* 1.1.1 PRE;
* 1.1.1.1 Var: Unfold Alias and Compress a ( g ) b c ( k ) into a(g) b c(k);
%let times = -1;

* Unfold;
%let prx = %sysfunc(prxparse(/\w+ *- *\w+.*/));
%if %sysfunc(prxmatch(&prx.,&var.)) %then %do;
  %let position = 0;
  %let length = 0;
  %syscall prxsubstr(prx,var,position,length);
  %let alias = %substr(&var.,&position.,&length.);

  %let replace = ;
  %do a = %sysfunc(compress(%scan(&alias.,1,%str(-)),,a)) %to %sysfunc(compress(%scan(&alias.,2,%str(-)),,a));
    %let replace = &replace. %sysfunc(compress(%scan(&alias.,1,%str(-)),,d))&a.;
  %end;

  %let prx = %sysfunc(prxparse(s/\w+ *- *\w+/&replace./));      
  %let var = %sysfunc(prxchange(&prx.,&times.,&var.));
%end;

* Compress;
%let prx = %sysfunc(prxparse(s/(\w+) +([\(\)])/\1\2/));
%let prx1 = %sysfunc(prxparse(s/(\'?\(\'?) +(\w+)/\1\2/));
%let prx2 = %sysfunc(prxparse(s/(\'?\)\'?) +(\w+)/\1 \2/));

%do %until(not %sysfunc(compare("&var.","%sysfunc(prxchange(&prx.,&times.,&var.))")));
  %syscall prxchange(prx,times,var);
  %syscall prxchange(prx1,times,var);
  %syscall prxchange(prx2,times,var);
%end;

* 1.1.1.2 Calculate for every sline;
%let Twidth_op = %scan(&Twidth.,2,%str(|));
  %if not %length(&Twidth_op.) %then %let Twidth_op = 0; 
%let Twidth = %scan(&Twidth.,1,%str(|));

%let fline = %sysfunc(repeat(_,&Twidth.));
%let width = %sysfunc(compbl(&width.));

* 1.1.2 VAR Disassemble;
%let prx = %sysfunc(prxparse(/\w+(\(.*?\))?/));
%let start = 1;
%let stop = %length(&var.);
%let position = 0;
%let length = 0;

%let varnum = 0;
%let all_vars = ;

%do %until(&start. ge &stop.);
  %syscall prxnext(prx,start,stop,var,position,length);
  %let all_vars = &all_vars. %sysfunc(compress(%qscan(%substr(&var.,&position.,&length.),1,'('),'""'));
  %let varnum = %eval(&varnum. + 1);
  %let varop_&varnum. = %qscan(%qscan(%substr(&var.,&position.,&length.),2,'('),1,')');

  %let start = %eval(&position. + &length.);
%end;
  * p:pageby, s:skipby, o:order, n:norepeat;

* 1.1.2.1 Display_Variables_List;
%let temp = ;
%do v = 1 %to &varnum.;
   %if not %sysfunc(indexw(%upcase(&&varop_&v..),O)) and not %sysfunc(indexw(%upcase(&&varop_&v..),P))
     %then %let temp = &temp. %scan(&all_vars.,&v.,%str( ));
%end;

%let count = 0;
%let DVL = ;
%let DVLS = %eval(%length(%sysfunc(compress(&header1.,%str(^|),k))) - 1);
%do v = 1 %to &DVLS.;
  %let DVL = &DVL. %scan(&temp.,-%eval(&DVLS. - &v. + 1),%str( ));
%end;

* 1.1.2.2 Order_Variables_List;
%let OVL = ;
%let OVL_post = ;
%do v = 1 %to &varnum.;
   %if %sysfunc(indexw(%upcase(&&varop_&v..),O)) %then %do;
     %let OVL = &OVL. %scan(&all_vars.,&v.,%str( ));
     %let OVL_post = &OVL_post. %scan(&all_vars.,%eval(&v. + 1),%str( ));
   %end;
%end;
%if &OVL. ne %then %let OVLS = %eval(%sysfunc(count(&OVL.,%str( ))) + 1);
  %else %let OVLS = 0;
 
* 1.1.2.3 Pageby_Variables_List;
%let PVL = ;
%let PVL_with_order = ;
%let NOPVL = ;
%do v = 1 %to &varnum.;
   %if %sysfunc(indexw(%upcase(&&varop_&v..),P)) %then %do;
     %if not %sysfunc(indexw(%upcase(&&varop_&v..),O)) %then %let PVL = &PVL. %scan(&all_vars.,&v.,%str( ));
     %let PVL_with_order = &PVL_with_order. %scan(&all_vars.,&v.,%str( ));
   %end;%else %let NOPVL = &NOPVL. %scan(&all_vars.,&v.,%str( ));
%end;
%if &PVL. ne %then %let PVLS = %eval(%sysfunc(count(&PVL.,%str( ))) + 1);
  %else %let PVLS = 0;
%let NOPVLS = %eval(%sysfunc(count(&NOPVL.,%str( ))) + 1);
  * 1.1.2.3.1 Get Pageby Format List;
 
* 1.1.2.4 Skipby_Variables_List;
%let SVL = ;
%let SVL_order = ;
%do v = 1 %to &varnum.;
   %if %sysfunc(indexw(%upcase(&&varop_&v..),S)) %then %do;
     %if %sysfunc(indexw(%upcase(&&varop_&v..),O)) %then %let SVL_order = &SVL_order. %scan(&all_vars.,&v.,%str( ));
     %else %let SVL = &SVL. %scan(&all_vars.,&v.,%str( ));
   %end;
%end;
%if &SVL_order. ne %then %let SVLS = %eval(%sysfunc(count(&SVL_order.,%str( ))) + 1);
  %else %if &SVL. ne %then %let SVLS = %eval(%sysfunc(count(&SVL.,%str( ))) + 1);
  %else %let SVLS = 0;

/*%add_blank(var);*/

* 1.2 Headers, Footers or Titles Dispose;
* 1.2.0 If Headers, Footers or Titles missing;
%macro if_missing(if_missing);
%let i = 1;
%if not %length(&&&if_missing..) %then %do %until(%length(&&&if_missing..));
  %if not %symexist(%substr(&if_missing.,1,%length(&if_missing.) - 1)&i.)
    %then %let &if_missing. = %eval(&i. - 1);
  %else %let i = %eval(&i. + 1);
%end;
/*%else %do j = 1 %to &&&if_missing..;*/
  /*%if not %symexist(%substr(&if_missing.,1,%length(&if_missing.) - 1)&j.)*/
    /*%then %let %substr(&if_missing.,1,%length(&if_missing.) - 1)&j. = ;*/
/*%end;*/
%mend if_missing;

%let title_style = %scan(&titles.,2,%str(|));
%let titles = %scan(&titles.,1,%str(|));
%if not %length(&title_style.) %then %do;
  %if_missing(titles);
  %do t = 1 %to &titles.;
    %let title_style = &title_style. c;
  %end;
%end;

%let footer_style = %scan(&footers.,2,%str(|));
%let footers = %scan(&footers.,1,%str(|));
%if not %length(&footer_style.) %then %do;
  %if_missing(footers);
  %do t = 1 %to &footers.;
    %let footer_style = &footer_style. l;
  %end;
%end;

%let header_style = %scan(&headers.,2,%str(|));
  %if not %length(&header_style.) %then %let header_style = l; 
%let headers = %scan(&headers.,1,%str(|));

%if_missing(headers);
%if_missing(footers);
%if_missing(titles);

  * 1.2.1 Headline Disassemble;
  %do h = 1 %to &headers.;
    %if %symexist(headerl&h.) %then %do;
      %let headerl_op&h. = %scan(&&headerl&h..,2,%str(|));
      %let headerl&h. = %scan(&&headerl&h..,1,%str(|));
    %end;
  %end;
  
%let prx_add = %sysfunc(prxparse(s/\s/@/));  * Convert space to @;
%let prx_single = %sysfunc(prxparse(s/\'\'?/tf_single_prx/));  * Convert single quote to tf_single_prx;
%do i = 1 %to &headers.;
  %let header&i. = %sysfunc(prxchange(&prx_single.,-1,%bquote(&&header&i..)));
  %if "%upcase(&header_style.)" eq "L" %then %let header&i. = %sysfunc(prxchange(&prx_add.,-1,&&header&i..));
  %add_blank(header&i.);
%end;

*Adaptive mediate between two parties. (Add in Version 1.2);
%if %length(&header_style.) > 2 %then %do;
  %let AMBTP = 1;
  %unfold(header_style);
  
  %do d = 1 %to &DVLS.;
    %let prx = %sysfunc(prxparse(/%sysfunc(repeat(%str((\d+ +\w *)),%eval(&dvls. - 1)))/));
	%if %sysfunc(prxmatch(&prx.,&header_style.)) %then %let temp = %sysfunc(prxposn(&prx.,&d.,&header_style.));
	%let AMBTP%scan(&temp.,1) = %scan(&temp.,2);
  %end;
  
  %let header_style = L;
%end;
%else %let AMBTP = 0;;

%do i = 1 %to &headers.;
  %do j = 2 %to %eval(&DVLS. + 1);
    %let header_m_%eval(&j. - 1) = 0;  *Dummy sum with 0;
    %if not %symexist(header_nl_&i.) %then %let header_nl_&i. = 0;  *Dummy New_Line sum with 0;
    %if not %symexist(header_m_&i._%eval(&j. - 1)) %then %let header_m_&i._%eval(&j. - 1) = 0;  *Dummy with 0;
    %if &j. = 2 %then %do;
      %let k = 1;
    %end;
	
    %if  &k. ne %eval(&DVLS. + 1) %then %do;
      %let header_&i._&k. = %nrbquote(%scan(&&header&i..,&j.,%str(|)));
	  * Count Max New_Line mark for every header contain.;
	  %let header_nl_&i. = %sysfunc(max(&&header_nl_&i..,%sysfunc(count(&&header_&i._&k..,%str(#)))));
	  
	  *Calculate for every cell(i,k) length;
		%if %sysfunc(count(&&header_&i._&k..,%str(^))) %then %let header_nl_&i._&k. = %sysfunc(count(&&header_&i._&k..,%str(^)));
		%else %let header_nl_&i._&k. = 0;
		  
		%let cell_length = 0;
		%if &&header_nl_&i._&k.. %then %do temp = &k. %to %eval(&k. + &&header_nl_&i._&k..);
		  %let cell_length = &cell_length., %scan(&width.,&temp.);
		%end;%else %let cell_length = &cell_length., %scan(&width.,&k.);
		%let cell_length = %sysfunc(sum(&cell_length.));
		%let width_sum = %sysfunc(sum(%sysfunc(tranwrd(%sysfunc(compbl(&width.)),%str( ),%bquote(,)))));
		%let cell_length = %sysfunc(int(%sysevalf(&cell_length. * 100 / &width_sum.)));
		%let repeat = %sysfunc(int(
		              %sysfunc(ifn(&cell_length. >= 25,
		                          (&cell_length. - %sysfunc(mod(&cell_length.,25))) / 25 + &cell_length. * &Twidth. / 100 + 1 ,
		                           &cell_length. * &Twidth. / 100 + 1))
							  )  );

		*Adaptive mediate between two parties. (Add in Version 1.2);
		%if &AMBTP. %then %do;
          %let header_&i._&k. = %trim(&&header_&i._&k..);
		  %let temp = %trim(%sysfunc(compress(%str(&&header_&i._&k..),^)));
		  %let prx = %sysfunc(prxparse(s/(.*?)\{ *unicode *\w+\}(.*)/\1\2/));
		  %syscall prxchange(prx,times,temp);
		  %let front = %sysfunc(int(%sysevalf((&repeat. + 1 - %length(&temp.)) / 2)));
		  %if %upcase(&&AMBTP&k..) eq C and %length(&&header_&i._&k..) %then %do;
            %if "%substr(%trim(&&header_&i._&k..),1,2)" ne "@" and &front. > 0
		      %then %let header_&i._&k. = %sysfunc(repeat(@,&front.))%trim(&&header_&i._&k..);
		  %end;
		%end;
	  
	  *Add Sline into headers;
	  %if %symexist(headerl&i.) %then %do x = 1 %to &DVLS.;
	    %if %upcase(&&headerl&i..) eq A %then %do y = 1 %to &DVLS.;
		  %let headerl&i. = &&headerl&i.. &y.;
		%end;
		%if %sysfunc(count(&&header_&i._&k..,%str(^))) %then %let header_nl_&i._&k. = %sysfunc(count(&&header_&i._&k..,%str(^)));
		  %else %let header_nl_&i._&k. = 0;
		  
		%if %length(&&headerl_op&i..) %then %let repeat = %eval(%scan(&&headerl_op&i..,&x.) + 1);
		%else %do;       *New algorithm for calculate slines Version 1.1;
		  %let cell_length = 0;
		  %if &&header_nl_&i._&k.. %then %do temp = &k. %to %eval(&k. + &&header_nl_&i._&k..);
		    %let cell_length = &cell_length., %scan(&width.,&temp.);
		  %end;%else %let cell_length = &cell_length., %scan(&width.,&k.);
		  %let cell_length = %sysfunc(sum(&cell_length.));
		  %let width_sum = %sysfunc(sum(%sysfunc(tranwrd(%sysfunc(compbl(&width.)),%str( ),%bquote(,)))));
		  %let cell_length = %sysfunc(int(%sysevalf(&cell_length. * 98 / &width_sum.)));
		  %let repeat = %sysfunc(int(
		                %sysfunc(ifn(&cell_length. >= 25,
		                            (&cell_length. - %sysfunc(mod(&cell_length.,25))) / 25 + &cell_length. * &Twidth. / 100 + 1 ,
		                             &cell_length. * &Twidth. / 100 + 1))
							    )  );
		%end;
		
		*Add Sline;
	    %if %scan(&&headerl&i..,&x.) eq &k. %then %let header_&i._&k. = &&header_&i._&k.. #%sysfunc(repeat(_,&repeat.));
		
	  %end;
	  
      %let l = &k.;
      %let header_m_&i._&k. = 1;
      
      %if %sysfunc(count(&&header_&i._&k..,%str(^))) %then %do;
        %let k = %eval(%sysfunc(count(&&header_&i._&k..,%str(^))) + &k. + 1);
        /*%let header_nl_&i._&k.. = %sysfunc(count(&&header_&i._&k..,%str(^)));*/
	  %end;
      %else %let k = %eval(&k. + 1);
      
      %let header_&i._&l. = %sysfunc(compress("&&header_&i._&l..",^));
    %end;

  %end;
%end;

%let prx_back = %sysfunc(prxparse(s/@/ /));  * Convert back;
* Comple New_Line to let all items in HEADS display on top;
%do h = 1 %to &headers.;
  %do v = 1 %to &DVLS.;
    %if %symexist(header_&h._&v.) %then %do;
      %if &&header_nl_&h.. > %sysfunc(count(&&header_&h._&v..,%str(#))) %then %do;
      %let prx = %sysfunc(prxparse(s/(\")(.*)(\")/\1\2 %sysfunc(repeat( \# ,
                          &&header_nl_&h.. - %sysfunc(count(&&header_&h._&v..,%str(#))) - 1)) \3/));  
	  %let header_&h._&v. = %sysfunc(prxchange(&prx.,&times.,&&header_&h._&v..));
	  %end;
      %if %upcase(&header_style.) eq L %then %let header_&h._&v. = %sysfunc(prxchange(&prx_back.,-1,&&header_&h._&v..));
	  %let header_&h._&v. = %sysfunc(tranwrd(&&header_&h._&v..,tf_single_prx,%str(%')));  
	%end;
  %end;
%end;

* Judge if var_sum(headers) have merge;
%do v = 1 %to &DVLS.;
  %do h = 1 %to &headers.;
    %let header_m_&v. = %eval(&&header_m_&v.. + &&header_m_&h._&v..);
  %end;
%end;

* 2 Report;

ods listing close;
ods rtf file="&outfile." style=global.rtf;

* 2.1 Title;
%do i = 1 %to &titles.;
  title&i. j = %scan(&title_style.,&i.) "%scan(&&title&i..,1,%str(|))" %scan(&&title&i..,2,%str(|));
%end;

  *Make a decide: if have pageby , do not need last head line;
  %if not &PVLS. %then title&i. "&fline.";;

* 2.2 Footer : Only Support repeat on every page now.;
%if &footers. < 11 %then %do i = 0 %to &footers.;
  %if not &i. %then %do;footnote1 "&fline.";%end;
  %else %if %symexist(footer&i.) %then %do;footnote%eval(&i. + 1) j = %scan(&footer_style.,&i.) "&&footer&i..";%end;
%end;

* 2.3 Report Body;
%if %length(&calign.) %then %do;
  %TF_align(in=&data.,out=&data._align,var=&calign.);
  %let data = &data._align;
%end;

option nothreads;
proc report data=&data. nowd headskip headline split="#" spacing=2 missing
            style(report)={protectspecialchars = off asis = on outputwidth = 100 %}
            style(header)={protectspecialchars = off asis = on just = &header_style.}
            style(column)={protectspecialchars = off asis = on just = l}
            style(lines) ={protectspecialchars = off asis = on};

* 2.3.2 Columns;
%macro columns;
%let left = 0;
%let right = 0;
%do v = 1 %to &DVLS.;
  %do h = 1 %to &headers.;
    %if &&header_m_&h._&v.. = 1 %then %do;
       %let left = %eval(&left. + 2);
      (&&header_&h._&v..(
    %end;
     %if &h. = &headers. %then %do; %scan(&DVL., &v.) 
       %let count = 0;
       %if &v. < &DVLS. %then %let mark = %eval(&v. + 1);
       %do temp = 1 %to &headers.;
         %let count = %eval(&count. + &&header_m_&temp._&mark..);
       %end;
       %let count = %eval(&count. * 2);
       %if &v < &DVLS. %then %let right = %eval(&right. + &count.);
       %if &v. = &DVLS. %then %sysfunc(repeat(%str(%)),%eval(&left. - &right. - 1)));
      %else %sysfunc(repeat(%str(%)),&count. - 1));
    %end;
  %end;
%end;
%mend;

%let columns = %columns;

%if &ovls. %then %do;
  * Insert ORDER Variables into &columns.;
  %do o = 1 %to &ovls.;
    %let prx = %sysfunc(prxparse(s/(^|\'?\)+\'?) *(\'?\(\'?\".*?\"\'?\(\'?)+( +%scan(&ovl_post.,
                                 &o.,%str( )) +)/\1 %scan(&ovl.,&o.,%str( )) \2\3/));
    %let columns = %sysfunc(prxchange(&prx.,&times.,&columns.));
  %end;
%end;%else %let columns = %scan(&all_vars.,1,%scan(&dvl.,1,%str( ))) &columns.;;

%let columns = &PVL_with_order. &columns.;

  columns &columns. ;
  /*%put >>&columns.<< ;*/

* 2.3.3 Pageby Define;
%if &PVLS. %then by &PVL_with_order.;;
  
* 2.3.4 Define;
%macro define_logic(which=&v.);
  %global define_logic_command;
  %if %sysfunc(indexw(%upcase(&&varop_&which..),O)) %then %do;
    %let define_logic_command = ;
    order noprint %return;
  %end;

  %if %sysfunc(indexw(%upcase(&&varop_&which..),P)) and not %sysfunc(indexw(%upcase(&&varop_&which..),O)) %then %do;
    %let define_logic_command = ;
    order noprint %return;
  %end;
  
/*   %if %sysfunc(indexw(%upcase(&&varop_&which..),S)) and not %sysfunc(indexw(%upcase(&&varop_&which..),O)) %then %do;
    %let define_logic_command = ;
     %let WDV = %eval(&WDV. + 1);
    order " " flow style(column)=[cellwidth=%scan(&width.,&WDV.,%str( ))% just=%scan(&align.,&WDV.,%str( ))] 
	%return;
  %end;
 */ 
  %if %sysfunc(indexw(%upcase(&&varop_&which..),N)) and not %sysfunc(indexw(%upcase(&&varop_&which..),O)) %then %do;
    %let define_logic_command = NOREPEAT;
     %let WDV = %eval(&WDV. + 1);
    order " " flow style(column)=[cellwidth=%scan(&width.,&WDV.,%str( ))% just=%scan(&align.,&WDV.,%str( ))] 
	%return;
  %end;
 
  %if not %length(&&varop_&which..) 
      or (%sysfunc(indexw(%upcase(&&varop_&which..),S)) and not %sysfunc(indexw(%upcase(&&varop_&which..),O))) 
  %then %do;
     %let WDV = %eval(&WDV. + 1);
     display " " flow style(column)=[cellwidth=%scan(&width.,&WDV.,%str( ))% just=%scan(&align.,&WDV.,%str( ))] 
		%return;
  %end;
%mend define_logic;

%let WDV = 0;  *Which Display Variable;
%do v = 1 %to &varnum.;
    /*%put define %scan(&all_vars.,&v.,%str( ))*/
    define %scan(&all_vars.,&v.,%str( ))
    / %define_logic
    ;
%end;

* 2.3.5 Skipby, PageByFormat or Other;
%if &SVLS. %then %do s = 1 %to &SVLS.;
    compute before %scan(&SVL.,&s.,%str( ));
      line @1 "";
    endcomp;
%end;

*If have a PageByFormat;
%let prx0 = %sysfunc(prxparse(/\$?\w+\./));
%let prx = %sysfunc(prxparse(s/.*? +(\$?\w+\.).*/\1/));
%let prx1 = %sysfunc(prxparse(s/.*? +([\'\"].*[\"\']).*/\1/));
%let count = 0;
%if &PVLS. %then %do v = 1 %to &varnum.;
  %if %sysfunc(indexw(%upcase(&&varop_&v..),P)) and not %sysfunc(indexw(%upcase(&&varop_&v..),O)) 
    and %sysfunc(prxmatch(&prx0.,&&varop_&v..)) %then %do;
    %let count = %eval(&count. + 1);
    %let pageby&count. = line @1 %sysfunc(prxchange(&prx1.,&times.,&&varop_&v..)) 
	                     %scan(&all_vars.,&v.,%str( )) %sysfunc(prxchange(&prx.,&times.,&&varop_&v..));
  %end;
%end;

%if &count. %then %do;
  compute before _page_;
    %do c = 1 %to &count.;
	  &&pageby&c..;
	%end;
    line @1 "&fline.";
  endcomp;	
%end;

run;

ods rtf close;
ods listing;

%if not %length(&debug.) %then %do;
  %if %length(&calign.) %then %do;
    proc datasets lib=work NOLIST;delete &data.;quit;
  %end;
%end;

%let tf_syserr = %eval(&syserr. - &tf_syserr.);
%if not %index(&options.,NOSUM) and &sysenv. eq FORE %then %do;
  %put RTF output %sysfunc(ifc(&tf_syserr.,may be not success.,SUCCESS!!));
  %if &tf_syserr. %then %do;
    %put Please check out the parameters below, and try again;
    %put If parameters all adhere to your code and all macro variables use right, please turn to use PROC REPORT %str(:-%();
  %end;
  %put ;
  
  %put >>>>>>>>>>>>>>>>>>>>>>>>> Summary (Use 'NOSUM' in options to close) <<<<<<<<<<<<<<<<<<<<<<<<<;
  %put ;
  %put Output path and file name: &outfile.;
  %put ;
  
  %put Headers, Titles and Footnotes:;
  %put %str(   )Headers: &Headers. Titles: &Titles. Footnotes: &Footers..;
  %put ;
  
  %put Variables:;
  %put %str(   )Variable(s) used to Display: %sysfunc(ifc(&DVLS.,&DVL.,NONE)).;
  %put %str(   )Variable(s) used to Order: %sysfunc(ifc(&OVLS.,&OVL.,NONE)).;
  %put %str(   )Variable(s) used to Pageby: %sysfunc(ifc(&PVLS.,&PVL.,NONE)).;
  %put %str(   )Variable(s) used to Skipby: %sysfunc(ifc(&SVLS.,&SVL.,NONE)).;
  %put ;

  %put >>>>>>>>>>>>>>>>>>>>>>>>> Summary (Use 'NOSUM' in options to close) <<<<<<<<<<<<<<<<<<<<<<<<<;
%end;

%mend TF_report;