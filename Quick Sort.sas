/* 子宏 用途是将输入的值串按照标杆值进行筛选 */
%macro fliter(align,listf,sign);
%let temp = ;
%do %while(%length(&listf.));
  %if %sysevalf(%scan(&listf.,1,%str( )) &sign. &align.) %then %let temp = &temp. %scan(&listf.,1,%str( ));
  %if %sysfunc(countw(&listf.,%str( ))) > 1 %then %let listf = %substr(&listf, %length(%scan(&listf,1,%str( ))) + 1);
  %else %let listf = ;
%end;
&temp.
%mend fliter;

/* 递归主体 值串进值串出 */
%macro quick_sortx(list);
%if %length(&list.) %then %let list = %sysfunc(compbl(&list.)); * 规整格式 ;

%if %sysfunc(countw(&list.,%str( ))) <= 1 %then &list.; * 迭代落地条件 ;
%else %do;
  %quick_sortx(%fliter(%scan(&list,1,%str( )), %substr(&list,%length(%scan(&list,1,%str( ))) + 1),<)) 
  %scan(&list,1,%str( )) 
  %quick_sortx(%fliter(%scan(&list,1,%str( )), %substr(&list,%length(%scan(&list,1,%str( ))) + 1),>=))
%end;
%mend quick_sortx;

%put %sysfunc(compbl(%quick_sortx(  6 5 4   7.5 3 -7 2    10 9 7 -1.1 -1.2 6 8)));
