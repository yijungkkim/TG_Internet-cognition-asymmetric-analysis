/******************************************************************************/
/* Transitions to internet and Cognitive trajectory                           */											  */
/* January 15, 2021                                                           */
/******************************************************************************/


  use data/codedl, clear

  global main dintcumpos dintcumneg dtime 
  global tvv dmarried  dinc ddep dhlth dadll dmem
  global dtvv dintcumpos dintcumneg dtime dwork ddep dinc dhlth dlivealone dmem
  global mtvv mmarried  minc mdep mhlth madll mmem mintcumpos mintcumneg mtime     
  
  global tiv baseage female nhwhite hsless somecollege collegemore
  
  global tint c.dintcumpos c.intcumpos#c.time c.dintcumneg c.intcumneg#c.time c.dtime 
  global mtint mmarried  minc mdep mhlth madll mmem mpostime mnegtime mintcumpos mintcumneg mtime 
  
  global aint c.dintcumpos c.intcumpos#c.livealone c.dintcumneg c.intcumneg#c.livealone dtime dlivealone
  global maint mmarried  minc mdep mhlth madll mmem mlivealone mposalone mnegalone mintcumpos mintcumneg mtime 

  global cint c.dintcumpos c.intcumpos#c.warebb c.dintcumneg c.intcumneg#c.warebb dtime   
  global mcint mmarried  minc mdep mhlth madll mmem mposwarebb mnegwarebb mintcumpos mintcumneg mtime 

* Tables 
  capture program drop yktablemain
  program define yktablemain
	foreach var of local 0 {
		capture confirm numeric variable `var'
		esttab, b(2) se wide noomit lab varwidth(30) modelwidth(10) nonumber ///
		 refcat(dmarried "Timevarying cov" baseage "Timeinvariant cov" hsless "Education", nol)
		esttab using `var'.csv, order(_cons) c("b (fmt(2)) _star se(par)") ///
							 noomit label nonumber noobs nobaselevels ///
							 refcat(dintcumneg "interaction1" dtime "interaction2" ///
									dmarried "Timevarying cov" ///
									baseage "Timeinvariant cov" hsless "Education", nol) ///
							 varl(_cons "Intercept") replace 	 
	}
  end
  
  capture program drop yktableint
  program define yktableint
	foreach var of local 0 {
		capture confirm numeric variable `var'
		esttab, b(2) se wide noomit lab varwidth(30) modelwidth(10) nonumber ///
		 refcat(dmarried "Timevarying cov" baseage "Timeinvariant cov" hsless "Education", nol)
		esttab using `var'.csv, order(_cons) c("b (fmt(2)) _star se(par)") ///
							 noomit label nonumber noobs nobaselevels ///
							 refcat(dmarried "Timevarying cov" baseage "Timeinvariant cov" hsless "Education", nol) ///
							 varl(_cons "Intercept") replace 	 
	}
  end
*


* WITHIN-BETWEEN RANDOM EFFECTS
  
  eststo clear
  foreach y of varlist cog {
		eststo: mixed `y' $main $tvv $tiv $mtvv  || hhidpn: time , vce(robust) // main; Table 2
  }
  yktablemain wbre_main_0817 
						 
  eststo clear
  foreach y of varlist cog {
		eststo: mixed `y' $tint $tvv $tiv $mtint || hhidpn: time, vce(robust) // time interaction; Table 2
  }
  yktableint wbre_tint_0817

  eststo clear
  eststo: mixed cog $main $tvv $tiv $mtvv if warebb==0  || hhidpn: time, vce(robust) // cohort; Table 3
  eststo: mixed cog $main $tvv $tiv $mtvv if warebb==1  || hhidpn: time, vce(robust) // cohort; Table 3
  yktablemain wbre_cohort_0817
  
  eststo clear
  eststo: mixed cog $tint $tvv $tiv $mtint if warebb==0  || hhidpn: time, vce(robust) // cohort; Table 3
  eststo: mixed cog $tint $tvv $tiv $mtint if warebb==1  || hhidpn: time, vce(robust) // cohort; Table 3
  yktableint wbre_cohort_tint_0817

  eststo clear
  eststo: mixed cog $main $tvv $tiv $mtvv if livealone==0  || hhidpn: time, vce(robust) // not live alone; Table 4
  eststo: mixed cog $main $tvv $tiv $mtvv if livealone==1  || hhidpn: time, vce(robust) // live alone; Table 4
  yktablemain wbre_lint_0817
					
  eststo clear
  eststo: mixed cog $tint $tvv $tiv $mtint if livealone==0  || hhidpn: time, vce(robust) // not live alone; Table 4
  eststo: mixed cog $tint $tvv $tiv $mtint if livealone==1  || hhidpn: time, vce(robust) // live alone; Table 4
  yktableint wbre_lint_tint_0817

  
  
* SUPPLEMENTARY ANALYSIS
  local intvar married inc dep hlth adll mem baseage female nhwhite hsless somecollege collegemore
  forvalues i = 1/12  {
	local k: word `i' of `intvar'
	local y: word `i' of 1 2 3 4 5 6 7 8 9 10 11 12
    gen intcohort`y'=`k'*warebb
	 gen intlive`y'=`k'*livealone
   } 
  forvalues i = 1/12  {
    local y: word `i' of 1 2 3 4 5 6 7 8 9 10 11 12
	bysort hhidpn: center intcohort`y', mean(m)
    bysort hhidpn: center intlive`y', mean(m)
  }


  
  local keyvar intcumpos intcumneg time
  forvalues i = 1/3  {
	local k: word `i' of `keyvar'
	local y: word `i' of 1 2 3
	gen keycohort`y'= `k'*warebb
	gen keylive`y'=`k'*livealone
  }
    forvalues i = 1/3 {
    local y: word `i' of 1 2 3 
	bysort hhidpn: center keycohort`y', mean(m)
    bysort hhidpn: center keylive`y', mean(m)
  }
  
  gen cohortpos3=intcumpos*time*warebb
  gen cohortneg3=intcumneg*time*warebb 
  gen livepos3=intcumpos*time*livealone 
  gen liveneg3=intcumneg*time*livealone 
  bysort hhidpn: center  cohortpos3 cohortneg3 livepos3 liveneg3, mean(m)
  
  
  global mintw mintcohort1 mintcohort2 mintcohort3 mintcohort4 ///
			   mintcohort5 mintcohort6 mintcohort7 mintcohort8 ///
			   mintcohort9 mintcohort10 mintcohort11  mintcohort12
  global mintl mintlive1 mintlive2 mintlive3 mintlive4 ///
			   mintlive5 mintlive6 mintlive7 mintlive8 ///
			   mintlive9 mintlive10 mintlive11 mintlive12
  global mkeyw mkeycohort1 mkeycohort2 mkeycohort3
  global mkeyl mkeylive1 mkeylive2 mkeylive3
  
     
  
  
  **SUPPLEMENTARY TABLE 1A - COHORT MAIN
  
  eststo clear
  eststo: mixed cog $cint $tvv $tiv $mcint c.warebb || hhidpn: time, vce(robust)  //  cohort interaction; Supplementary 1
  yktableint wbre_sup1_0817
  
  eststo clear
  eststo: mixed cog c.intcumpos##c.warebb  c.intcumneg##c.warebb c.time##c.warebb ///
		    c.married##c.warebb c.inc##c.warebb c.dep##c.warebb  c.hlth##c.warebb /// 
			c.adll##c.warebb c.mem##c.warebb ///
			c.baseage##c.warebb c.female##c.warebb c.nhwhite##c.warebb ///
			c.hsless##c.warebb c.somecollege##c.warebb c.collegemore##c.warebb ///
			$mtvv ///
			$mintw $mkeyw || hhidpn: time,  vce(robust) 
  esttab using cohortdif_main_0817.csv, order(_cons) c("b (fmt(3)) p(fmt(3)) se(par)") replace // THIS!!!!
  
		
  **SUPPLEMENTARY TABLE 1B - COHORT INTERACTION
  
  eststo clear
  eststo: mixed cog c.intcumpos#c.warebb#c.time c.intcumneg#c.warebb#c.time c.warebb#c.time ///
				$cint $tvv $tiv $mcint c.warebb || hhidpn: time, vce(robust) //  cohort 3-wayinteraction; Supplementary 1b	
  yktableint wbre_sup1b_0817
  
  
  eststo clear
  eststo: mixed cog c.intcumpos##c.warebb##c.time  c.intcumneg##c.warebb##c.time ///
		    c.married##c.warebb c.inc##c.warebb c.dep##c.warebb  c.hlth##c.warebb ///
			c.adll##c.warebb c.mem##c.warebb ///
			c.baseage##c.warebb c.female##c.warebb c.nhwhite##c.warebb ///
			c.hsless##c.warebb c.somecollege##c.warebb c.collegemore##c.warebb ///
			$mtvv mpostime mnegtime mcohortpos3 mcohortneg3 ///
			$mintw $mkeyw || hhidpn:time,  vce(robust) 
  esttab using cohortdif_int_0817.csv, order(_cons) c("b (fmt(3)) p(fmt(3)) se(par)") replace // THIS!!!
 
 
  **SUPPLEMENTARY TABLE 2A - HH MAIN   
  eststo clear
  eststo: mixed cog $aint $tvv $tiv $maint || hhidpn: time, // living alone interaction; Supplementary 2
  yktableint wbre_sup2_0817
  
  eststo clear
  eststo: mixed cog c.intcumpos##c.livealone  c.intcumneg##c.livealone c.time##c.livealone ///
		    c.married##c.livealone c.inc##c.livealone c.dep##c.livealone  c.hlth##c.livealone ///
			c.adll##c.livealone c.mem##c.livealone ///
			c.baseage##c.livealone c.female##c.livealone c.nhwhite##c.livealone ///
			c.hsless##c.livealone c.somecollege##c.livealone c.collegemore##c.livealone ///
			$mtvv mlivealone ///
		    $mkeyl || hhidpn: time,  
  esttab using livedif_main_0817a.csv, order(_cons) c("b (fmt(3)) p(fmt(3)) se(par)") replace // THIS!!
  
  
   **SUPPLEMENTARY TABLE 1A - HH INTERACTION
  eststo clear
  eststo: mixed cog c.intcumpos#c.livealone#c.time c.intcumneg#c.livealone#c.time c.livealone#c.time ///
					$aint $tvv $tiv $maint || hhidpn: time, // living alone interaction; Supplementary 2
  yktableint wbre_sup2b_0817
  
  
  eststo clear
  eststo: mixed cog c.intcumpos##c.livealone##c.time  c.intcumneg##c.livealone##c.time ///
		    c.married##c.livealone c.inc##c.livealone c.dep##c.livealone  c.hlth##c.livealone ///
			c.adll##c.livealone c.mem##c.livealone ///
			c.baseage##c.livealone c.female##c.livealone c.nhwhite##c.livealone ///
			c.hsless##c.livealone c.somecollege##c.livealone c.collegemore##c.livealone ///
			$mtvv mlivealone mpostime mnegtime mlivepos3 mliveneg3 ///
		    $mkeyl || hhidpn: time,
  esttab using livdif_int_0817a.csv, order(_cons) c("b (fmt(3)) p(fmt(3)) se(par)") replace // THIS!
  


			
			
  log close
  
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  *Figures
  mixed cog c.intcumpos c.intcumpos#c.warebb c.intcumneg c.intcumneg#c.warebb time ///
			married dep inc hlth adll ///
			$tiv $mcint c.warebb || hhidpn: time,
  margins, dydx(intcumneg) at(warebb=(0 1)) // intcumneg -0.50, at warebb=0, but intcumneg=0.50, if warebb=1
  margins, at(intcumneg=(0(1)3) warebb=(0 1))		
  marginsplot, scheme(s1mono)  recast(line) recastci(rarea) title ("Figure 1a. Cessation of Internet Use x Birth Cohort") ///
			   ytitle("Predicted Level of Cognitive Functioning", size(meds) margin(small))  /// 
			    xtitle("Number of Cessations in Internet Use in Later Life", size(s) margin(small)) ///
			   legend(cols(1) order( 4 "b.1942 or later" 3 "b.1941 or earlier") ring(0) position(10) ///
			   bmargin(medium) subtitle("Birth Cohort")) ciopts(fcolor(%50) lwidth(none)) ///
			   plot1opts(lpattern(dash)) plot2opts(lpattern(solid)) 
  graph export "D:\10_DEWS\Mydata\tv_paper\NegativeMood.tif", as(tif) name("Graph") replace


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
  
* Export to Excel - Main
  preserve
  import delimited wbre_main_0816.csv, clear	 
    foreach var of varlist *{
	      replace `var' = substr(`var',3,length(`var')-3)
	  }
	  
  putexcel set Tables3.xlsx, sheet(Table2)  modify 
  forvalues i= 1/3 {
  	local w: word `i' of B C D 
	local y: word `i' of 2 3 4 
	foreach num of numlist 4(1)28 {
		putexcel `w'`num' = v`y'[`num']  	
	}
  }
  restore
  

* Export to Excel - Time interaction
  preserve
  import delimited wbre_tint_0816.csv, clear	 
    foreach var of varlist *{
	      replace `var' = substr(`var',3,length(`var')-3)
	  }
	  
  putexcel set Tables3.xlsx, sheet(Table2)  modify 
  forvalues i= 1/3 {
  	local w: word `i' of F G H
	local y: word `i' of 2 3 4
	foreach num of numlist 4(1)28 {
		putexcel `w'`num'= v`y'[`num']  	
	}
  }
  restore



* Export to Excel - Cohort stratified
  preserve 
  import delimited wbre_cohort_0816.csv, clear	 
    foreach var of varlist *{
	      replace `var' = substr(`var',3,length(`var')-3)
	  }
	  
  putexcel set Tables3.xlsx, sheet(Table3)  modify 
  forvalues i= 1/6 {
  	local w: word `i' of B C D F G H
	local y: word `i' of 2 3 4 5 6 7
	foreach num of numlist 4(1)28 {
		putexcel `w'`num'= v`y'[`num']  	
	}
  }
  restore
  
* Export to Excel - Cohort stratified_time interaction
  preserve 
  import delimited wbre_cohort_tint_0816.csv, clear	 
    foreach var of varlist *{
	      replace `var' = substr(`var',3,length(`var')-3)
	  }
	  
  putexcel set Tables3.xlsx, sheet(Table3)  modify 
  forvalues i= 1/6 {
  	local w: word `i' of J K L N O P
	local y: word `i' of 2 3 4 5 6 7
	foreach num of numlist 4(1)28 {
		putexcel `w'`num'= v`y'[`num']  	
	}
  }
  restore


* Export to Excel - Living alone stratified
  preserve
  import delimited wbre_lint_0816.csv, clear	 
    foreach var of varlist *{
	      replace `var' = substr(`var',3,length(`var')-3)
	  }
	  
  putexcel set Tables3.xlsx, sheet(Table4)  modify 
   
  forvalues i= 1/6 {
  	local w: word `i' of B C D F G H
	local y: word `i' of 2 3 4 5 6 7
	foreach num of numlist 4(1)28 {
		putexcel `w'`num'= v`y'[`num']  	
	}
  }
  restore
  
* Export to Excel - Living alone stratified
  preserve
  import delimited wbre_lint_tint_0816.csv, clear	 
    foreach var of varlist *{
	      replace `var' = substr(`var',3,length(`var')-3)
	  }
	  
  putexcel set Tables3.xlsx, sheet(Table4)  modify 
   
  forvalues i= 1/6 {
  	local w: word `i' of J K L N O P
	local y: word `i' of 2 3 4 5 6 7
	foreach num of numlist 4(1)28 {
		putexcel `w'`num'= v`y'[`num']  	
	}
  }
  restore
  

* Export to Excel - Supplementary 1
  preserve 
  import delimited wbre_sup1_0816.csv, clear	 
    foreach var of varlist *{
	      replace `var' = substr(`var',3,length(`var')-3)
	  }
	  
  putexcel set Tables3.xlsx, sheet(Supplementary1)  modify 
  forvalues i= 1/3 {
  	local w: word `i' of B C D 
	local y: word `i' of 2 3 4 
	foreach num of numlist 4(1)28 {
		putexcel `w'`num'= v`y'[`num']  	
	}
  }
  restore

* Export to Excel - Supplementary 2
  preserve 
  import delimited wbre_sup2_0816.csv, clear	 
    foreach var of varlist *{
	      replace `var' = substr(`var',3,length(`var')-3)
	  }
	  
  putexcel set Tables3.xlsx, sheet(Supplementary2)  modify 
  forvalues i= 1/3 {
  	local w: word `i' of B C D
	local y: word `i' of 2 3 4
	foreach num of numlist 4(1)29 {
		putexcel `w'`num'= v`y'[`num']  	
	}
  }
  restore


  
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

