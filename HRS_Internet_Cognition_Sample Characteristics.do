  use des, clear
  
* Sample characteristics
  eststo clear
  eststo: estpost su cog intw ///
				   baseage female married livealone $race $edu ///
				   inc hlth adll dep 

  esttab, cells("mean (fmt(2)) sd (fmt(2)) min max") label varwidth(30) ///
		  refcat(baseage "Controls" nhwhite "Race" hsless "education", nol)
  esttab using descriptive.csv, cells("mean (fmt(2)) sd (par fmt(2)) min max") ///
		 refcat(baseage "Controls" nhwhite "Race" hsless "education", nol) ///
		 label varwidth(30) replace
		 
* Import back to STATA	 
  import delimited descriptive.csv, clear	 
    foreach var of varlist *{
	      replace `var' = substr(`var',3,length(`var')-3)
	  }

* Export to Excel
  putexcel set Tables3.xlsx, sheet(Table1)  modify
  
  forvalues i= 1/4 {
  	local w: word `i' of B C D E
	local y: word `i' of 2 3 4 5
	foreach num of numlist 2(1)22 {
		putexcel `w'`num'= v`y'[`num'+2]  	
	}
  }

    
* Sample characteristics - by cohort
  use des, clear

  eststo clear
  eststo: estpost su cog intw ///
				   baseage female married livealone $race $edu ///
				   inc hlth adll dep if warebb==0 & 
  eststo: estpost su cog intw ///
				   baseage female married livealone $race $edu ///
				   inc hlth adll dep if warebb==1
				   
  esttab, cells("mean (fmt(2)) sd (fmt(2)) min max") label varwidth(30) ///
		  refcat(baseage "Controls" nhwhite "Race" hsless "education", nol)
  esttab using descriptive_supp1.csv, cells("mean (fmt(2)) sd (par fmt(2)) min max") ///
		 refcat(baseage "Controls" nhwhite "Race" hsless "education", nol) ///
		 label varwidth(30) replace
		 
  foreach var in  cog baseage inc  hlth adll dep {
  	ttest `var', by(warebb)
  }
  
  foreach var in intw female married livealone ///
				  $race $edu {
  tab `var' warebb, ch
				  }
				  
* Sample characteristics - by living status
  use des, clear
  eststo clear
  eststo: estpost su cog intw ///
				   baseage female married livealone $race $edu ///
				   inc hlth adll dep if livealone==0 
  eststo: estpost su cog intw ///
				   baseage female married livealone $race $edu ///
				   inc hlth adll dep if livealone==1
				   
  esttab, cells("mean (fmt(2)) sd (fmt(2)) min max") label varwidth(30) ///
		  refcat(baseage "Controls" nhwhite "Race" hsless "education", nol)
  esttab using descriptive_supp2.csv, cells("mean (fmt(2)) sd (par fmt(2)) min max") ///
		 refcat(baseage "Controls" nhwhite "Race" hsless "education", nol) ///
		 label varwidth(30) replace
		 
  foreach var in  cog baseage inc dep adll  hlth {
  	ttest `var', by(livealone)
  }
  
  foreach var in intw female married livealone ///
				  $race $edu {
  tab `var' livealone, ch
				  }