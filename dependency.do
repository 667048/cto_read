local dependencies moss egenmore elabel findregex
local aux moss egenmore.sthlp elabel findregex

foreach dependency in `dependencies' {
	
	gettoken 1 aux : aux
	
	which `1'
	if _rc == 111 {
		
		ssc install `dependency'
		
	}
	
}
