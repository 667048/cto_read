local dependencies moss egenmore
local aux moss egenmore.sthlp

foreach dependency in `dependencies' {
	
	gettoken 1 aux : aux
	
	which `1'
	if _rc == 111 {
		
		ssc install `dependency'
		
	}
	
}
