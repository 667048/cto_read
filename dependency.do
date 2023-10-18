local dependencies moss egenmore

foreach dependency in `dependencies' {
	
	which `dependency'
	if _rc == 111 {
		
		ssc install `dependency'
		
	}
	
}
