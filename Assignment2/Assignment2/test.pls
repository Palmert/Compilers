PLATYPUS
{
	!<this file contains tests for the transition table
	!<Illegal numbers - what we're expenting according to table
	a = 01.0; !<error 01., decimal 0
	b = 0002.0; !<error 000, float 2.0
	c = 00.0; !<error 00., decimal 0
	d = .0; !<error ., decimal 0
	e = .01; !<error ., octal 1
	f = .8; !<error ., decimal 8
	g = .; !<error .
	h = #; !<error #
	i = ←; !<unicode character ←
	j = 0..; !<expecting 0., then error token .
	k = ..; !<error . error .
	!<more unicode and ascii chars
	ïτ▼3C#E╣?,7XÜ╚Φ
	!<string literal with line breaks should work
	"
	
	
	
	
	
	
	";

}!<this comment just ends ...