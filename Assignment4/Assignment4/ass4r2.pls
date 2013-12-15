!<This is a syntactically correct PLATYPUS program
!<Weiler's law:
!<"Nothing is impossible for the man who doesn't have to do it himself."
!<"Parsing is passing." S^R & Compilers' law

PLATYPUS{
 a=1.0;
 b=+0.;
 INPUT(c);
 INPUT(d,e,f);
 c=((d+e)/a)*f-(((d-e)*a)/f);
 OUTPUT(c);
 USING(a = a,a!=b .OR. c==d .AND. e<f .OR. a>0,c = e)REPEAT{
  
    OUTPUT("HERE");
	a = -1.0;
 };   
 OUTPUT();
 OUTPUT("Results: ");
 OUTPUT(d,e,f,a);
}