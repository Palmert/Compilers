!<Float Arithmetic Test
PLATYPUS { 
!<Expected Result: 31.0
aExpected = 31.0;
bExpected = 3.0;

a = 5.0 * 6.0 + 5.0 - 4.0;
OUTPUT("The current value of a is: "); OUTPUT(a); OUTPUT("
");
!<Expected Result: 3.0
b = 30.0 / 5.0 + 4.0 - 7.0;
OUTPUT("The current value of b is: "); OUTPUT(b); OUTPUT("
");
IF(a == aExpected .AND. b == bExpected)
THEN OUTPUT("Results match expected values
");
ELSE{
OUTPUT("Results do not match. Arithmetic error
");
};
}

 
