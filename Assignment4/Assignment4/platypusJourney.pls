!< About to embark on an epic journey deep into the heart of Platypus nesting
!< Have you prepared
PLATYPUS { 
a = 0;
b = 0;
c = 0;
d = 0;
e = 0;
f = 0;
g = 0;
h = 0;
i = 0;
j = 0;
k = 0;
l = 0;
m = 0;

rope# = "Coil of rope";
shovel# = "A Large shovel"
food# = "A chocolate bar";
weapon# = "A butter knife and fork";
fire# = "Matches"
bedding# = "A fluffy pillow"


OUTPUT("You are about to embark on an epic journey to defeat the legendary Dragon
");
OUTPUT("Time to gather your supplies.
You only have enough room for 5 things. What will you bring?
");
USING( index = 0, index < 5, index = index + 1) REPEAT
{
	IF(index == 0) THEN 
	OUTPUT("Choose your first item:
	1. Coil of rope
	2. Large shovel
	3. A chocolate bar
	4. A butter knife and fork
	5. Matches
	6. ");
OUTPUT("Make y
1. Yes, I am ready.
2. No, get me out of here!
");
INPUT(y);
IF(y == 1) THEN
USING(a = a + 1, a < 4, a = a + 1) REPEAT
{
OUTPUT("1st Nested Loop
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
IF(z<5)THEN OUTPUT("Z LESS THAN 5");
OUTPUT("
");
ELSE {OUTPUT("GREATER THAN 5");
OUTPUT("
");};
	USING(b = b + 1, b < 4, b = b + 1) REPEAT
{
OUTPUT("1st Inner loop");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
		USING(c = c + 1, c < 4, c = c+ 1) REPEAT
{
OUTPUT("2nd Inner Loop!");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
			USING(d = d + 1, d < 4, d = d + 1) REPEAT
{
OUTPUT("3rd Inner Loop!");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
				USING(e = e + 1, e < 4, e = e + 1) REPEAT
{
OUTPUT("4th Inner Loop!");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
					USING(f = f + 1, f < 4, f = f + 1) REPEAT
{
OUTPUT("5th Inner Loop!");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
						USING(g = g + 1, g < 4, g = g + 1) REPEAT
{
OUTPUT("6th Inner Loop!");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
							USING(h = h + 1, h < 4, h = h + 1) REPEAT
{
OUTPUT("7th Inner Loop");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
								USING(i = i + 1, i < 4, i = i + 1) REPEAT
{
OUTPUT("8 Inner Loop!");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
									USING(j = j + 1, j < 4, i = i + 1) REPEAT
{
OUTPUT("9 Inner Loop!");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
										USING(k = k + 1, k < 4, k = k + 1) REPEAT
{
OUTPUT("10th Inner Loop!");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
											USING( l = l + 1, l < 4, l = l + 1) REPEAT
{
OUTPUT("11 Inner Loop!");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
												USING(m = m + 1, m < 4, m = m + 1) REPEAT
{
OUTPUT("12th Inner Loop!");
OUTPUT("
");
z = a + b + c + d + e + f + g;
OUTPUT(z);
OUTPUT("
");
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
ELSE{ OUTPUT("Very well. Goodbye");};
}
