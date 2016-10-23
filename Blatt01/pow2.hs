pow2 b e = 
	if (e==0)
	then 1
	else 	if ((mod e 2) == 1)
		then b * pow2 (pow2 b 2) ((e-1)/2)
		else b * pow2 b (e-1)
