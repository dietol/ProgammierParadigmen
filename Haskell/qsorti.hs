qsorti [] = []
qsorti (p:ps) = (qsorti [x | x <- ps, x <= p]) ++p: (qsorti [x | x <- ps, x > p])

