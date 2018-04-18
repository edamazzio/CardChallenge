use "project2.sml";

val test1 = if officiate ([(Clubs, Num 4)],[], 15) = 7 then 1 else 0;

val test2 = if officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
			[Draw,Draw,Draw,Draw,Draw],
                        42)
               = 3
	    then 1 else 0;


val test3 = if ( (officiate ([(Clubs, Num 1),(Spades, Ace),(Diamonds, Num 8),(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Diamonds, Queen),(Clubs, Ace),(Clubs, Jack),(Hearts, Num 7),(Diamonds, Num 5),(Spades, Num 4),(Hearts, Num 6),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw, Draw, Discard(Hearts, Num 3), Draw, Draw, Discard(Clubs, Ace), Draw, Discard(Hearts, Num 6), Discard(Spades, Num 8), Draw, Draw],
		       50);
	      false
	    )
	    handle IllegalMove => true
	       )
	    then 1 else 0;

val test4 = if officiate ([(Clubs, Num 1),(Spades, Ace),(Diamonds, Num 8),(Hearts, Ace)],
		       [Draw, Draw, Discard(Clubs, Num 1)],
		       10) = 3
	     then 1 else 0;

val test5 = if officiate ([(Clubs, Num 1),(Spades, Ace),(Diamonds, Num 8),(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Diamonds, Queen),(Clubs, Ace),(Clubs, Jack),(Hearts, Num 7),(Diamonds, Num 5),(Spades, Num 4),(Hearts, Num 6),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw, Draw, Discard(Clubs, Num 1), Draw, Draw, Discard(Hearts, Ace), Draw, Draw, Draw, Discard(Diamonds, Jack), Draw, Draw, Draw, Discard(Spades, Num 1), Draw, Draw],
		       50) = 6
	     then 1 else 0;



val test6 = if officiate ([(Clubs, Num 1),(Spades, Ace),(Diamonds, Num 8),(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Diamonds, Queen),(Clubs, Ace),(Clubs, Jack),(Hearts, Num 7),(Diamonds, Num 5),(Spades, Num 4),(Hearts, Num 6),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw, Draw, Discard(Clubs, Num 1), Draw, Draw, Discard(Hearts, Ace), Draw, Draw, Draw, Discard(Diamonds, Jack), Draw, Draw, Draw, Discard(Spades, Num 1), Draw, Draw],
		       100) = 38
	     then 1 else 0;

val test7 = if officiate([],[],0) = 0 then 1 else 0;

val test8 = if officiate ([(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Clubs, Jack),(Hearts, Num 7),(Diamonds, Num 5),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw],
		       100) = 44
	    then 1 else 0;

val test9 = if officiate ([(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Clubs, Jack),(Hearts, Num 7),(Diamonds, Num 5),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw,Discard(Hearts, Ace)],
		       50) = 25
	    then 1 else 0;

val test10 = if officiate ([(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Clubs, Jack),(Hearts, Num 7),(Diamonds, Num 5),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw, Discard(Hearts,Ace),Draw,Draw],
		       50) = 17
	     then 1 else 0;

val test11 = if officiate ([(Clubs, Num 1),(Spades, Ace),(Diamonds, Num 8),(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Diamonds, Queen),(Clubs, Ace),(Clubs, Jack),(Hearts, Num 7),(Diamonds, Num 5),(Spades, Num 4),(Hearts, Num 6),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw, Draw, Discard(Clubs, Num 1), Draw, Draw, Discard(Hearts, Ace), Draw, Draw, Draw, Discard(Diamonds, Jack), Draw, Draw, Draw, Discard(Spades, Num 1), Draw, Draw],
		       1) = 16
	     then 1 else 0;

val test12 = if officiate ([(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Clubs, Jack),(Hearts, Num 7),(Diamonds, Num 5),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw,Draw,Draw,Draw,Draw,Draw],
		       100) = 55
	     then 1 else 0;

val test13 = if (  (officiate([(Clubs,Jack),(Spades,Num(8))],
                           [Draw,Discard(Hearts,Jack)],
                           42);
		 false
		) 
               handle IllegalMove => true
		)
	     then 1 else 0;

val test14 = if officiate ([(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw,Draw,Draw,Discard(Diamonds, Num 5),Draw,Draw],
		       5) = 9
	     then 1 else 0;

val test15 = if officiate ([(Clubs, Num 1),(Spades, Ace),(Diamonds, Num 8),(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Diamonds, Queen),(Clubs, Ace),(Clubs, Jack),(Hearts, Num 7),(Diamonds, Num 5),(Spades, Num 4),(Hearts, Num 6),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw, Draw, Discard(Clubs, Num 1), Draw, Draw, Discard(Hearts, Ace), Draw, Draw, Draw, Discard(Diamonds, Jack), Draw, Draw, Draw, Discard(Spades, Num 1), Draw, Draw],
		       10) = 3
	     then 1 else 0;

val test16 = if officiate ([(Hearts, Ace),(Diamonds, Num 5),(Diamonds, Jack),(Diamonds, King),(Spades, Num 1),(Clubs, Num 8),(Hearts, Num 3),(Spades, Num 7),(Clubs, Jack),(Hearts, Num 7),(Diamonds, Num 5),(Clubs, Num 8),(Spades, Num 8)],
		       [Draw,Draw],
		       15) = 1
	     then 1 else 0;

val test17 = if officiate ([(Clubs,Ace),(Spades,Ace),(Spades,Ace)],
			[Draw,Draw],
                        120) = 49
	     then 1 else 0;

val test18 = if officiate ([(Clubs,Ace),(Spades,Ace),(Spades,Ace),(Spades,Ace),(Spades,Ace),(Spades,Ace),(Spades,Ace),(Spades,Ace),(Spades,Num 7),(Spades,Ace),(Hearts,Ace),(Spades,Jack)],
			   [Draw,Draw,Draw,Draw,Draw,Draw],35) = 13
	     then 1 else 0;

val test19 = if officiate ([(Clubs,Ace)],
			[Draw,Draw,Draw,Draw,Draw],
                        1) = 15
	     then 1 else 0;

val test20 = if officiate ([(Clubs,Ace),(Spades,Ace),(Spades,Ace)],
			[Draw,Draw,Discard(Clubs, Ace)],
                        19) = 4
	     then 1 else 0;

val obtained = test1 + test2 + test3 + test4 + test5 + test6 + test7 + test8 + test9 + test10 + test11 + test12 + test13 + test14 + test15 + test16 + test17 + test18 + test19 + test20;
val final_grade = (obtained * 100) div 20;
