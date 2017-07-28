	subroutine filltab()

	integer dimx, dimy
	parameter (dimx = 8, dimy = 6)
	real ftab(dimx, dimy)
	common /ftab/ ftab
	integer x, y

	do 100 x = 1, dimx
	   do 110 y = 1, dimy
	      ftab(x, y) = x * 100 + y
 110	   continue
 100	continue
	end

	subroutine printtab(tab, dimx, dimy)

	integer dimx, dimy
	real tab(dimx, dimy)
	integer x, y

	do 200 x = 1, dimx
	   print 300, x, (tab(x, y), y = 1, dimy)
 300	   format(/1X, I3, 2X, 10F6.1/)
 200	continue
	end
