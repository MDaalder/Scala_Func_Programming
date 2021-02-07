object exercise {

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    def loop(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else loop(c - 1, r - 1) + loop(c, r - 1)

    }

    loop(c, r)
  }
  println(pascal(2,5))
}
// 0th row and 0th column (0,0) val == 1

// 0th column and 1 row (0,1) val == 1 (addition of 0 & (0,0))
// 1 column and 1 row (1, 1) val val == 1 (addition of 0 & (0,0))

// 0 column and 2 row (0, 2) val == 1 (addition of 0 & (0,1))
// 1 column and 2 row (1, 2) val == 2 (addition of (0,1) & (1,1))
// 2 column and 2 row (2, 2) val == 1 (addition of (1,1) & 0)

// 0 column and 3 row (0,3) val == 1 (addition of 0 & (0,2))
// 1 column and 3 row (1,3) val == 3 (addition of (0,2) & (1,2))
// 2 column and 3 row (2,3) val == 3 (addition of (1,2) & (2,2))
// 3 column and 3 row (3,3) val == 1 (addition of (2,2) & 0)

// values of any element at 0th column == 1, and any element at c=r (i.e. (c,c)) == 1
// values of any other element == (c-1, r-1) + (c, r-1)

// need something to keep track of 2? elements at once?
// only needs vals of (c-1, r-1); (c, r-1); (c, r)
// c can never exceed r
// start at (0,0) then go to (0, 1) then to (1,1) then to (0,2) then to (1,2) then to (2,2)
// set row first, then go up columns until c == r then go to r+1 so columns iteration should be in row function

