nu _X1 _X2 _X3 _X4 _X5 _X6 _X7
   _L1 _L2 _L3 _L4 _L5 _L6 _L7 _L8. (
  Cons(_L1, _X1, _X), int[_L1], 
  Cons(_L2, _X2, _X1), int[_L2],
  Cons(_L3, _X3, _X2), int[_L3], 
  Cons(_L4, _X4, _X3), int[_L4],
  Cons(_L5, _X5, _X4), int[_L5], 
  Cons(_L6, _X6, _X6), int[_L6],
  Cons(_L7, _X7, _X6), int[_L7], 
  Cons(_L8, _Y, _X7), int[_L8]
) 
: dlist[_Y, _X] 
{ 
  dlist[_U, _V] -> { _V >< _U }, 
  dlist[_U, _V] -> { nu _L _L1. (Cons(_L1, _L, _V), int[_L1], dlist[_U, _L]) }
}
