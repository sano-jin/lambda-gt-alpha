nu _L _L1 _L2. (
  Cons(_L1, _L, _X), int[_L1], 
  Cons(_L2, _L3, _L), int[_L2],
  Cons(_L5, _L4, _L3), int[_L5], 
  Cons(_L6, _Y, _L4), int[_L6]
) 
: dlist[_Y, _X] 
{ 
  dlist[_U, _V] -> { _V >< _U }, 
  dlist[_U, _V] -> { nu _L _L1. (Cons(_L1, _L, _V), int[_L1], dlist[_U, _L]) }
}
