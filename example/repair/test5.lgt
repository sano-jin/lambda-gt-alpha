nu _L _L1 _L2. (Cons(_L1, _L, _X), int[_L1], Cons(_L2, _Y, _L), int[_L2]) 
: dlist[_Y, _X] 
{ 
  dlist[_U, _V] -> { _V >< _U }, 
  dlist[_U, _V] -> { nu _L _L1. (Cons(_L1, _L, _V), int[_L1], dlist[_U, _L]) }
}
