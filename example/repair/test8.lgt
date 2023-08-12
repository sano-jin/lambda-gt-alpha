nu _X1 _X2 _X3 _X4 _X5 _X6 _X7
   _X8 _X9 _X10 _X11 _X12 _X13 _X14 _X15
   _L1 _L2 _L3 _L4 _L5 _L6 _L7 _L8 
   _L8 _L9 _L10 _L11 _L12 _L13 _L14 _L15 _L16. (
  Cons(_L1,  _X1,  _X),   int[_L1], 
  Cons(_L2,  _X2,  _X1),  int[_L2],
  Cons(_L3,  _X3,  _X2),  int[_L3], 
  Cons(_L4,  _X4,  _X3),  int[_L4],
  Cons(_L5,  _X5,  _X4),  int[_L5], 
  Cons(_L6,  _X6,  _X5),  int[_L6],
  Cons(_L7,  _X7,  _X6),  int[_L7], 
  Cons(_L8,  _X8,  _X7),  int[_L8],
  Cons(_L9,  _X9,  _X8),  int[_L9], 
  Cons(_L10, _X10, _X9),  int[_L10],
  Cons(_L11, _X11, _X10), int[_L11], 
  Cons(_L12, _X12, _X11), int[_L12],
  Cons(_L13, _X13, _X12), int[_L13], 
  Cons(_L14, _X14, _X13), int[_L14],
  Cons(_L15, _X15, _X14), int[_L15], 
  Cons(_L16, _Y,   _X15), int[_L16]
)
: dlist[_Y, _X] 
{ 
  dlist[_U, _V] -> { _V >< _U }, 
  dlist[_U, _V] -> { nu _L _L1. (Cons(_L1, _L, _V), int[_L1], dlist[_U, _L]) }
}
