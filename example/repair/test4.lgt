nu _L. (Cons(_L, _X), Cons(_Y, _L)) 
: dlist[_Y, _X] 
{ 
  dlist[_U, _V] -> { _V >< _U }, 
  dlist[_U, _V] -> { nu _L. (Cons(_L, _V), dlist[_U, _L]) }
}
