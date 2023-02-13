% lltree3.lgt
% Map leaves of a leaf-linked tree with an abbreviation (`nu X1, ..., Xn. T`). 

let f[_X] = {<\x[_X]. {nu _X1 _X2. (Succ (_X1, _X), x [_X1])}>(_X)} in

let map[_X] = 
  {<\f[_X].{<\x[_L, _R, _X].
  let rec helper[_X] x2[_L, _R, _X] =
    case {Log} {x2[_L, _R, _X]} of 
      {nu _L2 _R2 _X2 _X3. (
        y [_L, _R, _X, _L2, _R2, _X2], 
        Leaf (_X3, _L2, _R2, _X2), 
        z [_X3],
        M (_L2)
      )} -> 
        let z2[_X] = {f[_X]} {z[_X]} in
        {helper[_X]}
        {nu _L2 _R2 _X2 _X3 _X4. (
          y [_L, _R, _X, _L2, _R2, _X2], 
          Leaf (_X3, _L2, _R2, _X2), 
          z2 [_X3],
          M (_R2)
        )}
    | otherwise -> case {x2[_L, _R, _X]} of
      { y[_L, _R, _X], M (_R) } -> { y[_L, _R, _X] }
    | otherwise -> {Error, x2[_L, _R, _X]}
  in {helper [_X]} {x[_L, _R, _X], M (_L)}
  >(_X)}>(_X)} in

{map[_X]}
{f [_X]}
{
nu _X1 _X2 _X3 _X4 _X5. (
  Node (_X1, _X2, _X), 
  Leaf (_X4 ,_L, _X3, _X1),
  Zero (_X4),
  Leaf (_X5, _X3, _R, _X2),
  Zero (_X5)
)
}
    

% --->
% > {nu _L0 _L1 _L2 _L3 _L4. (M (_L), Node (_L0, _L1, _X), Leaf (_L2, _L, _L3, _L0), Zero (_L2), Leaf (_L4, _L3, _R, _L1), Zero (_L4))}
% > {nu _L0 _L1 _L2 _L3 _L4 _L5. (Leaf (_L0, _L, _L1, _L2), M (_L1), Zero (_L3), Node (_L2, _L4, _X), Leaf (_L3, _L1, _R, _L4), Succ (_L5, _L0), Zero (_L5))}
% > {nu _L0 _L1 _L2 _L3 _L4 _L5 _L6. (Leaf (_L0, _L1, _R, _L2), M (_R), Zero (_L3), Succ (_L3, _L4), Leaf (_L4, _L, _L1, _L5), Node (_L5, _L2, _X), Succ (_L6, _L0), Zero (_L6))}
% {nu _L0 _L1 _L2 _L3 _L4 _L5 _L6. (Zero (_L0), Zero (_L1), Succ (_L0, _L2), Succ (_L1, _L3), Leaf (_L3, _L4, _R, _L5), Leaf (_L2, _L, _L4, _L6), Node (_L6, _L5, _X))}
