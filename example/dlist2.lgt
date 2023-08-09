% dlist2.lgt
% Append two difference lists.

% (\x[Y, X] y[Y, X].x[y[Y], X]) () 
%   Cons (Val1, Y, X)
%   Cons (Val2, Y, X)

{(\ x[_Y, _X]. {(\ y[_Y, _X]. {nu _Z. (x[_Z, _X], y[_Y, _Z])})})}
  {nu _X1. (Cons (_X1, _Y, _X), Val1 (_X1))}
  {nu _X1. (Cons (_X1, _Y, _X), Val2 (_X1))}

% --->
% {nu _L0. nu _L1. nu _L2. (Cons (_L1, _L0, _X), Val1 (_L1), Cons (_L2, _Y, _L0), Val2 (_L2))}
