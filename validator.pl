ensure_loaded(library(lists)).

:-op(700, xfx, <>).

initArray(0, []).
initArray(N, [0|T]) :-
  N1 is N - 1,
  initArray(N1, T).

% State initialization and helper predicates
% State representation: (Variables, Pointers)
% Variables - list of pairs (Name, Value).
% Name can be a variable or array (array(Name)),
% Value of a variable is a single integer, and Value of an array is a list of integers.
% Pointers - list that stores number of currently executed instruction by process i

initVariables(variables([]), []).
initVariables(variables([X|Tail]), [(X, 0)|VState]) :-
  initVariables(variables(Tail), VState).

initArrays(arrays([]), _, []).
initArrays(arrays([X|Tail]), N, [(array(X), EmptyArray)|VState]) :-
  initArray(N, EmptyArray),
  initArrays(arrays(Tail), N, VState).

initState(Variables, Arrays, N, (VState, PState)) :- 
  initVariables(Variables, VariableState),
  initArrays(Arrays, N, ArrayState),
  append(VariableState, ArrayState, VState),
  initArray(N, PState).

getVariables((X, _), X).
getPointers((_, X), X).


% counterMoved(Prev, Pid, Next)
% true, when Next counter state differs from Prev counter state
% only by process Pid moving on by one instruction.

counterMoved([H|T], 0, [N|T2]) :- 
  N is H + 1,
  T = T2.
counterMoved([H|T], Pid, [H|T2]) :-
  NPid is Pid - 1,
  counterMoved(T, NPid, T2).

% setCounter(Next, Pid, Value, Next)
% true, when Next counter state differs from Prev counter state
% only by process Pid jumping to the instruction numbered Value - 1

setCounter([_|T], 0, Value, [ShiftValue|T]) :- ShiftValue is Value - 1.
setCounter([H|T], PId, Value, [H|T2]) :-
  PId > 0,
  NPid is PId - 1,
  setCounter(T, NPid, Value, T2).


% getVarValue(VState, Var, VState, Value)
% Gets value of a variable named Var.
% Var can be take the form of array(Name, Index)

getVarValue([(Var, Value)|_], Var, _, Value) :- not(Var = array(_, _)).
getVarValue([(Name, _)|T], Var, VState, Res) :-
  not(Name = Var),
  not(Var = array(_, _)),
  getVarValue(T, Var, VState, Res).

getVarValue([(array(Name), AValue)|_], array(Name, Index), VState, Result) :-
  evalArithm(Index, VState, IndexValue),
  nth0(IndexValue, AValue, Result).

getVarValue([(Name, _)|T], array(AName, Index), VState, Result) :-
  not(Name = array(AName)),
  getVarValue(T, array(AName, Index), VState, Result).


% expressions evaluation

evalSimple(N, _, N) :- integer(N).
evalSimple(N, VState, Res) :-
  getVarValue(VState, N, VState, Res).

evalArithm(A, VState, N) :- evalSimple(A, VState, N).
evalArithm(A + B, VState, Value) :-
  evalSimple(A, VState ,AValue),
  evalSimple(B, VState, BValue),
  Value is AValue + BValue.

evalArithm(A - B, VState, Value) :-
  evalSimple(A, VState ,AValue),
  evalSimple(B, VState, BValue),
  Value is AValue - BValue.

evalArithm(A * B, VState, Value) :-
  evalSimple(A, VState ,AValue),
  evalSimple(B, VState, BValue),
  Value is AValue * BValue.

evalArithm(A / B, VState, Value) :-
  evalSimple(A, VState, AValue),
  evalSimple(B, VState, BValue),
  Value is AValue // BValue.
  
evalBool(A < B, VState) :-
  evalSimple(A, VState, AValue),
  evalSimple(B, VState, BValue),
  AValue < BValue.

evalBool(A = B, VState) :-
  evalSimple(A, VState, AValue),
  evalSimple(B, VState, BValue),
  AValue = BValue.

evalBool(A <> B, VState) :-
  evalSimple(A, VState, AValue),
  evalSimple(B, VState, BValue),
  not(AValue = BValue).


% assignArray(Prev, I, Value, Next)
% Assings Value to the Prev array at I index
assignArray([_|T], 0, Value, [Value|T]).
assignArray([H|T], N, Value, [H|Res]) :-
  N1 is N - 1,
  assignArray(T, N1, Value, Res).


% doAssign(Name, Value, VState, VState, ResultVState)
% Assings Value to a variable named Name
% Name can take the form of array(Array, Index)
doAssign(_, _, _, [], []).
doAssign(Name, Value, _, [(Name, _)|T], [(Name, Value)|T]) :- not(Name = array(_, _)).
doAssign(Name, Value, VState, [(Name2, Value2)|T], Res) :-
  not(Name = Name2),
  not(Name = array(_, _)),
  doAssign(Name, Value, VState, T, TRes),
  Res = [(Name2, Value2)|TRes].

doAssign(array(AName, Index), Value, VState, [(array(AName), AValue)|T], [(array(AName), Res)|T]) :-
  evalArithm(Index, VState, IndexValue),
  assignArray(AValue, IndexValue, Value, Res).

doAssign(array(AName, Index), Value, VState, [(Name, OtherValue)|T], [(Name, OtherValue)|Res]) :-
  not(Name = array(AName)),
  doAssign(array(AName, Index), Value, VState, T, Res).


% exec(Instr, State, Pid, NextState)
% Executes statement Instr by Pid process
exec(assign(Name, Value), State, Pid, NextState) :-
  getVariables(State, VState),
  getPointers(State, PState),
  evalArithm(Value, [(pid, Pid)|VState], EValue),
  doAssign(Name, EValue, [(pid, Pid)|VState], VState, NextVState),
  counterMoved(PState, Pid, NextPState),
  getVariables(NextState, NextVState),
  getPointers(NextState, NextPState).

exec(goto(N), State, Pid, NextState) :-
  getVariables(State, VState),
  getPointers(State, PState),
  getVariables(NextState, VState),
  setCounter(PState, Pid, N, NextPState),
  getPointers(NextState, NextPState).

exec(condGoto(E, N), State, Pid, NextState) :-
  getVariables(State, VState),
  getPointers(State, PState),
  evalBool(E, [(pid, Pid)|VState]),
  getVariables(NextState, VState),
  setCounter(PState, Pid, N, NextPState),
  getPointers(NextState, NextPState).

exec(condGoto(E, _), State, Pid, NextState) :-
  getVariables(State, VState),
  getPointers(State, PState),
  not(evalBool(E, [(pid, Pid)|VState])),
  getVariables(NextState, VState),
  counterMoved(PState, Pid, NextPState),
  getPointers(NextState, NextPState).

exec(sekcja, State, Pid, NextState) :-
  getVariables(State, VState),
  getVariables(NextState, VState),
  getPointers(State, PState),
  counterMoved(PState, Pid, NextPState),
  getPointers(NextState, NextPState).


% Executes next statement by process Pid
step(Instructions, State, Pid, NextState) :-
  getPointers(State, PState),
  nth0(Pid, PState, InstructionNumber),
  nth0(InstructionNumber, Instructions, Instruction),
  exec(Instruction, State, Pid, NextState).


processInSection(Instructions, InstructionNumber) :-
  nth0(InstructionNumber, Instructions, sekcja).

countProcessInSection(_, [], _, 0, []).
countProcessInSection(Instructions, [CurrentInstruction|T], Counter, Res, [Counter|Indexes]) :-
  processInSection(Instructions, CurrentInstruction),
  NewCounter is Counter + 1,
  countProcessInSection(Instructions, T, NewCounter, Rest, Indexes),
  Res is Rest + 1.

countProcessInSection(Instructions, [CurrentInstruction|T], Counter, Res, Indexes) :-
  not(processInSection(Instructions, CurrentInstruction)),
  NewCounter is Counter + 1,
  countProcessInSection(Instructions, T, NewCounter, Res, Indexes).

isStateUnsafe(Instructions, State, Indexes) :-
  getPointers(State, PState),
  countProcessInSection(Instructions, PState, 0, Count, Indexes),
  Count > 1.

% Generates a state that was not visited
genState(Instructions, Visited, Paths, Next, NextPath) :-
  nth0(Index, Visited, Start),
  step(Instructions, Start, PId, Next),
  not(member(Next, Visited)),
  nth0(Index, Paths, StartPath),
  NextPath = [PId|StartPath],
  !.

searchStates(Instructions, Visited, Paths, [Next|Res], [NextPath|PRes]) :-
  last(Visited, Last),
  not(isStateUnsafe(Instructions, Last, _)),
  genState(Instructions, Visited, Paths, Next, NextPath),
  append(Visited, [Next], NextVisited),
  append(Paths, [NextPath], NextPaths),
  searchStates(Instructions, NextVisited, NextPaths, Res, PRes).

searchStates(_, _, _, [], []).

findUnsafe(Instructions, [H|_], [H2|_], Path, Indexes) :-
  isStateUnsafe(Instructions, H, Indexes),
  Path = H2.

findUnsafe(Instructions, [H|T], [_|T2], Path, Indexes) :-
  not(isStateUnsafe(Instructions, H, _)),
  findUnsafe(Instructions, T, T2, Path, Indexes).

verify :-
  read(N),
  read(Filename),
  verify(N, Filename).

verify(N, _) :-
  N < 1,
  write('Error: parameter 0 should be an integer > 0\n'),
  !.

verify(_, Filename) :-
  not(exists_file(Filename)),
  write('Error: no such file - '),
  write(Filename),
  write('\n'),
  !.

verify(N, Filename) :-
  see(Filename),
  read(Variables),
  read(Arrays),
  read(Instructions),
  seen(),
  initState(Variables, Arrays, N, State),
  program(Instr) = Instructions,
  searchStates(Instr, [State], [[]], ReachableStates, Paths),
  (findUnsafe(Instr, ReachableStates, Paths, RevPath, Indexes) ->
          reverse(RevPath, Path),
          write('Program is not valid.\nInvalid flow: '),
          write(Path),
          write('\n'),
          write('Process in section: '),
          write(Indexes),
          write('\n')
        ;
          write('Program is valid (safe).\n')
  ),
  !.

