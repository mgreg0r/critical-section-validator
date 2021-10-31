# Critical section violation detector
This is a university assignment to create a Prolog program that detects violation of critical sections in other programs.

Input programs are written in Prolog as shown in the example.

## Example program (Peterson's algorithm)
```variables([k]).
arrays([wants]).
program([assign(array(wants, pid), 1),
	 assign(k, pid),
         condGoto(array(wants, 1-pid) = 0, 5),
	 condGoto(k = pid, 3),
         sekcja,
	 assign(array(wants, pid), 0),
	 goto(1)]).```

