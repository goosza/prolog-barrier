# Barrier Synchronization in Prolog 🔄

A very simple barrier synchronization implementation in Prolog. Made this for Theoretical Informatics course.

## What's a Barrier?

A barrier is a synchronization mechanism in parallel computing that coordinates multiple execution units (processes, threads) at critical points in program execution. It ensures that no execution unit proceeds beyond the barrier point until all participants have completed their previous phase of computation. This synchronization is crucial when:
- Computations are divided into distinct phases
- Each phase requires all results from the previous phase
- Data consistency must be maintained across parallel operations

## What This Implementation Does

This particular implementation demonstrates barrier synchronization using processes:
- Manages multiple parallel processes
- Shows process timing and synchronization
- Uses mutex locks for thread safety
- Hopefully gives me a credit for the course

## How to Use It

Start by loading the program:
```prolog
?- [barrier].
```

Check available commands and how everything works:
```prolog
?- help.
```

You can:
- Launch processes with `launch_processes(N).` where N is 1-3
- Monitor process status using `get_all_process_status(States).`
- Check individual processes with `get_process_status(ID, State).`
- View barrier state using `get_barrier_status(Count, Arrived).`

## Testing

There's a test suite to verify everything works:
```prolog
?- [barrier_tests].
?- run_tests.
```

## Technical Notes
- Needs SWI-Prolog with thread support
- Handles up to 3 parallel processes
- Uses basic error handling
- Shows timing information for each process

## Limitations

Once again, this is a very basic implementation meant for educational purposes:
- Only handles up to 3 processes (hardcoded but bro just change the code)
- Uses simple polling mechanism for process synchronization
- The 5-second delay is just for demonstration
- Process behavior is simulated with sleep calls
- Error handling is minimal
- No support for barrier reuse
- Not suitable for production use (what a surprise)

---
*Again, made for Theoretical Informatics course - it's just a demo to understand barrier concept!*