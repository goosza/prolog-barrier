% Load main implementation
:- [barrier].

%%% Core Functionality Tests %%%

% Test barrier initialization
test_init :-
    format('Testing barrier initialization... '),
    init_barrier(3),
    get_barrier_status(BarrierCount, ArrivedCount),
    get_all_process_status(States),
    (BarrierCount =:= 3,
     ArrivedCount =:= 0,
     States = [ready, ready, ready] ->
        format('OK~n')
    ;
        format('FAILED~n')
    ),
    cleanup_barrier.

% Test process counting
test_process_count :-
    format('Testing process count validation... '),
    process_count(Count),
    (Count =:= 3 ->
        format('OK~n')
    ;
        format('FAILED~n')
    ).

% Test process synchronization
test_sync :-
    format('Testing process synchronization... '),
    cleanup_barrier,
    init_barrier(3),
    get_time(StartTime),
    forall(between(1, 3, ID),
           thread_create(process_behavior(ID), _, [detached(true)])),
    sleep(9),  % Wait for completion (2s work + 5s barrier + 1s final + buffer)
    get_all_process_status(States),
    get_time(EndTime),
    TestDuration is EndTime - StartTime,
    (forall(member(State, States), State = finished) ->
        format('OK (completed in ~2f seconds)~n', [TestDuration])
    ;
        format('FAILED - States: ~w (after ~2f seconds)~n', [States, TestDuration])
    ),
    cleanup_barrier.

test_errors :-
    format('Testing error handling...~n'),
    
    % Test invalid process count
    format('  - Invalid process count: '),
    (init_barrier(4) ->
        format('FAILED~n')
    ;
        format('OK~n')
    ),
    
    % Test zero processes
    format('  - Zero processes: '),
    cleanup_barrier,
    (init_barrier(0) ->
        format('FAILED~n')
    ;
        format('OK~n')
    ),
    cleanup_barrier.

%%% Run all tests %%%
run_tests :-
    format('~n=== Running Barrier Implementation Tests ===~n~n'),
    test_init,
    test_process_count,
    test_errors,
    format('~n=== Running Synchronization Test ===~n'),
    test_sync,
    format('~nAll tests completed.~n').