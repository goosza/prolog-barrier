%%% Static Facts %%%

% Each process(N) represents one process with ID N
process(1).
process(2).
process(3).

% All possible states of processes
process_state(ready).
process_state(waiting).
process_state(finished).

%%% Add thread support %%%
:- use_module(library(thread)).
:- mutex_create(barrier_mutex).

%%% Dynamic Facts %%%
% barrier_count(N) - how many processes we wait for
% arrived_count(N) - how many processes arrived
% process_status(ID, State) - state of process with ID
:- dynamic barrier_count/1.
:- dynamic arrived_count/1.
:- dynamic process_status/2.

%%% Init predicate %%%
init_barrier(Total) :-
    process_count(Count),
    Total =< Count,

    % Remove old states if exist
    retractall(barrier_count(_)),
    retractall(arrived_count(_)),
    retractall(process_status(_, _)),

    assertz(barrier_count(Total)),
    assertz(arrived_count(0)),

    init_process_states.

% Helper to count total number of processes
process_count(Count) :-
    findall(X, process(X), Processes),
    length(Processes, Count).

% Set each process to ready state
init_process_states :-
    process(ID),
    assertz(process_status(ID, ready)),
    fail.
init_process_states.

%%% Status check predicates %%%
% Get status of specific process
get_process_status(ID, State) :-
    process(ID),
    process_status(ID, State).

% Get barrier status
get_barrier_status(BarrierCount, ArrivedCount) :-
    barrier_count(BarrierCount),
    arrived_count(ArrivedCount).

% Get all process statuses
get_all_process_status(States) :-
    findall(State, process_status(_, State), States).

%%% Barrier Implementation %%%
%%% Process arrival at barrier %%%
% Handle process arriving at barrier
arrive_at_barrier(ID) :-
    with_mutex(barrier_mutex, (
        % Check if valid process and in ready state
        process(ID),
        process_status(ID, ready),
        
        % Update process state to waiting
        retract(process_status(ID, ready)),
        assertz(process_status(ID, waiting)),
        
        % Increment arrived count
        retract(arrived_count(Current)),
        NewCount is Current + 1,
        assertz(arrived_count(NewCount)),
        
        % Check if all processes arrived
        barrier_count(Total),
        (NewCount = Total -> 
            release_processes
        ;
            true
        )
    )).

% Release all processes when barrier is complete
release_processes :-
    % Change all waiting processes to finished
    forall(
        process_status(ID, waiting),
        (
            retract(process_status(ID, waiting)),
            assertz(process_status(ID, finished))
        )
    ).

%%% Process behavior with timing %%%
process_behavior(ID) :-
    get_time(StartTime),
    format('Process ~w: Starting at ~3f seconds~n', [ID, StartTime]),
    sleep(2),  % Simulate some work
    
    get_time(ArriveTime),
    format('Process ~w: Arriving at barrier at ~3f seconds~n', [ID, ArriveTime]),
    arrive_at_barrier(ID),
    
    get_time(PassTime),
    format('Process ~w: Passed barrier at ~3f seconds~n', [ID, PassTime]),
    sleep(1),  % More work after barrier
    
    get_time(EndTime),
    format('Process ~w: Finished at ~3f seconds~n', [ID, EndTime]).

%%% Launch processes %%%
launch_processes(N) :-
    init_barrier(N),
    forall(between(1, N, ID),
           thread_create(process_behavior(ID), _, 
                        [detached(true)])).