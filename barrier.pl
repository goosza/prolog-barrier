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