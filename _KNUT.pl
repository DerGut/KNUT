%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% General Game Player environment for
%% Methods of AI 2009
%% Helmar Gust (c) 2009

%% KNUT implementation
%% by Frederike Jung, Aaron Reer, Jannik Steinmann and Joshua Wiebe
%% {fjung, areer, jsteinmann, jwiebe}@uos.de

:- module('_KNUT',['ggp__KNUT_player'/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%  ___    __   _____   __        ___    __    __   ____ 
%% | | \  / /\   | |   / /\      | |_)  / /\  ( (` | |_  
%% |_|_/ /_/--\  |_|  /_/--\     |_|_) /_/--\ _)_) |_|__ 

%% Stores the role _KNUT received.
:- dynamic(role_data/1).

%% Our main data storage.
%% knut_data holds the expanded node including the amount of visits and wins
%% of a simulation and its hash key.
%% knut_data(?Hash_Key, ?State, ?Visits, ?Wins)
:- dynamic(knut_data/4).

%% Controls the strength of pruning.
%% High values lead to a broad search over unexplored nodes.
%% Low values lead to a denser search focusing on nodes with a high 
%% wins per visits ratio.
:- dynamic(parameter_UCT_data/1).

%% Stores the current best next state our search has concluded.
:- dynamic(current_best_state_data/1).



%%  ___   ___   _     _   __     __   _____  ___   ___   _        
%% / / \ | |_) | |   | | / /`_  / /\   | |  / / \ | |_) \ \_/     
%% \_\_/ |_|_) |_|__ |_| \_\_/ /_/--\  |_|  \_\_/ |_| \  |_|      
%% 		 ___   ___   ____  ___   _   __     __   _____  ____  __  
%% 		| |_) | |_) | |_  | | \ | | / /`   / /\   | |  | |_  ( (` 
%% 		|_|   |_| \ |_|__ |_|_/ |_| \_\_, /_/--\  |_|  |_|__ _)_) 

%% Initializes the player.
%% The player starts with a high UCT parameter in order to get a broad overview
%% of the game.
ggp__KNUT_player([], Role, _Move, _Time, _MovesList) :-
	assert(parameter_UCT_data(40)),
	assert(role_data(Role)).

%% Selects move if it is a direct winning move (first if multiple are present).
ggp__KNUT_player(State, Role, Move, _Time, _MovesList) :-
	findall(Role:M, gdl_legal(Role,M,State), Possible_Moves),
	findall(Winning_Move,(
		member(Winning_Move,Possible_Moves),
		ggp_next_state(State,[Winning_Move],Terminal_State),
		gdl_terminal(Terminal_State),
		gdl_goal(Role,Utility,Terminal_State),
		Utility > 50),
	Winning_Moves),
	Winning_Moves = [_|_],
	ggp_db(4,'Winning_Moves':Winning_Moves),
	nth0(0, Winning_Moves, Move).

%% Starts search the search for a best move
ggp__KNUT_player(State, Role, Move, _Time, _MovesList) :-

	%% Adds the initial state into our data base.
	term_hash(State, HashKey),
	assert(knut_data(HashKey, State, 1, 0)),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	search(Role, State), % The heart of KNUT

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Reads the best succeeding state determined by search from data base.
	current_best_state_data(Best_State),

	%% Translates the current and succeeding states into a move.
	gdl_legal([Move | Moves_Opponents], State),
	ggp_next_state(State, [Move | Moves_Opponents], Best_State),

	%% Output for debugging
	knut_data(_, Best_State, Visits, Wins),
	ggp_db(1, 'Best - Visits':Visits:'Wins':Wins).



%%  __   ____   __    ___   __    _    
%% ( (` | |_   / /\  | |_) / /`  | |_| 
%% _)_) |_|__ /_/--\ |_| \ \_\_, |_| | 

%% 
%% Initializes the monte carlo tree search.
%% 
%% The search repeats itself, calling the monte carlo algorithm again and again
%% until a time limit of 9/10 is reached.
%% 
%% In each iteration the algorithm performs the steps of 
%%	   1) Selecting a node that corresponds to the criteria of our UCT-function
%% 	   2) Expanding this node and appending it to the data base
%% 	   3) Simulating a random course of the game 
%%       and
%% 	   4) Back-propagating the result of this course (win, loss or draw)
%% 
%% after which the current best succeeding state is determined and updated in
%% our data base.
%% 
%% When the runtime of the repetitions reaches certain checkpoints of elapsed
%% time the parameter of our UCT-function is set down. 
%% This enables us to focus the search on more relevant branches of our search
%% tree when the time gets short.
%% 
%% See our documentation for a detailed explanation of search algorithm.
%% 
%% search(+Role, +State)
%% 
search(Role, State) :-

	%% Repeats the monte carlo tree search until time is (almost) over.
	repeat,
	ignore((

		%% The core of the search predicate. 
		monte_carlo(Role, State, _Expansion),

		%% Chooses best succeeding state by evaluating the ratio of wins.

		%% Updates the best succeeding state in our data base.
		choose_best_state(Role, State, Best_State),
		(current_best_state_data(Current_State) ->
			retractall(current_best_state_data(Current_State))
			;
			ggp_db(4, 'Update of Best_State':Best_State)
		),
		assert(current_best_state_data(Best_State)),
		knut_data(_, Best_State, Visits, Wins),
		ggp_db(2,'Checkpoint !!!!! Asserts new best state':Visits:Wins:State),

		%% Checkpoint 2 - half of time is elapsed
		(time_2 ->
			retractall(parameter_UCT_data(_)),
			assert(parameter_UCT_data(15))
			;
			%% Checkpoint 1 - quarter of time is elapsed
			(time_4 ->
				retractall(parameter_UCT_data(_)),
				assert(parameter_UCT_data(20))
			)
		)
	)),
	time_9_10, % Time is almost completely gone - quit repetition
	ggp_db(2, 'Exits loop'), !.



%%  _      ___   _     _____  ____      __     __    ___   _     ___  
%% | |\/| / / \ | |\ |  | |  | |_      / /`   / /\  | |_) | |   / / \ 
%% |_|  | \_\_/ |_| \|  |_|  |_|__     \_\_, /_/--\ |_| \ |_|__ \_\_/ 

%% 
%% The monte carlo tree search algortithm itself.
%% 
%% It has no relevant output (it updates the data base)
%% 
%% 
%% 
%% EXPANSION & SIMULATION
%% 
%% Base case.	
%% If at least one of the successor states of the given state are unknown to 
%% the data base monte carlo chooses a random state to expand. It uses a depth
%% first search like random algorithm in order to derive a possible outcome
%% of the expansion.
%% 
%% The expansion with its derived outcome and a counter for the first visit are
%% added to the data base.
%% 
%% monte_carlo(+Role, +State, -Expansion)
%% 
monte_carlo(Role, State, Expansion) :-
	
	%% EXPANSION

	%% Finds all possible moves from the given state.
	findall(Role:M, gdl_legal(Role, M, State), Possible_Moves),

	%% Translates a list of moves into a list of states that can be 
	%% reached by them.
	next_states(State, Possible_Moves, Possible_States),

	%% Prunes out those states that are not yet part of our data base.
	findall(Unknown_State, 
		(member(Unknown_State, Possible_States),
		not(knut_data(_, Unknown_State, _, _))
		),
		Possible_States_NOT_In_Data_Base),

	%% Choose a random unknown successor. If there aren't any 
	%% -> move on to DEEPENING.
	length(Possible_States_NOT_In_Data_Base, Length),
	ggp_db(1,'monte_carlo SIMULATION Length of unknown SuccessorStates':Length),

	Length > 0, !,
	N is random(Length),
	nth0(N, Possible_States_NOT_In_Data_Base, Expansion),



	%% SIMULATION

	%% Trace down the tree until a goal state is found. 
	%% Back-propagate the result.
	dfs(Role, Expansion, Won),
	ggp_db(3,'Monte Carlo SIMULATION done':Won),



	%% Adds the new expansion to the data base.
	sort(Expansion, SortedExpansion),	
	term_hash(SortedExpansion, HashKey),
	assert(knut_data(HashKey, Expansion, 1, Won)),
	ggp_db(1,'monte_carlo asserts: ':1:Won:Expansion).

%% 
%% SELECTION & BACK-PROPAGATION
%% 
%% Recursive call.
%% If all the successor states of the given state are known to the data base
%% monte carlo uses an evaluation function in order to select the node from
%% which it wants to proceed with the search.
%% The Upper Confidence Bounds applied to Trees (UCT) algorithm provides a
%% good balance between nodes that were found to be good and nodes that need
%% to be explored further.
%% 
%% After that the algorithm has been called recursively to deepen itself into
%% the search tree, it back-propagates the results determined by the simulation
%% of the recursion anchoring case.
%% All the parent nodes of the expansion are then updated in the data base and 
%% incremented in their visits.
%% 
%% monte_carlo(+Role, +State, -Expansion)
%% 
monte_carlo(Role, State, Expansion) :-

	%% SELECTION

	%% Uses the UCT-function to evaluate the state from which to move on.
	choose_most_interesting_state(Role, State, Most_Interesting_State),

	%% The role being on turn alternates with depth of the search tree.
	opponent(Role, Opponent),

	%% Deepens the search to the next level.
	monte_carlo(Opponent, Most_Interesting_State, Expansion),



	%% BACK-PROPAGATION

	%% Calculates new values in order to update the data base with newly
	%% derived information.
	knut_data(HashKey, Most_Interesting_State, Visits, Wins),
	retractall(knut_data(HashKey, Most_Interesting_State, Visits, Wins)),

	%% Calculate values to update 
	knut_data(_, Expansion, _, Won),
	New_Visits is Visits + 1,
	New_Wins is Wins + Won,

	%% Update to new values
	assert(knut_data(HashKey, Most_Interesting_State, New_Visits, New_Wins)), 
	ggp_db(1,'monte_carlo updates from':Visits:Wins:'to':New_Visits:New_Wins),
	!.



%%  __   _   _      _     _      __   _____  _   ___   _     
%% ( (` | | | |\/| | | | | |    / /\   | |  | | / / \ | |\ | 
%% _)_) |_| |_|  | \_\_/ |_|__ /_/--\  |_|  |_| \_\_/ |_| \| 

%% 
%% Depth first search. 
%% 
%% Simulates random plays until a terminal node is reached. The utility of the
%% goal states maps to different values that are used in the UCT-function.
%% 	  Win 	:=  1
%% 	  Draw	:=  0
%%    Loss 	:= -1
%% 
%% Finally it back-propagates the result.
%% 
%% 
%% 
%% Base case. 
%% A terminal state has been reached.
%% 
%% dfs(+Role, +State, -Value)
%% 
dfs(Role, State, Value) :-

	%% Checks for terminal state.
	gdl_terminal(State), !,

	%% Gets the role we received for this game from the data base.
	role_data(W_Harry),

	%% Determines the utility and maps it to a value.
	gdl_goal(W_Harry, Utility, State),
	ggp_db(3, 'State':State:'Role':Role:'Utility':Utility),
	(Utility = 0 -> Value is -1 ; Utility = 50 -> Value is 0 ; Value is 1),
	ggp_db(2, 'Utility: ':Utility).

%% 
%% Recursive call.
%% Chooses random successor and expands the next tree level.
%% 
%% dfs(+Role, +State, -Value)
%% 
dfs(Role, State, Value) :-
	
	%% The role being on turn alternates with depth of the search tree.
	opponent(Role, Opponent),

	%% Determines legal moves from that state on and selects a random one.
	findall(Opponent:M, gdl_legal(Opponent, M, State), Possible_Moves),
	length(Possible_Moves, Length),
	(Length > 0 ->
		N is random(Length),
		nth0(N, Possible_Moves, Rand_Move),

		%% Translates the chosen move into a succeeding state and calls
		%% dfs recursively upon the next tree level.
		ggp_next_state(State, [Rand_Move], Rand_State),
		dfs(Opponent, Rand_State, Value)
		;
		ggp_db(3, 'dfs No SuccessorStates')
	),
	ggp_db(3, 'dfs Value ':Value).



%%  __  _____   __   _____  ____      
%% ( (`  | |   / /\   | |  | |_      
%% _)_)  |_|  /_/--\  |_|  |_|__     
%%  __   ____  _     ____  __   _____  _   ___   _     
%% ( (` | |_  | |   | |_  / /`   | |  | | / / \ | |\ | 
%% _)_) |_|__ |_|__ |_|__ \_\_,  |_|  |_| \_\_/ |_| \| 

%% 
%% Chooses the state with the best wins per visits ratio. This state will be
%% updated into our data base in order to do the corresponding move after the 
%% time has run out.
%% 
%% choose_best_state(+Role, +State, -Best_State)
%% 
choose_best_state(Role, State, Best_State) :-

	%% Determines the succeeding moves and translates them into states.
	findall(Role:M, gdl_legal(Role, M, State), Possible_Moves),

	next_states(State, Possible_Moves, Possible_States),

	%% For comparison the states have to be known to the data base.
	findall(Known_State, (
		knut_data(_, Known_State, _, _),
		member(Known_State, Possible_States)
		), Possible_States_In_Data_Base),

	%% Extracts an initial element from the list of possible states to start the
	%% comparison with.
	member(First, Possible_States_In_Data_Base),

	knut_data(_, First, Visits, Wins),

	Ratio is Wins / Visits,

	%% Determines the state with the highest ratio.
	find_max(Possible_States_In_Data_Base, Ratio, First, Best_State).



%% 
%% Chooses the most interesting state corresponding to our UCT-function. The
%% winning state will be chosen to deepen the monte carlo search.
%% 
%% Upper Confidence Bounds applied to Trees (UCT) algorithm.
%% The UCT-function maps 
%% 	 - the amount of visits to a node
%% 	 - the amount of visits to its parent node
%% 	 - the amount of winning outcomes starting the simulation from this node
%% to a UCT value.
%% 
%% The UCT can be decomposed into the two functions of exploitation and 
%% exploration. 
%% The former favours nodes that seem to be a good choice to play
%% (i.e. have a good chance for a winning outcome). This value is calculated 
%% by the ratio of wins and visits of the desired node.
%% The latter represents the curious part of the algorithm that favours nodes
%% which need to be explored further (i.e. have not been visited enough). This
%% enables it to earn increasing confidence about the actual usefulness a
%% certain node.
%% 
%% By adapting the weights of these two functions we are able to narrow the 
%% search on a continuous scale.
%% 
%% See our documentation for a detailed explanation of the UCT-function.
%% 
%% choose_most_interesting_state(+Role, +State, -Most_Interesting_State)
%% 
choose_most_interesting_state(Role, State, Most_Interesting_State) :-
	
	%% Determines the succeeding moves and translates them into states.
	findall(Role:M, gdl_legal(Role, M, State), Possible_Moves),

	next_states(State, Possible_Moves, Possible_States),

	length(Possible_States, L),
	ggp_db(1, 'choose_most_interesting_state Possible_States':L:Possible_States),

	%% For comparison the states have to be known to the data base.
	findall(Known_State, (
		member(Known_State, Possible_States),
		knut_data(_, Known_State, _, _)
		), Possible_States_In_Data_Base),

	length(Possible_States_In_Data_Base, Lengthposs),
	ggp_db(1, 'choose_most_interesting_state Length Possible_States_In_Data_Base':
		Lengthposs:Possible_States_In_Data_Base),

	%% An initial element is extracted from the list of possible states to
	%% start the comparison with.
	member(First, Possible_States_In_Data_Base),
	
	%% Reads initial arguments for the UCT value from the data base.
	knut_data(_, First, Visits_First, Wins_First),
    knut_data(_, State, Visits_Parent, _Wins_Parent),

    %% Calculates the UCT value of the initial state with the current parameter
    %% written to our data base. 
	parameter_UCT_data(Lambda),
	Exploitation is Wins_First / Visits_First,
	Exploration is Lambda * sqrt(log(Visits_Parent) / Visits_First),
	UCT is Exploitation + Exploration,

	%% Determines the state with the highest UCT value.
	find_max_UCT(Visits_Parent, Possible_States_In_Data_Base, 
		UCT, First, Most_Interesting_State),
	ggp_db(2, 'choose_most_interesting_state ends').



%% 
%% Finds the state with the maximum wins per visits ratio.
%% 
%% Base case.
%% The list of states to compare is empty.
%% 
%% find_max(+States, +Comparison_Ratio, +Initial_State, -Best_State)
%% 
find_max([], _Max, Best_State, Best_State) :- !.

%% 
%% Recursive call.
%% No new max found.
%% The head of the input list is not greater than our current maximum. Checks
%% the rest of the list with the same current max.
%% 
find_max([State | R], Current_Max, _Current_Best_State, Best_State) :-
	knut_data(_, State, Visits, Wins),
	Ratio is Wins / Visits,
	Ratio =< Current_Max,
	find_max(R, Ratio, State, Best_State).

%% 
%% Recursive call.
%% New max found.
%% The head of the input list is greater than our current maximum. Checks the
%% rest of the list with the new maximum.
%% 
find_max([_State | R], Current_Max, Current_Best_State, Best_State) :-
	find_max(R, Current_Max, Current_Best_State, Best_State).



%% 
%% Finds the most interesting state according to the best values of exploration
%% and exploitation
%% 
%% Base case.
%% The list of states to compare is empty.
%% 
%% find_max_UCT(+Visits_Parent, +States, +Max_UCT, +Current_Best_State, -Best_State)
%% 
find_max_UCT(_Visits_Parent, [], _Max_UCT, Best_State, Best_State) :- 
	ggp_db(3, 'find_max_UCT -1-'), !.

%% 
%% Recursive call.
%% No new max found.
%% The head of the input list is not greater than our current maximum. Checks
%% the rest of the list with the same current max.
%% 
find_max_UCT(VisitsParent, [State | R], Current_Max_UCT, _Current_Best_State, Best_State) :-
	knut_data(_, State, Visits, Wins),
	parameter_UCT_data(Lambda),
	Exploitation is Wins / Visits,
	Exploration is Lambda * sqrt(log(VisitsParent) / Visits),
	UCT is Exploitation + Exploration, 
	UCT =< Current_Max_UCT,
	find_max_UCT(VisitsParent, R, UCT, State, Best_State),
	length([State | R], LengthInput),
	ggp_db(3, 'find_max_UCT -2- Length Input Possible_States':LengthInput).

%% 
%% Recursive call.
%% New max found.
%% The head of the input list is greater than our current maximum. Checks the
%% rest of the list with the new maximum.
%% 
find_max_UCT(VisitsParent, [_State | R], Current_Max, Current_Best_State, Best_State) :-
	find_max_UCT(VisitsParent, R, Current_Max, Current_Best_State, Best_State),
	ggp_db(3, 'find_max_UCT -3-':Current_Max).



%%  _     ____  _     ___   ____  ___   __  
%% | |_| | |_  | |   | |_) | |_  | |_) ( (` 
%% |_| | |_|__ |_|__ |_|   |_|__ |_| \ _)_) 

%% 
%% Translates a list of moves into a list of succeeding states.
%% 
%% Base case.
%% Both lists are empty.
%% 
%% next_states(+Current_State, +Moves, ?Successor_States)
%% 
next_states(_Current_State, [], []) :- !.

%% 
%% Recursive call.
%% Translates the first element, moves down the rest of the lists.
%% 
next_states(Current_State, [Move | Rest_Moves], [State | Rest_States]) :-
	ggp_next_state(Current_State, [Move], State),
	next_states(Current_State, Rest_Moves, Rest_States).



%% 
%% Switches the roles in a two-player game
%% 
%% opponent(+Player, -Opponent)
%% 
opponent(Player, Opponent) :-
	findall(Role, gdl_role(Role), Roles),
	length(Roles,L), 
	L =< 2,
	gdl_role(Opponent), 
	Player \= Opponent.



%% 
%% A custom time predicate that renders true if nine tenth of the time
%% is elapsed.
%% 
time_9_10 :- 
	get_time(T), 
	st_time(TS), 
	my_time(DT),  
	T > TS +  DT * 9 / 10.


% KNUT©
