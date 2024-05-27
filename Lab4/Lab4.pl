% Предикат для розділення числової послідовності на інтервали
split_into_intervals(X, N, Intervals) :-
    sort(X, Sorted),
    min_list(Sorted, Min),
    max_list(Sorted, Max),
    IntervalSize is (Max - Min) / N,
    findall([MinI, MaxI], (
        between(0, N, I),
        MinI is Min + I * IntervalSize,
        MaxI is Min + (I + 1) * IntervalSize
    ), Intervals).

% Предикат для визначення індексу інтервалу, до якого належить число
assign_interval(X, Intervals, Index) :-
    nth0(Index, Intervals, [Min, Max]),
    X >= Min,
    X =< Max.

% Предикат для перетворення числової послідовності у лінгвістичну
to_linguistic([], _, []).
to_linguistic([X|Xs], Alphabet, [Symbol|Symbols]) :-
    assign_interval(X, Alphabet, Index),
    (Index =:= -1 -> Symbol = '?'; nth0(Index, Alphabet, Symbol)),
    to_linguistic(Xs, Alphabet, Symbols).

% Предикат для побудови матриці передування
build_precedence_matrix(Linguistic, Alphabet, PrecedenceMatrix) :-
    length(Alphabet, N),
    empty_matrix(N, N, EmptyMatrix),
    build_precedence_matrix(Linguistic, Alphabet, EmptyMatrix, PrecedenceMatrix).

build_precedence_matrix([], _, Matrix, Matrix).
build_precedence_matrix([_], _, Matrix, Matrix).
build_precedence_matrix([Linguistic1, Linguistic2|Rest], Alphabet, AccMatrix, ResultMatrix) :-
    nth0(Index1, Alphabet, Linguistic1),
    nth0(Index2, Alphabet, Linguistic2),
    nth0(Index1, AccMatrix, Row),
    nth0(Index2, Row, Value),
    NewValue is Value + 1,
    replace_in_matrix(AccMatrix, Index1, Index2, NewValue, NewMatrix),
    build_precedence_matrix([Linguistic2|Rest], Alphabet, NewMatrix, ResultMatrix).

% Допоміжні предикати для побудови матриці передування
empty_matrix(N, M, Matrix) :-
    length(Row, M),
    length(Matrix, N),
    maplist(=(Row), Matrix).

replace_in_list([_|T], 0, X, [X|T]).
replace_in_list([H|T], I, X, [H|R]) :-
    I > -1,
    NI is I - 1,
    replace_in_list(T, NI, X, R).

replace_in_matrix([H|T], 0, J, X, [NewH|T]) :-
    replace_in_list(H, J, X, NewH).
replace_in_matrix([H|T], I, J, X, [H|NewT]) :-
    I > -1,
    NI is I - 1,
    replace_in_matrix(T, NI, J, X, NewT).

% Предикат для читання числової послідовності з файлу
read_series(File, Series) :-
    open(File, read, Stream),
    read(Stream, Series),
    close(Stream).

% Предикат для читання параметрів з файлу
read_params(File, AlphabetSize, Alphabet) :-
    open(File, read, Stream),
    read(Stream, AlphabetSize),
    read_line_to_codes(Stream, Line),
    atom_codes(AtomLine, Line),
    atomic_list_concat(Alphabet, ' ', AtomLine),
    close(Stream).

% Предикат для запису результатів у консоль та у файл
write_results(OutputPath, Alphabet, Linguistic, PrecedenceMatrix) :-
    format("Linguistic series: ~w\n", [Linguistic]),
    format("Precedence matrix:\n", []),
    write_precedence_matrix(Alphabet, PrecedenceMatrix),
    tell(OutputPath),
    format("Linguistic series: ~w\n", [Linguistic]),
    format("Precedence matrix:\n", []),
    write_precedence_matrix(Alphabet, PrecedenceMatrix),
    told.

write_precedence_matrix([], _).
write_precedence_matrix([Row|Rows], [Row|Matrix]) :-
    format("~w\n", [Row]),
    write_precedence_matrix(Rows, Matrix).

% Предикат для процесу повної обробки числової послідовності
process_series(SeriesFile, ParamsFile, OutputPath) :-
    read_series(SeriesFile, Series),
    read_params(ParamsFile, AlphabetSize, Alphabet),
    split_into_intervals(Series, AlphabetSize, Intervals),
    to_linguistic(Series, Alphabet, Linguistic),
    build_precedence_matrix(Linguistic, Alphabet, PrecedenceMatrix),
    write_results(OutputPath, Alphabet, Linguistic, PrecedenceMatrix).

% Головний предикат
main :-
    statistics(walltime, [_ | [_]]),
    process_series('number series.txt', 'params.txt', 'output.txt'),
    statistics(walltime, [_ | [ExecutionTime]]),
    format('Data written to output.txt. Duration: ~3f seconds', [ExecutionTime / 1000]).

% Виклик головного предикату
:- main.